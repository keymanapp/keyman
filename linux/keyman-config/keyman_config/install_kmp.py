#!/usr/bin/python3

import argparse
import json
import logging
import os.path
import subprocess
import sys
import tempfile
import zipfile
from os import listdir, makedirs
from shutil import copy2, rmtree
from ast import literal_eval

import requests

from keyman_config.get_kmp import get_keyboard_data, get_kmp
from keyman_config.kmpmetadata import parseinfdata, parsemetadata, KMFileTypes
from keyman_config.uninstall_kmp import uninstall_kmp
from keyman_config.convertico import checkandsaveico
from keyman_config.kvk2ldml import convert_kvk_to_ldml, output_ldml

#TODO single function with different options for shareddir and userdir?
# or two separate functions called shared functions?
#TODO work out what to do about fonts in userdir install
# they must be in ~/.local/share/fonts (~lsf)
# but the kmp will be installed in ~/.local/share/keyman (~ls/k)
# obvious thing is to symlink fonts from ~ls/k to ~lsf/keyman
#  but different kmps can have the same font/font file
#  and may have different version
#  and what do you do with uninstalling 
#  How to manage that?
# ~lsf/keyman/<kbid> just like in /u/l/s/fonts/keyman - sorted

#TODO userdir install
# just unpack everything in ~ls/k/kbid
# special processing for font, kvk, ico, kmn
# what errors need abort and uninstall?

def list_files(directory, extension):
	return (f for f in listdir(directory) if f.endswith('.' + extension))

def extract_kmp(kmpfile, directory):
	with zipfile.ZipFile(kmpfile,"r") as zip_ref:
		zip_ref.extractall(directory)

def get_metadata(tmpdirname):
	"""
	Get metadata from kmp.json if it exists.
	If it does not exist then will return get_infdata

	Args:
		inputfile (str): path to kmp file
		tmpdirname(str): temp directory to extract kmp

	Returns:
		list[5]: info, system, options, keyboards, files
			see kmpmetadata.parsemetadata for details
	"""
	kmpjson = os.path.join(tmpdirname, "kmp.json")
	if os.path.isfile(kmpjson):
		return parsemetadata(kmpjson, False)
	else:
		return get_infdata(tmpdirname)

def get_infdata(tmpdirname):
	"""
	Get metadata from kmp.inf if it exists.

	Args:
		inputfile (str): path to kmp file
		tmpdirname(str): temp directory to extract kmp

	Returns:
		list[5]: info, system, options, keyboards, files
			see kmpmetadata.parseinfdata for details
	"""
	kmpinf = os.path.join(tmpdirname, "kmp.inf")
	if os.path.isfile(kmpinf):
		info, system, options, keyboards, files =  parseinfdata(kmpinf, False)
		if files and not keyboards:
			id = "unknown"
			for kbfile in files:
				if kbfile['type'] == KMFileTypes.KM_KVK:
					id = os.path.basename(os.path.splitext(kbfile['name'])[0])
			#inf file may not have keyboards so generate it if needed
			keyboards = [ { 'name' : info['name']['description'],
				'id' : id,
				'version' : info['version']['description'] } ]
		return info, system, options, keyboards, files
	else:
		return None, None, None, None, None

def install_kmp_shared(inputfile, online=False):
	"""
	Install a kmp file to /usr/local/share/keyman

	Args:
		inputfile (str): path to kmp file
		online(bool, default=False): whether to attempt to get a source kmn and ico for the keyboard
	"""
	do_install_to_ibus = False
	# create a temporary directory using the context manager
	with tempfile.TemporaryDirectory() as tmpdirname:
		logging.debug('created temporary directory %s', tmpdirname)
		extract_kmp(inputfile, tmpdirname)
		info, system, options, keyboards, files = get_metadata(tmpdirname)

		if keyboards:
			kbid = keyboards[0]['id']
			logging.info("Installing %s", info['name']['description'])
			if not os.access('/usr/local/share/keyman', os.X_OK | os.W_OK): # Check for write access of keyman dir
				logging.error("You do not have permissions to install the keyboard files. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
				exit(3)
			kbdir = os.path.join('/usr/local/share/keyman', kbid)
			if not os.access('/usr/local/share/doc/keyman', os.X_OK | os.W_OK): # Check for write access of keyman doc dir
				logging.error("You do not have permissions to install the documentation. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
				exit(3)
			kbdocdir = os.path.join('/usr/local/share/doc/keyman', kbid)
			if not os.access('/usr/local/share/fonts/keyman', os.X_OK | os.W_OK): # Check for write access of keyman fonts
				logging.error("You do not have permissions to install the font files. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
				exit(3)
			kbfontdir = os.path.join('/usr/local/share/fonts/keyman', kbid)

			if online:
				kbdata = get_keyboard_data(kbid)
				# just get latest version of kmn unless there turns out to be a way to get the version of a file at a date
				if kbdata:
					if not os.path.isdir(kbdir):
						os.makedirs(kbdir)
					if 'sourcePath' in kbdata:
						base_url = "https://raw.github.com/keymanapp/keyboards/master/" + kbdata['sourcePath']
						kmn_url = base_url + "/source/" + kbid + ".kmn"
						response = requests.get(kmn_url)
						if response.status_code == 200:
							kmndownloadfile = os.path.join(tmpdirname, kbid + ".kmn")
							with open(kmndownloadfile, 'wb') as f:
								f.write(response.content)
							logging.info("Installing %s.kmn as keyman file, minimum version: %s", kbid, kbdata['minKeymanVersion'])
							copy2(kmndownloadfile, kbdir)
							kmn_file = os.path.join(kbdir, kbid + ".kmn")
							logging.info("Compiling kmn file")
							subprocess.run(["kmflcomp", kmn_file], stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
							kmfl_file = os.path.join(kbdir, kbid + ".kmfl")
							if not os.path.isfile(kmfl_file):
								logging.warning("Could not compile %s to %s so not installing keyboard.", kmn_file, kmfl_file)
								os.remove(kmn_file)
								os.rmdir(kbdir)
								return
							do_install_to_ibus = True
						else:
							logging.warning("install_kmp.py: warning: no kmn source file for %s so not installing keyboard.", kbid)
							os.rmdir(kbdir)
							return
						icodownloadfile = os.path.join(tmpdirname, kbid + ".ico")
						if not os.path.isfile(icodownloadfile):
							ico_url = base_url + "/source/" + kbid + ".ico"
							response = requests.get(ico_url)
							if response.status_code == 200:
								with open(icodownloadfile, 'wb') as f:
									f.write(response.content)
								logging.info("Installing %s.ico as keyman file", kbid)
								copy2(icodownloadfile, kbdir)
								checkandsaveico(icodownloadfile)
								copy2(icodownloadfile+".png", kbdir)
							else:
								logging.warning("install_kmp.py: warning: no ico source file for %s", kbid)
					with open(os.path.join(kbdir, kbid + '.json'), 'w') as outfile:
						json.dump(kbdata, outfile)
						logging.info("Installing api data file %s.json as keyman file", kbid)
				else:
					logging.error("install_kmp.py: error: cannot download keyboard data so not installing.")
					return

			for f in files:
				fpath = os.path.join(tmpdirname, f['name'])
				ftype = f['type']
				if ftype == KMFileTypes.KM_DOC or ftype == KMFileTypes.KM_IMAGE:
					logging.info("Installing %s as documentation", f['name'])
					if not os.path.isdir(kbdocdir):
						os.makedirs(kbdocdir)
					copy2(fpath, kbdocdir)
				elif ftype == KMFileTypes.KM_FONT:
					logging.info("Installing %s as font", f['name'])
					if not os.path.isdir(kbfontdir):
						os.makedirs(kbfontdir)
					copy2(fpath, kbfontdir)
				elif ftype == KMFileTypes.KM_META or ftype == KMFileTypes.KM_SOURCE or ftype == KMFileTypes.KM_KMX:
					logging.info("Installing %s as keyman file", f['name'])
					if not os.path.isdir(kbdir):
						os.makedirs(kbdir)
					copy2(fpath, kbdir)
				elif ftype == KMFileTypes.KM_KVK:
					logging.info("Converting %s to LDML and installing both as as keyman file", f['name'])
					if not os.path.isdir(kbdir):
						os.makedirs(kbdir)
					copy2(fpath, kbdir)
					ldml = convert_kvk_to_ldml(fpath)
					name, ext = os.path.splitext(f['name'])
					ldmlfile = os.path.join(kbdir, name+".ldml")
					output_ldml(ldmlfile, ldml)
				elif ftype == KMFileTypes.KM_ICON:
					logging.info("Converting %s to PNG and installing both as keyman files", f['name'])
					if not os.path.isdir(kbdir):
						os.makedirs(kbdir)
					copy2(fpath, kbdir)
					checkandsaveico(fpath)
					copy2(fpath+".png", kbdir)
			if do_install_to_ibus:
				install_to_ibus(kmn_file)
		else:
			logging.error("install_kmp.py: error: No kmp.json or kmp.inf found in %s", inputfile)
			logging.info("Contents of %s:", inputfile)
			for o in os.listdir(tmpdirname):
				logging.info(o)

def install_kmp_user(inputfile, online=False):
	do_install_to_ibus = False
	keyboardid, ext = os.path.splitext(os.path.basename(inputfile))
	home = os.path.expanduser("~")
	datahome = os.environ.get("XDG_DATA_HOME", os.path.join(home, ".local", "share"))
	kbdir=os.path.join(datahome, "keyman", keyboardid)
	if not os.path.isdir(kbdir):
		os.makedirs(kbdir)

	extract_kmp(inputfile, kbdir)
	info, system, options, keyboards, files = get_metadata(kbdir)

	if keyboards:
		kbid = keyboards[0]['id']
		logging.info("Installing %s", info['name']['description'])
		if online:
			kbdata = get_keyboard_data(kbid)
			# just get latest version of kmn unless there turns out to be a way to get the version of a file at a date
			if kbdata:
				if 'sourcePath' in kbdata:
					base_url = "https://raw.github.com/keymanapp/keyboards/master/" + kbdata['sourcePath']
					kmn_url = base_url + "/source/" + kbid + ".kmn"
					response = requests.get(kmn_url)
					if response.status_code == 200:
						kmndownloadfile = os.path.join(kbdir, kbid + ".kmn")
						with open(kmndownloadfile, 'wb') as f:
							f.write(response.content)
						kmn_file = os.path.join(kbdir, kbid + ".kmn")
						logging.info("Compiling kmn file")
						subprocess.run(["kmflcomp", kmn_file], stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
						kmfl_file = os.path.join(kbdir, kbid + ".kmfl")
						if not os.path.isfile(kmfl_file):
							logging.warning("Could not compile %s to %s so not installing keyboard.", kmn_file, kmfl_file)
							os.remove(kmn_file)
							rmtree(kbdir)
							return
						do_install_to_ibus = True
					else:
						logging.warning("install_kmp.py: warning: no kmn source file for %s so not installing keyboard.", kbid)
						rmtree(kbdir)
						return
					icodownloadfile = os.path.join(kbdir, kbid + ".ico")
					if not os.path.isfile(icodownloadfile):
						ico_url = base_url + "/source/" + kbid + ".ico"
						response = requests.get(ico_url)
						if response.status_code == 200:
							with open(icodownloadfile, 'wb') as f:
								f.write(response.content)
							logging.info("Installing %s.ico as keyman file", kbid)
							checkandsaveico(icodownloadfile)
						else:
							logging.warning("install_kmp.py: warning: no ico source file for %s", kbid)
				with open(os.path.join(kbdir, kbid + '.json'), 'w') as outfile:
					json.dump(kbdata, outfile)
					logging.info("Installing api data file %s.json as keyman file", kbid)
			else:
				logging.error("install_kmp.py: error: cannot download keyboard data so not installing.")
				return

		for f in files:
			fpath = os.path.join(kbdir, f['name'])
			ftype = f['type']
			if ftype == KMFileTypes.KM_FONT:
				#TODO Special handling of font to link it into font dir
				logging.info("Installing %s as font", f['name'])
			elif ftype == KMFileTypes.KM_KVK:
				# Special handling to convert kvk into LDML
				logging.info("Converting %s to LDML and installing both as as keyman file", f['name'])
				ldml = convert_kvk_to_ldml(fpath)
				name, ext = os.path.splitext(f['name'])
				ldmlfile = os.path.join(kbdir, name+".ldml")
				output_ldml(ldmlfile, ldml)
			elif ftype == KMFileTypes.KM_ICON:
				# Special handling of icon to convert to PNG
				logging.info("Converting %s to PNG and installing both as keyman files", f['name'])
				checkandsaveico(fpath)
		if do_install_to_ibus:
			install_to_ibus(kmn_file)
	else:
		logging.error("install_kmp.py: error: No kmp.json or kmp.inf found in %s", inputfile)
		logging.info("Contents of %s:", inputfile)
		for o in os.listdir(tmpdirname):
			logging.info(o)



def install_to_ibus(kmn_file):
	if sys.version_info.major == 3 and sys.version_info.minor < 6:
		dconfreadresult = subprocess.run(["dconf", "read", "/desktop/ibus/general/preload-engines"],
			stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
		dconfread = dconfreadresult.stdout.decode("utf-8", "strict")
	else:
		dconfreadresult = subprocess.run(["dconf", "read", "/desktop/ibus/general/preload-engines"],
			stdout=subprocess.PIPE, stderr= subprocess.STDOUT, encoding="UTF8")
		dconfread = dconfreadresult.stdout
	if (dconfreadresult.returncode == 0) and dconfread:
		preload_engines = literal_eval(dconfread)
		preload_engines.append(kmn_file)
		logging.info("Installing %s into IBus", kmn_file)
		if sys.version_info.major == 3 and sys.version_info.minor < 6:
			dconfwriteresult = subprocess.run(["dconf", "write", "/desktop/ibus/general/preload-engines", str(preload_engines)],
				stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
		else:
			dconfwriteresult = subprocess.run(["dconf", "write", "/desktop/ibus/general/preload-engines", str(preload_engines)],
				stdout=subprocess.PIPE, stderr= subprocess.STDOUT, encoding="UTF8")



def install_kmp(inputfile, online=False, sharedarea=False):
	"""
	Install a kmp file

	Args:
		inputfile (str): path to kmp file
		online(bool, default=False): whether to attempt to get a source kmn and ico for the keyboard
		sharedarea(bool, default=False): whether install kmp to shared area or user directory
	"""
	if sharedarea:
		install_kmp_shared(inputfile, online)
	else:
		install_kmp_user(inputfile, online)
