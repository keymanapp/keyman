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

from keyman_config.get_kmp import get_keyboard_data, get_kmp, user_keyboard_dir, user_keyman_dir, user_keyman_font_dir
from keyman_config.kmpmetadata import parseinfdata, parsemetadata, KMFileTypes
from keyman_config.uninstall_kmp import uninstall_kmp
from keyman_config.convertico import checkandsaveico
from keyman_config.kvk2ldml import convert_kvk_to_ldml, output_ldml

#TODO userdir install
# special processing for kmn
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
		return info, system, options, keyboards, files
	else:
		return None, None, None, None, None

def download_source(kbid, kbdir, sourcePath):
	# just get latest version of kmn unless there turns out to be a way to get the version of a file at a date
	base_url = "https://raw.github.com/keymanapp/keyboards/master/" + sourcePath
	kmn_url = base_url + "/source/" + kbid + ".kmn"
	response = requests.get(kmn_url)
	if response.status_code == 200:
		kmn_file = os.path.join(kbdir, kbid + ".kmn")
		with open(kmn_file, 'wb') as f:
			f.write(response.content)
			logging.info("Installing %s.kmn as keyman file", kbid)
		logging.info("Compiling kmn file")
		subprocess.run(["kmflcomp", kmn_file], stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
		kmfl_file = os.path.join(kbdir, kbid + ".kmfl")
		if not os.path.isfile(kmfl_file):
			logging.warning("Could not compile %s to %s so not installing keyboard.", kmn_file, kmfl_file)
			os.remove(kmn_file)
			rmtree(kbdir)
			return 1
		do_install_to_ibus = True
	else:
		logging.warning("install_kmp.py: warning: no kmn source file for %s so not installing keyboard.", kbid)
		rmtree(kbdir)
		return 1
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
	return 0

def process_keyboard_data(kbid, kbdir):
	kbdata = get_keyboard_data(kbid)
	if kbdata:
		if not os.path.isdir(kbdir):
			os.makedirs(kbdir)
		if 'sourcePath' in kbdata:
			ret = download_source(kbid, kbdir, kbdata['sourcePath'])
			if ret > 0:
				return ret
		with open(os.path.join(kbdir, kbid + '.json'), 'w') as outfile:
			json.dump(kbdata, outfile)
			logging.info("Installing api data file %s.json as keyman file", kbid)
	else:
		logging.error("install_kmp.py: error: cannot download keyboard data so not installing.")
		rmtree(kbdir)
		return 1
	return 0

def check_keyman_dir(basedir, error_message):
	# check if keyman subdir exists
	keyman_dir = os.path.join(basedir, "keyman")
	if os.path.isdir(keyman_dir):
		# Check for write access of keyman dir to be able to create subdir
		if not os.access(keyman_dir, os.X_OK | os.W_OK):
			logging.error(error_message)
			exit(3)
	else:
		# Check for write access of basedir and create keyman subdir if we can
		if not os.access(basedir, os.X_OK | os.W_OK):
			logging.error(error_message)
			exit(3)
		os.mkdir(keyman_dir)

def install_kmp_shared(inputfile, online=False):
	"""
	Install a kmp file to /usr/local/share/keyman

	Args:
		inputfile (str): path to kmp file
		online(bool, default=False): whether to attempt to get a source kmn and ico for the keyboard
	"""
	do_install_to_ibus = False
	check_keyman_dir('/usr/local/share', "You do not have permissions to install the keyboard files to the shared area /usr/local/share/keyman")
	check_keyman_dir('/usr/local/share/doc', "You do not have permissions to install the documentation to the shared documentation area /usr/local/share/doc/keyman")
	check_keyman_dir('/usr/local/share/fonts', "You do not have permissions to install the font files to the shared font area /usr/local/share/fonts")

	kmpid, ext = os.path.splitext(os.path.basename(inputfile))
	kmpdir = os.path.join('/usr/local/share/keyman', kmpid)
	kmpdocdir = os.path.join('/usr/local/share/doc/keyman', kmpid)
	kmpfontdir = os.path.join('/usr/local/share/fonts/keyman', kmpid)
	if not os.path.isdir(kmpdir):
		os.makedirs(kmpdir)
	extract_kmp(inputfile, kmpdir)
	info, system, options, keyboards, files = get_metadata(kmpdir)

	if keyboards:
		logging.info("Installing %s", info['name']['description'])
		if online:
			ret = process_keyboard_data(kmpid, kmpdir)
			if ret > 0:
				return
			if len(keyboards) > 1:
				for kb in keyboards:
					if kb['id'] != kmpid:
						ret = process_keyboard_data(kb['id'], kmpdir)
						if ret > 0:
							return

		for f in files:
			fpath = os.path.join(kmpdir, f['name'])
			ftype = f['type']
			if ftype == KMFileTypes.KM_DOC or ftype == KMFileTypes.KM_IMAGE:
				#Special handling of doc and images to hard link them into doc dir
				logging.info("Installing %s as documentation", f['name'])
				if not os.path.isdir(kmpdocdir):
					os.makedirs(kmpdocdir)
				os.link(fpath, os.path.join(kmpdocdir, f['name']))
			elif ftype == KMFileTypes.KM_FONT:
				#Special handling of font to hard link it into font dir
				logging.info("Installing %s as font", f['name'])
				if not os.path.isdir(kmpfontdir):
					os.makedirs(kmpfontdir)
				os.link(fpath, os.path.join(kmpfontdir, f['name']))
			elif ftype == KMFileTypes.KM_SOURCE:
				#TODO for the moment just leave it for ibus-kmfl to ignore if it doesn't load
				logging.info("Installing %s as keyman file", f['name'])
			elif ftype == KMFileTypes.KM_OSK:
				# Special handling to convert kvk into LDML
				logging.info("Converting %s to LDML and installing both as as keyman file", f['name'])
				ldml = convert_kvk_to_ldml(fpath)
				name, ext = os.path.splitext(f['name'])
				ldmlfile = os.path.join(kmpdir, name+".ldml")
				output_ldml(ldmlfile, ldml)
				# Special handling of icon to convert to PNG
			elif ftype == KMFileTypes.KM_ICON:
				logging.info("Converting %s to PNG and installing both as keyman files", f['name'])
				checkandsaveico(fpath)
		if do_install_to_ibus:
			install_to_ibus(kmn_file)
	else:
		logging.error("install_kmp.py: error: No kmp.json or kmp.inf found in %s", inputfile)
		logging.info("Contents of %s:", inputfile)
		for o in os.listdir(kmpdir):
			logging.info(o)
		rmtree(kmpdir)

def install_kmp_user(inputfile, online=False):
	do_install_to_ibus = False
	kmpid, ext = os.path.splitext(os.path.basename(inputfile))
	kmpdir=user_keyboard_dir(kmpid)
	if not os.path.isdir(kmpdir):
		os.makedirs(kmpdir)

	extract_kmp(inputfile, kmpdir)
	info, system, options, keyboards, files = get_metadata(kmpdir)

	if keyboards:
		logging.info("Installing %s", info['name']['description'])
		if online:
			ret = process_keyboard_data(kmpid, kmpdir)
			if ret > 0:
				return
			if len(keyboards) > 1:
				for kb in keyboards:
					if kb['id'] != kmpid:
						ret = process_keyboard_data(kb['id'], kmpdir)
						if ret > 0:
							return

		for f in files:
			fpath = os.path.join(kmpdir, f['name'])
			ftype = f['type']
			if ftype == KMFileTypes.KM_FONT:
				#Special handling of font to hard link it into font dir
				fontsdir = os.path.join(user_keyman_font_dir(), kmpid)
				if not os.path.isdir(fontsdir):
					os.makedirs(fontsdir)
				os.link(fpath, os.path.join(fontsdir, f['name']))
				logging.info("Installing %s as font", f['name'])
			elif ftype == KMFileTypes.KM_OSK:
				# Special handling to convert kvk into LDML
				logging.info("Converting %s to LDML and installing both as as keyman file", f['name'])
				ldml = convert_kvk_to_ldml(fpath)
				name, ext = os.path.splitext(f['name'])
				ldmlfile = os.path.join(kmpdir, name+".ldml")
				output_ldml(ldmlfile, ldml)
			elif ftype == KMFileTypes.KM_ICON:
				# Special handling of icon to convert to PNG
				logging.info("Converting %s to PNG and installing both as keyman files", f['name'])
				checkandsaveico(fpath)
			elif ftype == KMFileTypes.KM_SOURCE:
				#TODO for the moment just leave it for ibus-kmfl to ignore if it doesn't load
				pass
		if do_install_to_ibus:
			install_to_ibus(kmn_file)
	else:
		logging.error("install_kmp.py: error: No kmp.json or kmp.inf found in %s", inputfile)
		logging.info("Contents of %s:", inputfile)
		for o in os.listdir(kmpdir):
			logging.info(o)
		rmtree(kmpdir)



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
