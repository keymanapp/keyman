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
# from ast import literal_eval
from enum import Enum

gi.require_version('IBus', '1.0')
from gi.repository import IBus

import requests

from keyman_config.get_kmp import get_keyboard_data, get_kmp, user_keyboard_dir, user_keyman_dir, user_keyman_font_dir
from keyman_config.kmpmetadata import parseinfdata, parsemetadata, infmetadata_to_json, KMFileTypes
from keyman_config.uninstall_kmp import uninstall_kmp
from keyman_config.convertico import checkandsaveico
from keyman_config.kvk2ldml import convert_kvk_to_ldml, output_ldml

#TODO userdir install
# special processing for kmn if needed
#TODO optionally standardise throughout on variable names
# packageID for kmps and keyboardID for keyboards
# see https://docs.google.com/document/d/1sj7W6pCiN-_iRss5iRdib1aHaSTmYoLIueQSKJeNy8Q/edit#heading=h.mq0rc28mf031

class InstallStatus(Enum):
	Continue = 0
	Warning = 1
	Abort = 2

class InstallError(Exception):
    """Exception raised for errors in KMP installation.

    Attributes:
        status -- InstallStatus for what to do when the error occurrs
        message -- explanation of the error
    """

    def __init__(self, status, message):
        self.status = status
        self.message = message

def list_files(directory, extension):
	return (f for f in listdir(directory) if f.endswith('.' + extension))

def extract_kmp(kmpfile, directory):
	with zipfile.ZipFile(kmpfile,"r") as zip_ref:
		zip_ref.extractall(directory)

def get_metadata(tmpdirname):
	"""
	Get metadata from kmp.json if it exists.
	If it does not exist then will return get_and_convert_infdata

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
		return get_and_convert_infdata(tmpdirname)

def get_and_convert_infdata(tmpdirname):
	"""
	Get metadata from kmp.inf if it exists.
	Convert it to kmp.json if possible

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
		j = infmetadata_to_json(info, system, options, keyboards, files)
		kmpjson = os.path.join(tmpdirname, "kmp.json")
		with open(kmpjson, "w") as write_file:
			print(j, file=write_file)
		return info, system, options, keyboards, files
	else:
		return None, None, None, None, None

def download_source(keyboardID, packageDir, sourcePath):
	# just get latest version of kmn unless there turns out to be a way to get the version of a file at a date
	base_url = "https://raw.github.com/keymanapp/keyboards/master/" + sourcePath
	kmn_url = base_url + "/source/" + keyboardID + ".kmn"
	response = requests.get(kmn_url)
	if response.status_code == 200:
		kmn_file = os.path.join(packageDir, keyboardID + ".kmn")
		with open(kmn_file, 'wb') as f:
			f.write(response.content)
			logging.info("Installing %s.kmn as keyman file", keyboardID)
		logging.info("Compiling kmn file")
		subprocess.run(["kmflcomp", kmn_file], stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
		# kmfl_file = os.path.join(kbdir, kbid + ".kmfl")
		# if not os.path.isfile(kmfl_file):
		# 	message = "Could not compile %s to %s so not installing keyboard." % (kmn_file, kmfl_file)
		# 	os.remove(kmn_file)
		# 	rmtree(kbdir)
		# 	raise InstallError(InstallStatus.Abort, message)
	# else:
	# 	message = "install_kmp.py: warning: no kmn source file for %s so not installing keyboard." % (kbid)
	# 	rmtree(kbdir)
	# 	raise InstallError(InstallStatus.Abort, message)
	icodownloadfile = os.path.join(packageDir, keyboardID + ".ico")
	if not os.path.isfile(icodownloadfile):
		ico_url = base_url + "/source/" + keyboardID + ".ico"
		response = requests.get(ico_url)
		if response.status_code == 200:
			with open(icodownloadfile, 'wb') as f:
				f.write(response.content)
			logging.info("Installing %s.ico as keyman file", keyboardID)
			checkandsaveico(icodownloadfile)
		else:
			logging.warning("install_kmp.py: warning: no ico source file for %s", keyboardID)

def process_keyboard_data(keyboardID, packageDir):
	kbdata = get_keyboard_data(keyboardID)
	if kbdata:
		if not os.path.isdir(packageDir):
			os.makedirs(packageDir)
		if 'sourcePath' in kbdata:
			download_source(keyboardID, packageDir, kbdata['sourcePath'])

		with open(os.path.join(packageDir, keyboardID + '.json'), 'w') as outfile:
			json.dump(kbdata, outfile)
			logging.info("Installing api data file %s.json as keyman file", keyboardID)
	# else:
	# 	message = "install_kmp.py: error: cannot download keyboard data so not installing."
	# 	rmtree(kbdir)
	# 	raise InstallError(InstallStatus.Abort, message)

def check_keyman_dir(basedir, error_message):
	# check if keyman subdir exists
	keyman_dir = os.path.join(basedir, "keyman")
	if os.path.isdir(keyman_dir):
		# Check for write access of keyman dir to be able to create subdir
		if not os.access(keyman_dir, os.X_OK | os.W_OK):
			raise InstallError(InstallStatus.Abort, error_message)
	else:
		# Check for write access of basedir and create keyman subdir if we can
		if not os.access(basedir, os.X_OK | os.W_OK):
			raise InstallError(InstallStatus.Abort, error_message)
		os.mkdir(keyman_dir)

def install_kmp_shared(inputfile, online=False):
	"""
	Install a kmp file to /usr/local/share/keyman

	Args:
		inputfile (str): path to kmp file
		online(bool, default=False): whether to attempt to get a source kmn and ico for the keyboard
	"""
	check_keyman_dir('/usr/local/share', "You do not have permissions to install the keyboard files to the shared area /usr/local/share/keyman")
	check_keyman_dir('/usr/local/share/doc', "You do not have permissions to install the documentation to the shared documentation area /usr/local/share/doc/keyman")
	check_keyman_dir('/usr/local/share/fonts', "You do not have permissions to install the font files to the shared font area /usr/local/share/fonts")

	packageID, ext = os.path.splitext(os.path.basename(inputfile))
	packageDir = os.path.join('/usr/local/share/keyman', packageID)
	kmpdocdir = os.path.join('/usr/local/share/doc/keyman', packageID)
	kmpfontdir = os.path.join('/usr/local/share/fonts/keyman', packageID)
	if not os.path.isdir(packageDir):
		os.makedirs(packageDir)
	extract_kmp(inputfile, packageDir)
	info, system, options, keyboards, files = get_metadata(packageDir)

	if keyboards:
		logging.info("Installing %s", info['name']['description'])
		if online:
			process_keyboard_data(packageID, packageDir)
			if len(keyboards) > 1:
				for kb in keyboards:
					if kb['id'] != packageID:
						process_keyboard_data(kb['id'], packageDir)

		for f in files:
			fpath = os.path.join(packageDir, f['name'])
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
				ldmlfile = os.path.join(packageDir, name+".ldml")
				output_ldml(ldmlfile, ldml)
				# Special handling of icon to convert to PNG
			elif ftype == KMFileTypes.KM_ICON:
				logging.info("Converting %s to PNG and installing both as keyman files", f['name'])
				checkandsaveico(fpath)
		for kb in keyboards:
			# install all kmx for first lang not just packageID
			kmx_file = os.path.join(packageDir, kb['id'] + ".kmx")
			install_to_ibus(lang, kmx_file)
		restart_ibus()
	else:
		logging.error("install_kmp.py: error: No kmp.json or kmp.inf found in %s", inputfile)
		logging.info("Contents of %s:", inputfile)
		for o in os.listdir(packageDir):
			logging.info(o)
		rmtree(packageDir)
		message = "install_kmp.py: error: No kmp.json or kmp.inf found in %s" % (inputfile)
		raise InstallError(InstallStatus.Abort, message)

def install_kmp_user(inputfile, online=False):
	packageID, ext = os.path.splitext(os.path.basename(inputfile))
	packageDir=user_keyboard_dir(packageID)
	if not os.path.isdir(packageDir):
		os.makedirs(packageDir)

	extract_kmp(inputfile, packageDir)
	info, system, options, keyboards, files = get_metadata(packageDir)

	if keyboards:
		logging.info("Installing %s", info['name']['description'])
		if online:
			process_keyboard_data(packageID, packageDir)
			if len(keyboards) > 1:
				for kb in keyboards:
					if kb['id'] != packageID:
						process_keyboard_data(kb['id'], packageDir)

		for f in files:
			fpath = os.path.join(packageDir, f['name'])
			ftype = f['type']
			if ftype == KMFileTypes.KM_FONT:
				#Special handling of font to hard link it into font dir
				fontsdir = os.path.join(user_keyman_font_dir(), packageID)
				if not os.path.isdir(fontsdir):
					os.makedirs(fontsdir)
				os.link(fpath, os.path.join(fontsdir, f['name']))
				logging.info("Installing %s as font", f['name'])
			elif ftype == KMFileTypes.KM_OSK:
				# Special handling to convert kvk into LDML
				logging.info("Converting %s to LDML and installing both as as keyman file", f['name'])
				ldml = convert_kvk_to_ldml(fpath)
				name, ext = os.path.splitext(f['name'])
				ldmlfile = os.path.join(packageDir, name+".ldml")
				output_ldml(ldmlfile, ldml)
			elif ftype == KMFileTypes.KM_ICON:
				# Special handling of icon to convert to PNG
				logging.info("Converting %s to PNG and installing both as keyman files", f['name'])
				checkandsaveico(fpath)
			elif ftype == KMFileTypes.KM_SOURCE:
				#TODO for the moment just leave it for ibus-kmfl to ignore if it doesn't load
				pass

		for kb in keyboards:
			# install all kmx for first lang not just packageID
			kmx_file = os.path.join(packageDir, kb['id'] + ".kmx")
			install_to_ibus(lang, kmx_file)
		restart_ibus()

	else:
		logging.error("install_kmp.py: error: No kmp.json or kmp.inf found in %s", inputfile)
		logging.info("Contents of %s:", inputfile)
		for o in os.listdir(packageDir):
			logging.info(o)
		rmtree(packageDir)
		message = "install_kmp.py: error: No kmp.json or kmp.inf found in %s" % (inputfile)
		raise InstallError(InstallStatus.Abort, message)

def install_to_ibus(lang, kmx_file):
	try:
		keyboard_id = "%s:%s" % (lang, kmx_file)
		logging.debug("getting bus")
		bus = IBus.Bus()
		logging.debug("installing to ibus")
		ibus_settings = Gio.Settings.new("org.freedesktop.ibus.general")
		preload_engines = ibus_settings.get_strv("preload-engines")
		logging.debug(preload_engines)
		if keyboard_id not in preload_engines:
			preload_engines.append(keyboard_id)
		logging.debug(preload_engines)
		ibus_settings.set_strv("preload-engines", preload_engines)
		bus.preload_engines(preload_engines)
	except Exception as e:
		logging.debug("Failed to set up install %s to IBus", keyboard_id)
		logging.debug(e)

def restart_ibus():
	try:
		bus = IBus.Bus()
		logging.info("restarting IBus")
		bus.exit(True)
	except Exception as e:
		logging.debug("Failed to restart IBus")
		logging.debug(e)


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
