#!/usr/bin/python3

import sys
import datetime
import time
import json
import logging
import requests
import requests_cache
import os
from pathlib import Path

def get_keyboard_data(keyboardid):
	"""
	Get Keyboard data from web api.

	Args:
		keyboardid (str): Keyboard ID
	Returns:
		dict: Keyboard data
	"""
	logging.info("Getting data for keyboard %s", keyboardid)
	api_url = "https://api.keyman.com/keyboard/" + keyboardid
	logging.debug("At URL %s", api_url)
	home = str(Path.home())
	cache_dir = os.path.join(home, ".local/share/keyman")
	current_dir = os.getcwd()
	expire_after = datetime.timedelta(days=1)
	if not os.path.isdir(cache_dir):
		os.makedirs(cache_dir)
	os.chdir(cache_dir)
	requests_cache.install_cache(cache_name='keyman_cache', backend='sqlite', expire_after=expire_after)
	now = time.ctime(int(time.time()))
	response = requests.get(api_url)
	logging.debug("Time: {0} / Used Cache: {1}".format(now, response.from_cache))
	os.chdir(current_dir)
	requests_cache.core.uninstall_cache()
	if response.status_code == 200:
		return response.json()
	else:
		return None

def get_download_folder():
	"""
	Folder where downloaded files will be saved.

	Returns:
	    str: path of user keyman cache folder
	"""
	home = os.path.expanduser("~")
	cachebase = os.environ.get("XDG_CACHE_HOME", os.path.join(home, ".cache"))
	km_cache=os.path.join(cachebase, "keyman")
	if not os.path.isdir(km_cache):
		os.mkdir(km_cache)
	return km_cache

def get_kmp_file(kbdata):
	"""
	Get info from keyboard data to download kmp then download it.

	Args:
		kbdata (dict): Keyboard data
	Returns:
		str: path where kmp file has been downloaded
	"""
	if 'packageFilename' not in kbdata:
		logging.info("get_kmp.py: Keyboard does not have a kmp file available")
		return None

	kmp_url = "https://downloads.keyman.com/keyboards/" + kbdata['id'] + "/" + kbdata['version'] + "/" + kbdata['packageFilename']
	downloadfile = os.path.join(get_download_folder(), kbdata['packageFilename'])
	return download_kmp_file(kmp_url, downloadfile)

def download_kmp_file(url, kmpfile):
	"""
	Download kmp file.

	Args:
		url (str): URL to download the kmp file from.
		kmpfile (str): Where to save the kmp file.
			currently it does no checks on this location
			assumes that is in users keyman cache dir
	Returns:
		str: path where kmp file has been downloaded
	"""
	logging.info("Download URL: %s", url)
	downloadfile = None
	response = requests.get(url) #, stream=True)
	if response.status_code == 200:
		with open(kmpfile, 'wb') as f:
			f.write(response.content)
			downloadfile = kmpfile
	return downloadfile

def get_kmp(keyboardid):
	"""
	Download a kmp file given a keyboard id.

	Args:
		keyboardid (str): Keyboard ID
	Returns:
		str: path where kmp file has been downloaded
	"""
	kbdata = get_keyboard_data(keyboardid)
	if (kbdata):
		return get_kmp_file(kbdata)
	else:
		logging.warning("get_kmp.py: Could not download information about keyboard.")
		return None
	return