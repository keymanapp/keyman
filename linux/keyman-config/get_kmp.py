#!/usr/bin/python3

import sys
import datetime
import time
import json
import requests
import requests_cache
import os

def get_keyboard_data(keyboardid, verbose=False):
	"""
	Get Keyboard data from web api.

	Args:
		keyboardid (str): Keyboard ID
		verbose(bool, default False): verbose output
	Returns:
		dict: Keyboard data
	"""
	if verbose:
		print("Getting data for keyboard", keyboardid)
	api_url = "https://api.keyman.com/keyboard/" + keyboardid
	if verbose:
		print("At URL", api_url)
	cache_dir = "~/.local/share/keyman"
	current_dir = os.getcwd()
	expire_after = datetime.timedelta(days=1)
	if not os.path.isdir(cache_dir):
		os.makedirs(cache_dir)
	os.chdir(cache_dir)
	requests_cache.install_cache(cache_name='keyman_cache', backend='sqlite', expire_after=expire_after)
	now = time.ctime(int(time.time()))
	response = requests.get(api_url)
	if verbose:
		print("Time: {0} / Used Cache: {1}".format(now, response.from_cache))
	os.chdir(current_dir)
	requests_cache.core.uninstall_cache()
	if response.status_code == 200:
#		return json.loads(response.content.decode('utf-8'))
		return response.json()
	else:
		return None

def get_download_folder():
	"""
	Folder where downloaded files will be saved.

	Returns:
	    str: path of user downloads folder
	"""
	home = os.path.expanduser("~")
	return os.path.join(home, "Downloads")

def get_kmp_file(kbdata, verbose=False):
	"""
	Get info from keyboard data to download kmp then download it.

	Args:
		kbdata (dict): Keyboard data
		verbose (bool, default False): verbose output
	Returns:
		str: path where kmp file has been downloaded
	"""
	if 'packageFilename' not in kbdata:
		print("get_kmp.py: Keyboard does not have a kmp file available")
		return None

	kmp_url = "https://downloads.keyman.com/keyboards/" + kbdata['id'] + "/" + kbdata['version'] + "/" + kbdata['packageFilename']
	downloadfile = os.path.join(get_download_folder(), kbdata['packageFilename'])
	return download_kmp_file(kmp_url, downloadfile, verbose)

def download_kmp_file(url, kmpfile, verbose=False):
	"""
	Download kmp file.

	Args:
		url (str): URL to download the kmp file from.
		kmpfile (str): Where to save the kmp file.
		verbose (bool, default False): verbose output
	Returns:
		str: path where kmp file has been downloaded
	"""
	if verbose:
		print("Download URL:", url)
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
		print("get_kmp.py: Could not download information about keyboard.")
		return None
	return

def main(argv):
	if len(sys.argv) != 2:
		print("get_kmp.py <keyboard id>")
		sys.exit(2)
	keyboardid = sys.argv[1]
	get_kmp(keyboardid)

if __name__ == "__main__":
	main(sys.argv[1:])
