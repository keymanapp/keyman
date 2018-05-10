#!/usr/bin/python3

# Get a kmp file given a keyboard id

import sys
import datetime
import time
import json
import requests
import requests_cache
import os
#from tqdm import tqdm

def get_keyboard_data(keyboardid):
	print("Getting data for keyboard", keyboardid)
	api_url = "https://api.keyman.com/keyboard/" + keyboardid
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
	print("Time: {0} / Used Cache: {1}".format(now, response.from_cache))
	os.chdir(current_dir)
	requests_cache.core.uninstall_cache()
	if response.status_code == 200:
#		return json.loads(response.content.decode('utf-8'))
		return response.json()
	else:
		return None

def get_download_folder():
	home = os.path.expanduser("~")
	return os.path.join(home, "Downloads")

def get_kmp_file(kbdata):
	if 'packageFilename' not in kbdata:
		print("get_kmp.py: Keyboard does not have a kmp file available")
		return None
#	print("kmpfile:", kbdata['packageFilename'])
#	print("version:", kbdata['version'])

	kmp_url = "https://downloads.keyman.com/keyboards/" + kbdata['id'] + "/" + kbdata['version'] + "/" + kbdata['packageFilename']
	print("Download URL:", kmp_url)
	response = requests.get(kmp_url) #, stream=True)
	downloadfile = os.path.join(get_download_folder(), kbdata['packageFilename'])
	#with open(downloadfile , "wb") as handle:
	#	for data in tqdm(response.iter_content()):
	#		handle.write(data)
	print(len(response.content))
	with open(downloadfile, 'wb') as f:
		f.write(response.content)
	return downloadfile

def get_kmp(keyboardid):
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
