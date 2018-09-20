#!/usr/bin/python3

# Read in local keyboards json file

# Later check if online
# If so then get it from web api
# Is there a way to check if has changed from cached version and only download if it has?

# Smaller than languages file (~60% of size) so more network efficent
# but needs a little more processing
# using gzip makes network transfer much less

# for each keyboard
# for each language in keyboard
# add language to a region if it doesn't already exist
# add keyboard to that language if it isn't already on it

import datetime
import time
import json
import requests
import requests_cache
import os
from pathlib import Path

def get_api_keyboards(verbose=False):
	"""
	Get Keyboards data from web api.

	Args:
		verbose(bool, default False): verbose output
	Returns:
		dict: Keyboard data
		None: if http request not successful
	"""
	api_url = "https://api.keyman.com/cloud/4.0/keyboards?version=10.0"
	headers = {'Content-Type': 'application/json',
		'Accept-Encoding': 'gzip, deflate, br'}
	home = str(Path.home())
	cache_dir = os.path.join(home, ".local/share/keyman")
	current_dir = os.getcwd()
	expire_after = datetime.timedelta(days=1)
	if not os.path.isdir(cache_dir):
		os.makedirs(cache_dir)
	os.chdir(cache_dir)
	requests_cache.install_cache(cache_name='keyman_cache', backend='sqlite', expire_after=expire_after)
	now = time.ctime(int(time.time()))
	response = requests.get(api_url, headers=headers)
	if verbose:
		print("Time: {0} / Used Cache: {1}".format(now, response.from_cache))
	os.chdir(current_dir)
	if response.status_code == 200:
#		return json.loads(response.content.decode('utf-8'))
		return response.json()
	else:
		return None

def parse_keyboard(data, verbose=False):
	regions = { 1 : {"name" : "Undefined", "languages" : {} }, 2 : {"name" : "Africa", "languages" : {} }, 3 : {"name" : "Asia", "languages" : {} }, 4 :  {"name" : "Europe", "languages" : {} }, 5 :  {"name" : "Unused", "languages" : {} }, 6 :  {"name" : "Americas", "languages" : {} }, 7 : {"name" : "Asia Pacific", "languages" : {} } }
	options = data['options']
	for kb in data['keyboard']:
		#print(kb['id'])
		for lang in kb['languages']:
			#print(" ", lang['id'], lang['region'], regions[lang['region']]['name'])
			if not lang['name'] in regions[lang['region']]['languages']:
				regions[lang['region']]['languages'][lang['name']] = lang
			if not 'keyboards' in regions[lang['region']]['languages'][lang['name']]:
				regions[lang['region']]['languages'][lang['name']]['keyboards'] = {}
			if not kb['id'] in regions[lang['region']]['languages'][lang['name']]['keyboards']:
				regions[lang['region']]['languages'][lang['name']]['keyboards'][kb['id']] = kb
	if verbose:
		for region in regions:
			print("--- Region", str(region), regions[region]['name'], "---")
			for langname in sorted(regions[region]['languages']):
				print(langname)
				for kbname in sorted(regions[region]['languages'][langname]['keyboards']):
					print("  Keyboard:", regions[region]['languages'][langname]['keyboards'][kbname]['name'])

def main():
	data = get_api_keyboards()
	if data:
		parse_keyboard(data)
	else:
		print("Failed to get data, using local file")
		with open("keyboards", "r") as read_file:
			localdata = json.load(read_file)
			parse_keyboard(localdata)


if __name__ == "__main__":
	main()
