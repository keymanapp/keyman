#!/usr/bin/python3

# Read in local keyboards json file

# Later check if online
# If so then get it from web api
# Is there a way to check if has changed from cached version and only download if it has?

# Smaller than languages file (~60% of size) so more network efficent
# but needs a little more processing

# for each keyboard
# for each language in keyboard
# add language to a region if it doesn't already exist
# add keyboard to that language if it isn't already on it

import json


def main():
	regions = { 1 : {"name" : "Undefined", "languages" : {} }, 2 : {"name" : "Africa", "languages" : {} }, 3 : {"name" : "Asia", "languages" : {} }, 4 :  {"name" : "Europe", "languages" : {} }, 5 :  {"name" : "Unused", "languages" : {} }, 6 :  {"name" : "Americas", "languages" : {} }, 7 : {"name" : "Asia Pacific", "languages" : {} } }
	with open("keyboards", "r") as read_file:
		data = json.load(read_file)
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
		for region in regions:
			print("--- Region", str(region), regions[region]['name'], "---")
			for langname in sorted(regions[region]['languages']):
				print(langname)
				for kbname in sorted(regions[region]['languages'][langname]['keyboards']):
					print("  Keyboard:", kbname)

if __name__ == "__main__":
	main()
