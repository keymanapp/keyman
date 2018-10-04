#!/usr/bin/python3

import datetime
import logging
import os
import requests
import requests_cache
import subprocess
import tempfile
import time
from dirlist import list_keyboards
#from keymankeyboards import get_api_keyboards

#from keyman_config import get_kmp, install_kmp
from keyman_config.get_kmp import get_keyboard_data, get_kmp_file, keyman_cache_dir
from keyman_config.install_kmp import get_metadata, get_infdata, extract_kmp

#TODO check for kmn and check if it is compilable
#TODO extra output files jsonkmpnokmn, jsonkmpbadkmn, goodjsonkmpkmn and for inf as well

def get_kmn(kbid, sourcePath):
	base_url = "https://raw.github.com/keymanapp/keyboards/master/" + sourcePath
	kmn_url = base_url + "/source/" + kbid + ".kmn"

	cache_dir = keyman_cache_dir()
	current_dir = os.getcwd()
	expire_after = datetime.timedelta(days=7)
	if not os.path.isdir(cache_dir):
		os.makedirs(cache_dir)
	os.chdir(cache_dir)
	requests_cache.install_cache(cache_name='keyman_cache', backend='sqlite', expire_after=expire_after)
	now = time.ctime(int(time.time()))
	response = requests.get(kmn_url)
	logging.debug("Time: {0} / Used Cache: {1}".format(now, response.from_cache))
	os.chdir(current_dir)
	requests_cache.core.uninstall_cache()

	return requests.get(kmn_url)	

def main():
	logging.basicConfig(level=logging.DEBUG, format='%(levelname)s:%(message)s')
	keyboarddata = list_keyboards()
	if keyboarddata:
		with open('./nokmp.txt', 'wt') as nokmp, \
			open('./infnokeyboard.txt', 'wt') as infnokeyboard, \
			open('./goodjsonkmpkmn.txt', 'wt') as goodjsonkmpkmn, \
			open('./jsonkmpnokmn.txt', 'wt') as jsonkmpnokmn, \
			open('./jsonkmpbadkmn.txt', 'wt') as jsonkmpbadkmn, \
			open('./jsonkmpmissingkmn.txt', 'wt') as jsonkmpmissingkmn, \
			open('./brokeninf.txt', 'wt') as brokeninf, \
			open('./nodata.txt', 'wt') as nodata, \
			open('./goodinfkmp.txt', 'wt') as goodinfkmp:

			print("Keyboard: will work in kmfl :)", file=goodjsonkmpkmn) # goodjsonkmpkmn
			print("Keyboard: has uncompilable kmn", file=jsonkmpbadkmn) # jsonkmpbadkmn
			print("Keyboard: has json in kmp but can't find the kmn on github", file=jsonkmpmissingkmn) # jsonkmpmissingkmn
			print("Keyboard: has json in kmp but has no sourcePath to look for kmn on github", file=jsonkmpnokmn) # jsonkmpnokmn
			print("Keyboard: has kmp with kmp.inf", file=goodinfkmp)
			print("Keyboard: has kmp with kmp.inf but it has no Keyboard", file=infnokeyboard)
			print("Keyboard: has kmp but no kmp.json and no or broken kmp.inf", file=brokeninf)
			print("Keyboard: does not have kmp so mobile/web only", file=nokmp)
			print("Keyboard: has no data", file=nodata)


			for kbid in keyboarddata:
				kbdata = get_keyboard_data(kbid, True)
				print(kbid)
				if kbdata:
					if 'packageFilename' in kbdata:
						kmpfile = get_kmp_file(kbdata, True)
						with tempfile.TemporaryDirectory() as tmpdirname:
							extract_kmp(kmpfile, tmpdirname)
							try:
								info, system, options, keyboards, files = get_metadata(tmpdirname)
								if keyboards:
									if 'sourcePath' in kbdata:
										response = get_kmn(kbid, kbdata['sourcePath'])
										if response.status_code == 200:
											kmndownloadfile = os.path.join(tmpdirname, kbid + ".kmn")
											with open(kmndownloadfile, 'wb') as f:
												f.write(response.content)
											subprocess.run(["kmflcomp", kmndownloadfile], stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
											kmfl_file = os.path.join(tmpdirname, kbid + ".kmfl")
											if os.path.isfile(kmfl_file):
												logging.debug("goodjsonkmpkmn")
												print(kbid, file=goodjsonkmpkmn) # goodjsonkmpkmn
											else:
												logging.debug("jsonkmpbadkmn")
												print(kbid, file=jsonkmpbadkmn) # jsonkmpbadkmn
										else:
											logging.debug("jsonkmpmissingkmn")
											print(kbid, file=jsonkmpmissingkmn) # jsonkmpmissingkmn
									else:
										logging.debug("jsonkmpnokmn")
										print(kbid, file=jsonkmpnokmn) # jsonkmpnokmn
								else:
									info, system, options, keyboards, files = get_infdata(tmpdirname)
									if keyboards:
										logging.debug("infnokeyboard")
										print(kbid, file=goodinfkmp)
									elif files:
										logging.debug("goodinfkmp")
										print(kbid, file=infnokeyboard)
									else:
										print(kbid, file=brokeninf)
							except KeyError:
								logging.debug("brokeninf")
								print(kbid, file=brokeninf)
					else:
						logging.debug("nokmp")
						print(kbid, file=nokmp)
				else:
					logging.debug("nodata")
					print(kbid, file=nodata)


if __name__ == "__main__":
	main()
