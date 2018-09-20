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
#from keyman_config.get_kmp import get_keyboard_data, get_kmp_file
from keyman_config.install_kmp import get_metadata, extract_kmp
from keyman_config.kmpmetadata import KMFileTypes

#TODO check if any files in files list in cached kmps are KM_UNKNOWN

def main():
	logging.basicConfig(level=logging.DEBUG, format='%(levelname)s:%(message)s')
	keyboarddata = list_keyboards()
	home = os.path.expanduser("~")
	cachebase = os.environ.get("XDG_CACHE_HOME", os.path.join(home, ".cache"))
	km_cache=os.path.join(cachebase, "keyman")
	if keyboarddata:
		with open('./unknownfiles.txt', 'wt') as unknownfiles:
			print("files found in cached kmps of type KM_UNKNOWN", file=unknownfiles)
			for kbid in keyboarddata:
				kmpfile = os.path.join(km_cache, kbid+".kmp")
				if os.path.exists(kmpfile):
					with tempfile.TemporaryDirectory() as tmpdirname:
						extract_kmp(kmpfile, tmpdirname)
						try:
							info, system, options, keyboards, files = get_metadata(tmpdirname)
							if files:
								for kbfile in files:
									if kbfile['type'] == KMFileTypes.KM_UNKNOWN:
										print(kbfile['name'], file=unknownfiles)
						except Exception as e:
							print(type(e))    # the exception instance
							print(e.args)     # arguments stored in .args
							print(e)          # __str__ allows args to be printed directly,		pass
							pass


if __name__ == "__main__":
	main()
