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
from keyman_config.install_kmp import get_metadata, extract_kmp
from keyman_config.kmpmetadata import KMFileTypes
from keyman_config.get_kmp import keyman_cache_dir

#TODO check if any files in files list in cached kmps are KM_UNKNOWN

def main():
	logging.basicConfig(level=logging.INFO, format='%(levelname)s:%(message)s')
	km_cache=keyman_cache_dir()
	logging.info("looking for KM_UNKNOWN files in cached kmps")
	keyboarddata = list_keyboards()
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
