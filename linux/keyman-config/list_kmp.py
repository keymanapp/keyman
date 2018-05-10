#!/usr/bin/python3

import tempfile
from keymankeyboards import get_api_keyboards
from get_kmp import get_keyboard_data, get_kmp_file
from install_kmp import get_metadata

def main():
	keyboarddata = get_api_keyboards()
	if keyboarddata:
		with open('./nokmp.txt', 'wt') as nokmp, open('./nojson.txt', 'wt') as nojson, open('./nodata.txt', 'wt') as nodata, open('./goodkmp.txt', 'wt') as goodkmp:
			for kb in keyboarddata['keyboard']:
				kbdata = get_keyboard_data(kb['id'])
				print(kb['id'])
				if kbdata:
					if 'packageFilename' in kbdata:
						kmpfile = get_kmp_file(kbdata)
						with tempfile.TemporaryDirectory() as tmpdirname:
							info, system, options, keyboards, files = get_metadata(kmpfile, tmpdirname)
							if keyboards:
								print("Keyboard:", kb['id'], "has kmp", kbdata['packageFilename'], "with kmp.json", file=goodkmp)
							else:
								print("Keyboard:", kb['id'], "has kmp", kbdata['packageFilename'], "but no kmp.json", file=nojson)
					else:
						print("Keyboard:", kb['id'], "does not have kmp", file=nokmp)
				else:
					print("Keyboard:", kb['id'], "has no data", file=nodata)


if __name__ == "__main__":
	main()
