#!/usr/bin/python3

import tempfile
from keymankeyboards import get_api_keyboards
from get_kmp import get_keyboard_data, get_kmp_file
from install_kmp import get_metadata, get_infdata, extract_kmp

def main():
	keyboarddata = get_api_keyboards()
	if keyboarddata:
		with open('./nokmp.txt', 'wt') as nokmp, \
			open('./infnokeyboard.txt', 'wt') as infnokeyboard, \
			open('./brokeninf.txt', 'wt') as brokeninf, \
			open('./nodata.txt', 'wt') as nodata, \
			open('./goodjsonkmp.txt', 'wt') as goodjsonkmp, \
			open('./goodinfkmp.txt', 'wt') as goodinfkmp:
			for kb in keyboarddata['keyboard']:
				kbdata = get_keyboard_data(kb['id'])
				print(kb['id'])
				if kbdata:
					if 'packageFilename' in kbdata:
						kmpfile = get_kmp_file(kbdata)
						with tempfile.TemporaryDirectory() as tmpdirname:
							extract_kmp(kmpfile, tmpdirname)
							info, system, options, keyboards, files = get_metadata(tmpdirname)
							if keyboards:
								print("Keyboard:", kb['id'], "has kmp", kbdata['packageFilename'], "with kmp.json", file=goodjsonkmp)
							else:
								info, system, options, keyboards, files = get_infdata(tmpdirname)
								if keyboards:
									print("Keyboard:", kb['id'], "has kmp", kbdata['packageFilename'], "with kmp.inf", file=goodinfkmp)
								elif files:
									print("Keyboard:", kb['id'], "has kmp", kbdata['packageFilename'], "with kmp.inf but it has no Keyboard", file=infnokeyboard)
								else:
									print("Keyboard:", kb['id'], "has kmp", kbdata['packageFilename'], "but no kmp.json and no or broken kmp.inf", file=brokeninf)
					else:
						print("Keyboard:", kb['id'], "does not have kmp", file=nokmp)
				else:
					print("Keyboard:", kb['id'], "has no data", file=nodata)


if __name__ == "__main__":
	main()
