#!/usr/bin/python3

import tempfile
import zipfile
import sys
import os.path
from kmpmetadata import parsemetadata, determine_filetype
from os import listdir, makedirs
from shutil import copy2
 
def list_files(directory, extension):
    return (f for f in listdir(directory) if f.endswith('.' + extension))

def main(argv):
	if len(sys.argv) != 2:
		print("install_kmp.py <kmpfile>")
		sys.exit(2)
	inputfile = sys.argv[1]

	if not os.path.isfile(inputfile):
		print("install_kmp.py Input file ", inputfile, " not found")
		print("install_kmp.py <kmpfile>")
		sys.exit(2)
	
	name, ext = os.path.splitext(inputfile)
	if ext != ".kmp":
		print("install_kmp.py Input file ", inputfile, "is not a kmp file")
		print("install_kmp.py <kmpfile>")
		sys.exit(2)

	# create a temporary directory using the context manager
	with tempfile.TemporaryDirectory() as tmpdirname:
		print('created temporary directory', tmpdirname)
		with zipfile.ZipFile(inputfile,"r") as zip_ref:
			zip_ref.extractall(tmpdirname)
#			jsonfiles = list_files(tmpdirname, "json")
#			for f in jsonfiles:
#				print(f)
			kmpjson = os.path.join(tmpdirname, "kmp.json")
			if os.path.isfile(kmpjson):
				info, system, options, keyboards, files = parsemetadata(kmpjson, False)
				kbid = keyboards[0]['id']
				print("Installing", info['name']['description'])
				if not os.access('/usr/local/share/keyman', os.X_OK | os.W_OK)): # Check for write access of keyman dir
					print("You do not have permissions to install the keyboard files. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
					exit(3)
				kbdir = os.path.join('/usr/local/share/keyman', kbid)
				if not os.access('/usr/local/share/doc/keyman', os.X_OK | os.W_OK): # Check for write access of keyman doc dir
					print("You do not have permissions to install the documentation. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
					exit(3)
				kbdocdir = os.path.join('/usr/local/share/doc/keyman', kbid)
				if not os.access('/usr/local/share/fonts/keyman', os.X_OK | os.W_OK): # Check for write access of keyman fonts
					print("You do not have permissions to install the font files. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
					exit(3)
				kbfontdir = os.path.join('/usr/local/share/fonts/keyman', kbid)
				for f in files:
					fpath = os.path.join(tmpdirname, f['name'])
					ftype = determine_filetype(f['name'])
					if ftype == "Documentation":
						print("Installing", f['name'], "as documentation")
						if not os.path.isdir(kbdocdir):
							os.makedirs(kbdocdir)
						copy2(fpath, kbdocdir)
					elif ftype == "Font":
						print("Installing", f['name'], "as font")
						if not os.path.isdir(kbfontdir):
							os.makedirs(kbfontdir)
						copy2(fpath, kbfontdir)
					elif ftype == "Metadata" or ftype == "Image" or ftype == "Keyboard icon"  or ftype == "Keyboard icon" or ftype == "Compiled keyboard" or ftype == "Compiled on screen keyboard":
						print("Installing", f['name'], "as keyman file")
						if not os.path.isdir(kbdir):
							os.makedirs(kbdir)
						copy2(fpath, kbdir)

if __name__ == "__main__":
	main(sys.argv[1:])
