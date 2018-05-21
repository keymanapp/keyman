#!/usr/bin/python3

import argparse
import json
import tempfile
import zipfile
import sys
import os.path
import requests
from kmpmetadata import parsemetadata, parseinfdata, determine_filetype
from get_kmp import get_kmp, get_keyboard_data
from list_installed_kmp import get_kmp_version
from uninstall_kmp import uninstall_kmp
from os import listdir, makedirs
from shutil import copy2
 
def list_files(directory, extension):
	return (f for f in listdir(directory) if f.endswith('.' + extension))

def get_metadata(inputfile, tmpdirname):
	"""
	Get metadata from kmp.json if it exists.

	Args:
		inputfile (str): path to kmp file
		tmpdirname(str): temp directory to extract kmp

	Returns:
		list[5]: info, system, options, keyboards, files
			see kmpmetadata.parsemetadata for details
	"""
	with zipfile.ZipFile(inputfile,"r") as zip_ref:
		zip_ref.extractall(tmpdirname)
#		jsonfiles = list_files(tmpdirname, "json")
#		for f in jsonfiles:
#			print(f)
		kmpjson = os.path.join(tmpdirname, "kmp.json")
		if os.path.isfile(kmpjson):
			return parsemetadata(kmpjson, False)
		else:
			return None, None, None, None, None

def get_infdata(inputfile, tmpdirname):
	"""
	Get metadata from kmp.inf if it exists.

	Args:
		inputfile (str): path to kmp file
		tmpdirname(str): temp directory to extract kmp

	Returns:
		list[5]: info, system, options, keyboards, files
			see kmpmetadata.parseinfdata for details
	"""
	with zipfile.ZipFile(inputfile,"r") as zip_ref:
		zip_ref.extractall(tmpdirname)
		kmpinf = os.path.join(tmpdirname, "kmp.inf")
		if os.path.isfile(kmpinf):
			return parseinfdata(kmpinf, False)
		else:
			return None, None, None, None, None

def install_kmp(inputfile, withkmn=False):
	"""
	Install a kmp file to /usr/local/share/keyman

	Args:
		inputfile (str): path to kmp file
		withkmn(bool, default=False): whether to attempt to get a source kmn for the keyboard
	"""
	# create a temporary directory using the context manager
	with tempfile.TemporaryDirectory() as tmpdirname:
		print('created temporary directory', tmpdirname)
		info, system, options, keyboards, files = get_metadata(inputfile, tmpdirname)
		if not keyboards:
			# no json file so trying inf file
			info, system, options, keyboards, files = get_infdata(inputfile, tmpdirname)
		if files and not keyboards:
			#inf file may not have keyboards so generate it if needed
			keyboards = [ { 'name' : info['name']['description'],
				'id' : os.path.basename(os.path.splitext(inputfile)[0]),
				'version' : info['version']['description'] } ]
		if keyboards:
			kbid = keyboards[0]['id']
			print("Installing", info['name']['description'])
			if not os.access('/usr/local/share/keyman', os.X_OK | os.W_OK): # Check for write access of keyman dir
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

			if withkmn:
				kbdata = get_keyboard_data(kbid)
				# just get latest version of kmn unless there turns out to be a way to get the version of a file at a date
#				if 'lastModifiedDate' in kbdata:
#					lastModifiedDate = datetime(kbdata['lastModifiedDate'])
#					print("Last Modified Date:", lastModifiedDate)
				if kbdata:
					if 'sourcePath' in kbdata:
						base_url = "https://raw.github.com/keymanapp/keyboards/master/" + kbdata['sourcePath']
						kmn_url = base_url + "/source/" + kbid + ".kmn"
						response = requests.get(kmn_url)
						if response.status_code == 200:
							downloadfile = os.path.join(tmpdirname, kbid + ".kmn")
							with open(downloadfile, 'wb') as f:
								f.write(response.content)
							print("Installing", kbid + ".kmn", "as keyman file, mininum version:", kbdata['minKeymanVersion'])
							if not os.path.isdir(kbdir):
								os.makedirs(kbdir)
							copy2(downloadfile, kbdir)
						else:
							print("install_kmp.py: warning: no kmn source file for", kbid)
					if not os.path.isdir(kbdir):
							os.makedirs(kbdir)
					with open(os.path.join(kbdir, kbid + '.json'), 'w') as outfile:
						json.dump(kbdata, outfile)
						print("Installing api data file", kbid + ".json", "as keyman file")
				else:
					print("install_kmp.py: error: cannot download keyboard data so not installing.")

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
				elif ftype == "Metadata" or ftype == "Image" or ftype == "Keyboard icon"  or ftype == "Keyboard source" or ftype == "Keyboard icon" or ftype == "Compiled keyboard" or ftype == "Compiled on screen keyboard":
					print("Installing", f['name'], "as keyman file")
					if not os.path.isdir(kbdir):
						os.makedirs(kbdir)
					copy2(fpath, kbdir)
		else:
			print("install_kmp.py: error: No kmp.json or kmp.inf found in", inputfile)
			print("Contents of", inputfile+":")
			for o in os.listdir(tmpdirname):
				print(o)


def main():
	parser = argparse.ArgumentParser(description='Install a Keyman keyboard, either a local .kmp file or specify a keyboard id to download and install')
	parser.add_argument('-f', metavar='<kmpfile>', help='Keyman kmp file')
	parser.add_argument('-k', metavar='<keyboardid>', help='Keyman keyboard id')

	args = parser.parse_args()
	if args.k and args.f:
		print("install_kmp.py: error: too many arguments: either install a local kmp file or specify a keyboard id to download and install.")
		sys.exit(2)

	if args.f:
		name, ext = os.path.splitext(args.f)
		if ext != ".kmp":
			print("install_kmp.py Input file", args.f, "is not a kmp file.")
			print("install_kmp.py -f <kmpfile>")
			sys.exit(2)

		if not os.path.isfile(args.f):
			print("install_kmp.py Keyman kmp file", args.f, "not found.")
			print("install_kmp.py -f <kmpfile>")
			sys.exit(2)

		install_kmp(args.f)
	elif args.k:
		installed_kmp_ver = get_kmp_version(args.k)
		kbdata = get_keyboard_data(args.k)
		if not kbdata:
			print("install_kmp.py: error: Could not download keyboard data for", args.k)
		if installed_kmp_ver:
#			print("Found installed version", installed_kmp_ver)
#			print("Api knows about version", kbdata['version'])
			if kbdata['version'] == installed_kmp_ver:
				print("install_kmp.py The %s version of the %s keyboard is already installed." % (installed_kmp_ver, args.k))
				sys.exit(0)
			elif float(kbdata['version']) > float(installed_kmp_ver):
				print("install_kmp.py A newer version of %s keyboard is available. Uninstalling old version %s then downloading and installing new version %s." % (args.k, installed_kmp_ver, kbdata['version']))
				uninstall_kmp(args.k)

		kmpfile = get_kmp(args.k)
		if kmpfile:
			install_kmp(kmpfile, True)
		else:
			print("install_kmp.py: error: Could not download keyboard package", args.k)
	else:
		print("install_kmp.py: error: no arguments: either install a local kmp file or specify a keyboard id to download and install.")
		sys.exit(2)



if __name__ == "__main__":
	main()
