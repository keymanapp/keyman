#!/usr/bin/python3

import ast
import logging
import subprocess
import sys
import os.path
from shutil import rmtree

def uninstall_from_ibus(keyboardid):
	kbdir = os.path.join('/usr/local/share/keyman', keyboardid)
	kmnfile = os.path.join(kbdir, keyboardid+".kmn")
	if sys.version_info.major == 3 and sys.version_info.minor < 6:
		result = subprocess.run(["dconf", "read", "/desktop/ibus/general/preload-engines"],
			stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
		logging.debug(result.stdout.decode("utf-8", "strict"))
		dconfread = result.stdout.decode("utf-8", "strict")
	else:
		result = subprocess.run(["dconf", "read", "/desktop/ibus/general/preload-engines"],
			stdout=subprocess.PIPE, stderr= subprocess.STDOUT, encoding="UTF8")
		dconfread = result.stdout
	if (result.returncode == 0) and dconfread:
		preload_engines = ast.literal_eval(dconfread)
		if kmnfile not in preload_engines:
			logging.info("%s is not installed in IBus", kmnfile)
			return
		preload_engines.remove(kmnfile)
		logging.info("Uninstalling %s from IBus", kmnfile)
		if sys.version_info.major == 3 and sys.version_info.minor < 6:
			result2 = subprocess.run(["dconf", "write", "/desktop/ibus/general/preload-engines", str(preload_engines)],
				stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
		else:
			result2 = subprocess.run(["dconf", "write", "/desktop/ibus/general/preload-engines", str(preload_engines)],
				stdout=subprocess.PIPE, stderr= subprocess.STDOUT, encoding="UTF8")

def uninstall_kmp_shared(keyboardid):
	"""
	Uninstall a kmp from /usr/local/share/keyman

	Args:
		keyboardid (str): Keyboard ID
	"""
	kbdir = os.path.join('/usr/local/share/keyman', keyboardid)
	if not os.path.isdir(kbdir):
		logging.error("Keyboard directory for %s does not exist. Aborting", keyboardid)
		exit(3)

	kbdocdir = os.path.join('/usr/local/share/doc/keyman', keyboardid)
	kbfontdir = os.path.join('/usr/local/share/fonts/keyman', keyboardid)

	logging.info("Uninstalling keyboard: %s", keyboardid)
	if not os.access(kbdir, os.X_OK | os.W_OK): # Check for write access of keyman dir
		logging.error("You do not have permissions to uninstall the keyboard files. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
		exit(3)
	if os.path.isdir(kbdocdir):
		if not os.access(kbdocdir, os.X_OK | os.W_OK): # Check for write access of keyman doc dir
			logging.error("You do not have permissions to uninstall the documentation. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
			exit(3)
		rmtree(kbdocdir)
		logging.info("Removed documentation directory: %s", kbdocdir)
	else:
		logging.info("No documentation directory")
	if os.path.isdir(kbfontdir):
		if not os.access(kbfontdir, os.X_OK | os.W_OK): # Check for write access of keyman fonts
			logging.error("You do not have permissions to uninstall the font files. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
			exit(3)
		rmtree(kbfontdir)
		logging.info("Removed font directory: %s", kbfontdir)
	else:
		logging.info("No font directory")
	uninstall_from_ibus(keyboardid)
	rmtree(kbdir)
	logging.info("Removed keyman directory: %s", kbdir)
	logging.info("Finished uninstalling keyboard: %s", keyboardid)

def uninstall_kmp_user(keyboardid):
	"""
	Uninstall a kmp from ~/.local/share/keyman

	Args:
		keyboardid (str): Keyboard ID
	"""
	# TODO use XDG_DATA_HOME
	pass


def uninstall_kmp(keyboardid, sharedarea):
	"""
	Uninstall a kmp

	Args:
		keyboardid (str): Keyboard ID
		sharedarea (str): whether to uninstall from /usr/local or ~/.local
	"""
	if sharedarea:
		uninstall_kmp_shared(keyboardid)
	else:
		uninstall_kmp_user(keyboardid)
