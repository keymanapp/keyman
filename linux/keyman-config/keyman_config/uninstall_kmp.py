#!/usr/bin/python3

import ast
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
		print(result.stdout.decode("utf-8", "strict"))
		dconfread = result.stdout.decode("utf-8", "strict")
	else:
		result = subprocess.run(["dconf", "read", "/desktop/ibus/general/preload-engines"],
			stdout=subprocess.PIPE, stderr= subprocess.STDOUT, encoding="UTF8")
		dconfread = result.stdout
	if (result.returncode == 0) and dconfread:
		preload_engines = ast.literal_eval(dconfread)
		if kmnfile not in preload_engines:
			print(kmnfile, "is not installed in IBus")
			return
		preload_engines.remove(kmnfile)
		print("Uninstalling", kmnfile, "from IBus")
		if sys.version_info.major == 3 and sys.version_info.minor < 6:
			result2 = subprocess.run(["dconf", "write", "/desktop/ibus/general/preload-engines", str(preload_engines)],
				stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
		else:
			result2 = subprocess.run(["dconf", "write", "/desktop/ibus/general/preload-engines", str(preload_engines)],
				stdout=subprocess.PIPE, stderr= subprocess.STDOUT, encoding="UTF8")

def uninstall_kmp(keyboardid):
	"""
	Uninstall a kmp from /usr/local/share/keyman

	Args:
		keyboardid (str): Keyboard ID
	"""
	kbdir = os.path.join('/usr/local/share/keyman', keyboardid)
	if not os.path.isdir(kbdir):
		print("Keyboard directory for", keyboardid, "does not exist. Aborting")
		exit(3)

	kbdocdir = os.path.join('/usr/local/share/doc/keyman', keyboardid)
	kbfontdir = os.path.join('/usr/local/share/fonts/keyman', keyboardid)

	print("Uninstalling keyboard:", keyboardid)
	if not os.access(kbdir, os.X_OK | os.W_OK): # Check for write access of keyman dir
		print("You do not have permissions to uninstall the keyboard files. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
		exit(3)
	if os.path.isdir(kbdocdir):
		if not os.access(kbdocdir, os.X_OK | os.W_OK): # Check for write access of keyman doc dir
			print("You do not have permissions to uninstall the documentation. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
			exit(3)
		rmtree(kbdocdir)
		print("Removed documentation directory:", kbdocdir)
	else:
		print("No documentation directory")
	if os.path.isdir(kbfontdir):
		if not os.access(kbfontdir, os.X_OK | os.W_OK): # Check for write access of keyman fonts
			print("You do not have permissions to uninstall the font files. You need to be a member of the keyman group. `sudo adduser <username> keyman`")
			exit(3)
		rmtree(kbfontdir)
		print("Removed font directory:", kbfontdir)
	else:
		print("No font directory")
	uninstall_from_ibus(keyboardid)
	rmtree(kbdir)
	print("Removed keyman directory:", kbdir)
	print("Finished uninstalling keyboard:", keyboardid)


def main(argv):
	if len(sys.argv) != 2:
		print("uninstall_kmp.py <keyboard id>")
		sys.exit(2)
	keyboardid = sys.argv[1]
	uninstall_kmp(keyboardid)

if __name__ == "__main__":
	main(sys.argv[1:])
