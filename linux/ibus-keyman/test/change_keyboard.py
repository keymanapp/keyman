#!/usr/bin/python3

import gi
import logging
import subprocess
import sys

from ast import literal_eval

gi.require_version('IBus', '1.0')
from gi.repository import IBus


def bus_has_engine(bus, name):
  engines = bus.get_engines_by_names([name])
  # print(engines)
  # print(len(engines))
  return len(engines)

def get_current_engine(bus):
  try:
    contextname = bus.current_input_context()
    # print(contextname)
    ic = IBus.InputContext.get_input_context(contextname, bus.get_connection())
    engine = ic.get_engine()
    if engine:
      return engine.get_name()
  except Exception as e:
    print("Failed to get current engine")
    print(e)


def change_keyboard(bus, name):
  try:
    contextname = bus.current_input_context()
    # print(contextname)
    ic = IBus.InputContext.get_input_context(contextname, bus.get_connection())
    # print(name)
    if bus_has_engine(bus, name) <= 0:
      print ("Could not find engine %s"%name)
    else:
      ic.set_engine(name)
  except Exception as e:
    print("Failed to change keyboard")
    print(e)

def install_to_ibus(lang, kmx_file):
  ibus_keyboardID = lang + ":" + kmx_file
  if sys.version_info.major == 3 and sys.version_info.minor < 6:
    dconfreadresult = subprocess.run(["dconf", "read", "/desktop/ibus/general/preload-engines"],
      stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
    dconfread = dconfreadresult.stdout.decode("utf-8", "strict")
  else:
    dconfreadresult = subprocess.run(["dconf", "read", "/desktop/ibus/general/preload-engines"],
      stdout=subprocess.PIPE, stderr= subprocess.STDOUT, encoding="UTF8")
    dconfread = dconfreadresult.stdout
  if (dconfreadresult.returncode == 0) and dconfread:
    preload_engines = literal_eval(dconfread)
    if ibus_keyboardID not in preload_engines:
      preload_engines.append(ibus_keyboardID)
      logging.info("Installing %s into IBus", ibus_keyboardID)
      if sys.version_info.major == 3 and sys.version_info.minor < 6:
        dconfwriteresult = subprocess.run(["dconf", "write", "/desktop/ibus/general/preload-engines", str(preload_engines)],
          stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
      else:
        dconfwriteresult = subprocess.run(["dconf", "write", "/desktop/ibus/general/preload-engines", str(preload_engines)],
          stdout=subprocess.PIPE, stderr= subprocess.STDOUT, encoding="UTF8")
      if (dconfwriteresult.returncode == 0):
        pass
        # restart IBus to be sure the keyboard is installed
        # logging.info("Restarting IBus for %s", ibus_keyboardID)
        # ibusrestartresult = subprocess.run(["ibus", "restart"])
        # if (ibusrestartresult.returncode != 0):
        #   message = "install_kmp.py: error %d: Could not restart IBus." % (ibusrestartresult.returncode)
        #   raise InstallError(InstallStatus.Continue, message)
        # else:
        #   logging.info("Restarted IBus for %s", ibus_keyboardID)
      else:
        message = "install_kmp.py: error %d: Could not install the keyboad to IBus." % (dconfwriteresult.returncode)
        raise InstallError(InstallStatus.Continue, message)
    else:
      logging.info("%s is already installed", ibus_keyboardID)
  else:
    message = "install_kmp.py: error %d: Could not read dconf preload-engines entry so cannot install to IBus" % (dconfreadresult.returncode)
    raise InstallError(InstallStatus.Continue, message)

def uninstall_from_ibus(lang, kmx_file):
  ibus_keyboardID = lang + ":" + kmx_file
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
    if ibus_keyboardID not in preload_engines:
      logging.info("%s is not installed in IBus", ibus_keyboardID)
      return
    preload_engines.remove(ibus_keyboardID)
    logging.info("Uninstalling %s from IBus", ibus_keyboardID)
    if sys.version_info.major == 3 and sys.version_info.minor < 6:
      result2 = subprocess.run(["dconf", "write", "/desktop/ibus/general/preload-engines", str(preload_engines)],
        stdout=subprocess.PIPE, stderr= subprocess.STDOUT)
    else:
      result2 = subprocess.run(["dconf", "write", "/desktop/ibus/general/preload-engines", str(preload_engines)],
        stdout=subprocess.PIPE, stderr= subprocess.STDOUT, encoding="UTF8")

#    name = "kyu-Kali:/usr/local/share/keyman/sil_kayah_kali/sil_kayah_kali.kmx"

def main(argv):
    if len(sys.argv) != 2:
        logging.error("change_keyboard.py <lang:kmx_path>")
        sys.exit(2)
    logging.basicConfig(level=logging.INFO)
    change_keyboard(sys.argv[1])

if __name__ == "__main__":
    main(sys.argv[1:])