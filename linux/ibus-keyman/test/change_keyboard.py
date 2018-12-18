#!/usr/bin/python3

import gi
import logging
import sys

gi.require_version('IBus', '1.0')
from gi.repository import IBus

def bus_has_engine(bus, name):
  engines = bus.get_engines_by_names([name])
  return len(engines)

def get_current_engine(bus):
  try:
    contextname = bus.current_input_context()
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
    ic = IBus.InputContext.get_input_context(contextname, bus.get_connection())
    if bus_has_engine(bus, name) <= 0:
      print ("Could not find engine %s"%name)
    else:
      ic.set_engine(name)
  except Exception as e:
    print("Failed to change keyboard")
    print(e)

def main(argv):
    if len(sys.argv) != 2:
        logging.error("change_keyboard.py <lang:kmx_path>")
        sys.exit(2)
    logging.basicConfig(level=logging.INFO)
    change_keyboard(sys.argv[1])

if __name__ == "__main__":
    main(sys.argv[1:])