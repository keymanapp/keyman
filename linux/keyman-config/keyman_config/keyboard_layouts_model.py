#!/usr/bin/python3

from enum import IntEnum

import gi

gi.require_version('Gtk', '3.0')

from gi.repository import GdkPixbuf, Gtk


class Model(IntEnum):
    # Keep in sync with _add_model() below
    ICON = 0
    NAME = 1
    VERSION = 2
    PACKAGEID = 3
    LOCATION = 4
    AREA = 5
    INSTALLPATH = 6
    WELCOMEFILE = 7
    OPTIONSFILE = 8


def create_kbd_layouts_model():
    return Gtk.ListStore(
      # Keep in sync with Model above
      GdkPixbuf.Pixbuf,  # icon
      str,    # name
      str,    # installed package version
      str,    # package ID
      int,    # enum InstallLocation
      str,    # InstallLocation area
      str,    # Tooltip with InstallLocation path
      str,    # path to welcome file if it exists or None
      str)    # path to options file if it exists or None
