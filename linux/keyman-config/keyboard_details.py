#!/usr/bin/python3

# Keyboard details window

import gi
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit', '3.0')
from gi.repository import Gtk, WebKit

class KeyboardDetailsView(Gtk.Window):
    def __init__(self, kmp):
        if "keyboard" in kmp["name"].lower():
            wintitle = kmp["name"]
        else:
            wintitle = kmp["name"] + " keyboard"
        Gtk.Window.__init__(self, title=wintitle)