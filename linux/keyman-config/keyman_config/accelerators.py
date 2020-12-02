#!/usr/bin/python3

import gi
from gi.repository import Gtk
gi.require_version('Gtk', '3.0')


def bind_accelerator(accelerators, widget, accelerator, signal='clicked'):
    key, mod = Gtk.accelerator_parse(accelerator)
    widget.add_accelerator(signal, accelerators, key, mod, Gtk.AccelFlags.VISIBLE)


def init_accel(win):
    win.accelerators = Gtk.AccelGroup()
    win.add_accel_group(win.accelerators)
