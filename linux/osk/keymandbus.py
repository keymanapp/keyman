#!/usr/bin/python

#import gi
#gi.require_version('IBus', '1.0')
#gi.require_version('DBus', '1.0')
#from gi.repository import IBus
#from gi.repository import DBus

from gi.repository import GLib
import dbus
from dbus.mainloop.glib import DBusGMainLoop

DBusGMainLoop(set_as_default=True)

session_bus = dbus.SessionBus()

loop = GLib.MainLoop()
loop.run()

session_bus.add_signal_receiver()