#!/usr/bin/python3

import gi
import logging
import os
import sys
import time
from threading import Timer

from keypress_actions import KeyPressAction, KeyReleaseAction

try:
    from xkbgroup import XKeyboard
    has_xkbgroup = True
except:
    has_xkbgroup = False

gi.require_version('Gtk', '3.0')
gi.require_version('Gdk', '3.0')
from gi.repository import Gtk, Gdk, Gio
gi.require_version('IBus', '1.0')
from gi.repository import IBus

from change_keyboard import change_keyboard, bus_has_engine, get_current_engine

class TestView(Gtk.Window):

    def __init__(self, test_name):
        Gtk.Window.__init__(self, title=test_name)
        self.known_modifiers = {
            "LCTRL" : 37,
            "RCTRL" : 105,
            "LALT" : 64,
            "RALT" : 108,
            "SHIFT" : 50,
            "ALT" : 64,
            "CTRL" : 37 }

        self.known_keys = {
            "K_A" : 38,
            "K_B" : 56,
            "K_C" : 54,
            "K_D" : 40,
            "K_E" : 26,
            "K_F" : 41,
            "K_G" : 42,
            "K_H" : 43,
            "K_I" : 31,
            "K_J" : 44,
            "K_K" : 45,
            "K_L" : 46,
            "K_M" : 58,
            "K_N" : 57,
            "K_O" : 32,
            "K_P" : 33,
            "K_Q" : 24,
            "K_R" : 27,
            "K_S" : 39,
            "K_T" : 28,
            "K_U" : 30,
            "K_V" : 55,
            "K_W" : 25,
            "K_X" : 53,
            "K_Y" : 29,
            "K_Z" : 52,
            "K_COLON"   : 47, #      // &HBA
            "K_EQUAL"   : 21, #      // &HBB
            "K_COMMA"   : 59, #      // &HBC
            "K_HYPHEN"  : 20, #   // &HBD
            "K_PERIOD"  : 60, #   // &HBE
            "K_SLASH"   : 61, #      // &HBF
            "K_BKQUOTE" : 49, #    // &HC0
            "K_LBRKT"   : 34, #      // &HDB
            "K_BKSLASH" : 51, #    // &HDC
            "K_RBRKT"   : 35, #      // &HDD
            "K_QUOTE"   : 48, #      // &HDE
            "K_oE2"     : 94, #      // &HE2
            "K_0" : 19,
            "K_1" : 10,
            "K_2" : 11,
            "K_3" : 12,
            "K_4" : 13,
            "K_5" : 14,
            "K_6" : 15,
            "K_7" : 16,
            "K_8" : 17,
            "K_9" : 18,
            "K_F1" : 67,
            "K_F2" : 68,
            "K_F3" : 69,
            "K_F4" : 70,
            "K_F5" : 71,
            "K_F6" : 72,
            "K_F7" : 73,
            "K_F8" : 74,
            "K_F9" : 75,
            "K_F10" : 76,
            "K_F11" : 95,
            "K_F12" : 96,
            "K_CAPS" : 66,
            "K_ENTER" : 36,
            "K_ESC" : 9,
            "K_SPACE" : 65,
            "K_TAB" : 23,
            "K_UP" : 111,
            "K_DOWN" : 116,
            "K_LEFT" : 113,
            "K_RIGHT" : 114,
            "K_HOME" : 110,
            "K_END" : 115,
            "K_BKSP" : 22 }
        self.keys = self.context = self.expected = ""
        self.haspressedkeys = False
        self.test_name = test_name
        home = os.path.expanduser("~")
        keyboarddir = os.path.join(home, ".local/share/keyman/test_kmx")
        self.kmx_path = os.path.join(keyboarddir, self.test_name + ".kmx")
        self.kmn_path = os.path.join(keyboarddir, self.test_name + ".kmn")
        self.keyboard_id = "und:" + keyboarddir + "/" + self.test_name + ".kmx"

        self.load_source(self.kmn_path)
        self.change_to_keyboard(self.keyboard_id, self.kmx_path)
        with open(self.test_name+".in", "wt") as f:
            f.write(self.expected)

        self.grid = Gtk.Grid()
        self.add(self.grid)
        self.create_textview()

        logging.info("keys %d:%s:" % (len(self.keys), self.keys))
        logging.info("context %d:%s:" % (len(self.context), self.context))
        logging.info("expected %d:%s:" % (len(self.expected), self.expected))

    def on_focus_in(self, args, data):
        t = Timer(1.0, self.do_keypresses)
        t.start()

    def do_keypresses(self):
        if self.keys and not self.haspressedkeys:
            self.haspressedkeys = True
            if has_xkbgroup:
                with XKeyboard() as xkb:
                    logging.info("xkb %d:%s:%s", xkb.group_num, xkb.group_symbol, xkb.group_name)
            keys = self.keys.split("]")
            for key in keys:
                if (key):
                    key = key[1:]
                    keyparts = key.split(" ")
                    mods = []
                    for part in keyparts:
                        if part in self.known_keys:
                            mainkey = part
                        elif part in self.known_modifiers:
                            mods.append(part)
                    logging.info("key is %s with modifiers %s" % (mainkey, mods))
                    keyval = self.known_keys[mainkey]
                    logging.info("%s", keyval)
                    for modifier in mods:
                        modkeypress = KeyPressAction(self.known_modifiers[modifier])
                        modkeypress._keyPress(self.known_modifiers[modifier])

                    mainkeypress = KeyPressAction(keyval)
                    mainkeyrelease = KeyReleaseAction(keyval)
                    mainkeypress._keyPress(keyval)
                    mainkeyrelease._keyRelease(keyval)

                    for modifier in mods:
                        modkeypress = KeyReleaseAction(self.known_modifiers[modifier])
                        modkeypress._keyRelease(self.known_modifiers[modifier])

                    time.sleep(0.05)

            t = Timer(1.0, self.do_destroy)
            t.start()

    def change_to_keyboard(self, keyboard_id, kmx_path):
        logging.debug(keyboard_id)
        try:
            logging.debug("getting bus")
            bus = IBus.Bus()
            logging.debug("getting default keyboard")
            self.default_keyboard = get_current_engine(bus)
            logging.debug(self.default_keyboard)
            logging.debug("installing to ibus")
            ibus_settings = Gio.Settings.new("org.freedesktop.ibus.general")
            preload_engines = ibus_settings.get_strv("preload-engines")
            logging.debug(preload_engines)
            # if bad_keyboard in preload_engines:
            #     preload_engines.remove(bad_keyboard)
            if keyboard_id not in preload_engines:
                preload_engines.append(keyboard_id)
            logging.debug(preload_engines)
            ibus_settings.set_strv("preload-engines", preload_engines)
            bus.preload_engines(preload_engines)
            logging.info("changing keyboard to %s", keyboard_id)
            change_keyboard(bus, keyboard_id)
        except Exception as e:
            logging.debug("Failed to set up keyboard")
            logging.debug(e)

    def reset_keyboard(self, keyboard_id):
        try:
            logging.debug("getting bus")
            bus = IBus.Bus()
            logging.debug("setting keyboard back to default: %s", self.default_keyboard)
            change_keyboard(bus, self.default_keyboard)
            ibus_settings = Gio.Settings.new("org.freedesktop.ibus.general")
            preload_engines = ibus_settings.get_strv("preload-engines")
            logging.debug(preload_engines)
            if keyboard_id in preload_engines:
                preload_engines.remove(keyboard_id)
            logging.debug(preload_engines)
            ibus_settings.set_strv("preload-engines", preload_engines)
            bus.preload_engines(preload_engines)
        except Exception as e:
            logging.debug("Failed to reset keyboard")
            logging.debug(e)


    def load_source(self, kmn_path):
        if os.path.exists(kmn_path):
            with open(kmn_path) as kmn:
                for line in kmn:
                    if line.startswith("c keys: "):
                        self.keys = line[8:].rstrip()
                    if line.startswith("c expected: "):
                        expected = line[12:].rstrip()
                        if expected == "\\b": # beep test is sound not text
                            self.expected = ""
                        else:
                            self.expected = expected.encode("utf-8").decode('unicode-escape')
                    if line.startswith("c context: "):
                        self.context = line[11:].rstrip()

    def create_textview(self):
        scrolledwindow = Gtk.ScrolledWindow()
        scrolledwindow.set_hexpand(True)
        scrolledwindow.set_vexpand(True)
        self.grid.attach(scrolledwindow, 0, 1, 3, 1)

        self.textview = Gtk.TextView()
        self.textview.connect("focus-in-event", self.on_focus_in)
        self.textbuffer = self.textview.get_buffer()
        self.textbuffer.set_text(self.context)
        scrolledwindow.add(self.textview)

    def do_destroy(self):
        with open(self.test_name+".out", "wt") as f:
            start = self.textbuffer.get_start_iter()
            end = self.textbuffer.get_end_iter()
            text = self.textbuffer.get_text(start, end, True)
            logging.info("text buffer:%s", text)
            f.write(text)
        self.reset_keyboard(self.keyboard_id)
        Gtk.main_quit()

    def on_destroy(self, args):
        self.do_destroy()

# Test view contains a single multiline edit

# input test_name
# test dir ~/.local/share/keyman/test_kmx
# read test_name.kmn to get e.g.
#c keys: [K_1][K_BKSP][K_2][K_BKSP][K_3][K_BKSP][K_4][K_BKSP][K_5][K_BKSP][K_6][K_BKSP]
#c expected: aa
#c context: 

# keyboard is und:~/.local/share/keyman/test_kmx/test_name.kmx
# can the current ibus keyboard be changed from a python program?

# set initial text to "context"
# parse "keys" and input them
#    can I generate keypresses in python?
# read output text and compare to "expected"


def main(argv):
    if len(sys.argv) != 2:
        logging.error("Too many arguments: %s", sys.argv)
        logging.error("test_ibus_keyman.py <test_name>")
        sys.exit(2)
    if len(sys.argv[1]) == 0:
        logging.error("Empty test name: %s", sys.argv)
        logging.error("test_ibus_keyman.py <test_name>")
        sys.exit(2)
    logging.basicConfig(level=logging.INFO, format='%(levelname)s:%(message)s')
    # logging.basicConfig(level=logging.DEBUG, format='%(levelname)s:%(message)s')
    w = TestView(sys.argv[1])
    w.connect("destroy", w.on_destroy)
    w.resize(576, 324)
    w.show_all()
    Gtk.main()

if __name__ == "__main__":
    main(sys.argv[1:])