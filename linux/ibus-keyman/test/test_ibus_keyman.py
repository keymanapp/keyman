#!/usr/bin/python3

import gi
import logging
import os
import sys
import time

from pynput.keyboard import Key, Controller

gi.require_version('Gtk', '3.0')
gi.require_version('Gdk', '3.0')
from gi.repository import Gtk, Gdk, Gio
gi.require_version('IBus', '1.0')
from gi.repository import IBus

from change_keyboard import change_keyboard, bus_has_engine, get_current_engine

class TestView(Gtk.Window):

    def __init__(self, test_name):
        Gtk.Window.__init__(self, title=test_name)

        self.known_modifiers = { "SHIFT" : Key.shift,
            "LCTRL" : Key.ctrl_l,
            "RCTRL" : Key.ctrl_r,
            "LALT" : Key.alt,
            "RALT" : Key.alt_gr }
        self.known_keys = { "K_A" : "a",
            "K_B" : "b",
            "K_C" : "c",
            "K_D" : "d",
            "K_E" : "e",
            "K_F" : "f",
            "K_O" : "o",
            "K_X" : "x",
            "K_1" : "1",
            "K_2" : "2",
            "K_3" : "3",
            "K_4" : "4",
            "K_5" : "5",
            "K_6" : "6",
            "K_7" : "7",
            "K_8" : "8",
            "K_SPACE" : Key.space,
            "K_BKSP" : Key.backspace }
        self.keys = self.context = self.expected = ""
        self.haspressedkeys = False
        self.test_name = test_name
        home = os.path.expanduser("~")
        keyboarddir = os.path.join(home, ".local/share/keyman/test_kmx")
        self.kmx_path = os.path.join(keyboarddir, self.test_name + ".kmx")
        self.kmn_path = os.path.join(keyboarddir, self.test_name + ".kmn")
        self.keyboard_id = "und:" + keyboarddir + "/" + self.test_name + ".kmx"

        # self.set_default_size(-1, 350)
        self.load_source(self.kmn_path)
        self.change_to_keyboard(self.keyboard_id, self.kmx_path)
        with open(self.test_name+".in", "wt") as f:
#             print(self.expected)
#             print(self.expected.encode('utf-8'))
# #            print([ord(c) for c in self.expected.encode('utf-8')])
#             print(self.expected.encode('utf-8').decode("utf-8", "strict"))
            f.write(self.expected)

        self.grid = Gtk.Grid()
        self.add(self.grid)

        self.create_textview()
        # self.create_toolbar()
        # self.create_buttons()
        logging.info("keys %d:%s:" % (len(self.keys), self.keys))
        logging.info("context %d:%s:" % (len(self.context), self.context))
        logging.info("expected %d:%s:" % (len(self.expected), self.expected))
        # self.do_keypresses(self.keys)

    def do_keypresses(self, args, data):
        if self.keys and not self.haspressedkeys:
            localkeyboard = Controller()
            keys = self.keys.split("]")
            # print(keys)
            for key in keys:
                if (key):
                    key = key[1:]
                    # print(key)
                    keyparts = key.split(" ")
                    # print(keyparts)
                    mods = []
                    for part in keyparts:
                        # print(part)
                        if part in self.known_keys:
                            mainkey = part
                            # print("mainkey:", mainkey)
                        elif part in self.known_modifiers:
                            mods.append(part)
                            # print("mods:", mods)
                        # else:
                            # print("unknown part %s", part)
                    logging.info("key is %s with modifiers %s" % (mainkey, mods))
                    # self.activate_focus()
                    # self.grab_focus()
                    # self.textview.grab_focus()
                    if len(mods) == 0:
                        localkeyboard.press(self.known_keys[mainkey])
                        localkeyboard.release(self.known_keys[mainkey])
                    elif len(mods) == 1:
                        with localkeyboard.pressed(self.known_modifiers[mods[0]]):
                            localkeyboard.press(self.known_keys[mainkey])
                            localkeyboard.release(self.known_keys[mainkey])
                    elif len(mods) == 2:
                        with localkeyboard.pressed(self.known_modifiers[mods[0]]):
                            with localkeyboard.pressed(self.known_modifiers[mods[1]]):
                                localkeyboard.press(self.known_keys[mainkey])
                                localkeyboard.release(self.known_keys[mainkey])
                    elif len(mods) == 3:
                        with localkeyboard.pressed(self.known_modifiers[mods[0]]):
                            with localkeyboard.pressed(self.known_modifiers[mods[1]]):
                                    with localkeyboard.pressed(self.known_modifiers[mods[2]]):
                                        localkeyboard.press(self.known_keys[mainkey])
                                        localkeyboard.release(self.known_keys[mainkey])
                    elif len(mods) == 4:
                        with localkeyboard.pressed(self.known_modifiers[mods[0]]):
                            with localkeyboard.pressed(self.known_modifiers[mods[1]]):
                                with localkeyboard.pressed(self.known_modifiers[mods[2]]):
                                    with localkeyboard.pressed(self.known_modifiers[mods[3]]):
                                        localkeyboard.press(self.known_keys[mainkey])
                                        localkeyboard.release(self.known_keys[mainkey])
                    else:
                        logging.warning("too many modifiers %d:%s", len(mods), mods)
                        localkeyboard.press(self.known_keys[mainkey])
                        localkeyboard.release(self.known_keys[mainkey])
                time.sleep(0.1)

            self.haspressedkeys = True
            time.sleep(1)
            with localkeyboard.pressed(Key.ctrl_l):
                localkeyboard.press("a")
                localkeyboard.release("a")

    def change_to_keyboard(self, keyboard_id, kmx_path):
        logging.debug(keyboard_id)
        # bad_keyboard = "und:/home/daniel/.local/share/keyman/test_kmx/001 - basic input UnicodeI.kmn.kmx"
        # bad_keyboard = "und:/home/daniel/.local/share/keyman/test_kmx/.kmx"
        # bad_keyboard = "und:/home/daniel/.local/share/keyman/test_kmx/${tests[count]}.kmx"
        bad_keyboard = "und:/home/daniel/.local/share/keyman/test_kmx/012 - ralt.kmn.kmx"
        try:
            logging.debug("getting bus")
            bus = IBus.Bus()
            logging.debug("getting default keyboard")
            self.default_keyboard = get_current_engine(bus)
            logging.debug(self.default_keyboard)
            # if bus_has_engine(bus, keyboard_id) <= 0:
            logging.debug("installing to ibus")
            ibus_settings = Gio.Settings.new("org.freedesktop.ibus.general")
            # logging.debug(ibus_settings)
            # list_keys = ibus_settings.list_keys()
            # logging.debug(list_keys)
            preload_engines = ibus_settings.get_strv("preload-engines")
            logging.debug(preload_engines)
            if bad_keyboard in preload_engines:
                preload_engines.remove(bad_keyboard)
            if keyboard_id not in preload_engines:
                preload_engines.append(keyboard_id)
            logging.debug(preload_engines)
            ibus_settings.set_strv("preload-engines", preload_engines)
            # ibus_settings.apply()
            # ibus_settings.sync()
            bus.preload_engines(preload_engines)
            # bus.exit(True)
            # time.sleep(2)
            # reconnected = False
            # bus = IBus.Bus()
            # while not reconnected:
            #     reconnected = bus.is_connected()
            # install_to_ibus("und", kmx_path)

            logging.info("changing keyboard to %s", keyboard_id)
            change_keyboard(bus, keyboard_id)
            # logging.debug("changed keyboard to %s", keyboard_id)
            # logging.debug("uninstalling from ibus")
            # uninstall_from_ibus("und", kmx_path)
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
            #bus.exit(True)
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
                        self.expected = expected.encode("utf-8").decode('unicode-escape')
                        # print(expected)
                        # print(expected.encode("utf-8"))
                        # self.expected = expected
                        # self.expected = expected.replace("\U", "\u")
                    if line.startswith("c context: "):
                        self.context = line[11:].rstrip()

    def create_textview(self):
        scrolledwindow = Gtk.ScrolledWindow()
        scrolledwindow.set_hexpand(True)
        scrolledwindow.set_vexpand(True)
        self.grid.attach(scrolledwindow, 0, 1, 3, 1)

        self.textview = Gtk.TextView()
        self.textview.connect("focus-in-event", self.do_keypresses)
        self.textview.connect("select-all", self.on_select_all)
        self.textbuffer = self.textview.get_buffer()
        self.textbuffer.set_text(self.context)
        # self.textbuffer.set_text("This is some text inside of a Gtk.TextView. "
        #     + "Select text and click one of the buttons 'bold', 'italic', "
        #     + "or 'underline' to modify the text accordingly.")
        scrolledwindow.add(self.textview)

        # self.tag_bold = self.textbuffer.create_tag("bold",
        #     weight=Pango.Weight.BOLD)
        # self.tag_italic = self.textbuffer.create_tag("italic",
        #     style=Pango.Style.ITALIC)
        # self.tag_underline = self.textbuffer.create_tag("underline",
        #     underline=Pango.Underline.SINGLE)
        # self.tag_found = self.textbuffer.create_tag("found",
        #     background="yellow")

    def on_select_all(self, args, select):
        self.on_destroy(args)
        pass

    def on_destroy(self, args):
        with open(self.test_name+".out", "wt") as f:
            start = self.textbuffer.get_start_iter()
            end = self.textbuffer.get_end_iter()
            # start, end = self.textbuffer.get_bounds()
            text = self.textbuffer.get_text(start, end, True)
            logging.info("text buffer:%s", text)
            f.write(text)
        self.reset_keyboard(self.keyboard_id)
        Gtk.main_quit()


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