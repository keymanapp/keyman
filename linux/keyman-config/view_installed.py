#!/usr/bin/python3

import os.path
import pathlib
import gi
gi.require_version('Gtk', '3.0')
gi.require_version('Gdk', '3.0')
gi.require_version('WebKit', '3.0')
from gi.repository import Gtk, Gdk, WebKit
from list_installed_kmp import get_installed_kmp
from welcome import WelcomeView
from keyboard_details import KeyboardDetailsView
from uninstall_kmp import uninstall_kmp

class KeyboardBox(Gtk.Box):
    def __init__(self, kmp, window):
        Gtk.Box.__init__(self)
        self.parent = window

        print(kmp)
        self.kmp = kmp
        icofile = os.path.join("/usr/local/share/keyman", self.kmp["id"], self.kmp["id"] + ".ico.jpg")
        if not os.path.isfile(icofile):
            icofile = "icon_kmp.png"
        self.image = Gtk.Image.new_from_file(icofile)
        #image.set_from_pixbuf(pics[0])
        #grid.add(image)
        self.pack_start(self.image, False, False, 10)

        self.label1 = Gtk.Label()
        labeltext = kmp["name"] + " (" + kmp["version"] + ")"
        self.label1.set_text(labeltext)
        self.label1.set_halign(Gtk.Align.END)
        #grid.attach_next_to(label1, image, Gtk.PositionType.RIGHT, 1, 1)
        self.pack_start(self.label1, False, False, 10)

        self.expandbutton = Gtk.Button()
        self.expandimage = Gtk.Image.new_from_file("expand20.png")
        self.expandbutton.set_image(self.expandimage)
        self.expandbutton.set_tooltip_text("More information about " + kmp["name"])
        #grid.attach_next_to(expandbutton, helpbutton, Gtk.PositionType.RIGHT, 1, 1)
        self.expandbutton.connect("clicked", self.on_expand_clicked)
        self.pack_end(self.expandbutton, False, False, 0)

        self.uninstallbutton = Gtk.Button()
        self.uninstallimage = Gtk.Image.new_from_file("cross20.png")
        self.uninstallbutton.set_image(self.uninstallimage)
        self.uninstallbutton.set_tooltip_text("Uninstall " + kmp["name"])
        #grid.attach_next_to(uninstallbutton, expandbutton, Gtk.PositionType.RIGHT, 1, 1)
        self.uninstallbutton.connect("clicked", self.on_uninstall_clicked)
        self.pack_end(self.uninstallbutton, False, False, 0)

        self.helpbutton = Gtk.Button()
        self.helpimage = Gtk.Image.new_from_file("help20.png")
        self.helpbutton.set_image(self.helpimage)
        self.helpbutton.set_tooltip_text("Help for " + kmp["name"])
        #grid.attach_next_to(helpbutton, label1, Gtk.PositionType.RIGHT, 1, 1)
        self.helpbutton.connect("clicked", self.on_help_clicked)
        self.pack_end(self.helpbutton, False, False, 0)

    def on_help_clicked(self, button):
        print("Open welcome.htm for", self.kmp["name"], "if available")
        welcome_file = os.path.join("/usr/local/share/doc/keyman", self.kmp["id"], "welcome.htm")
        if os.path.isfile(welcome_file):
            uri_path = pathlib.Path(welcome_file).as_uri()
            print("opening", uri_path)
            w = WelcomeView(uri_path, self.kmp["id"])
            w.resize(800, 600)
            w.show_all()
        else:
            print("not available")

    def on_uninstall_clicked(self, button):
        print("Uninstall keyboard", self.kmp["id"], "?")
        dialog = Gtk.MessageDialog(self.parent, 0, Gtk.MessageType.QUESTION,
            Gtk.ButtonsType.YES_NO, "Uninstall keyboard?")
        dialog.format_secondary_text(
            "Are you sure that you want to uninstall the" + self.kmp["name"] + "keyboard")
        response = dialog.run()
        dialog.destroy()
        if response == Gtk.ResponseType.YES:
            print("Uninstalling keyboard", self.kmp["name"])
            uninstall_kmp(self.kmp["id"])
            self.parent.refresh_installed_kmp()
        elif response == Gtk.ResponseType.NO:
            print("Not uninstalling keyboard", self.kmp["name"])

    def on_expand_clicked(self, button):
        print("Show keyboard details of", self.kmp["name"])
        w = KeyboardDetailsView(self.kmp)
        w.resize(800, 450)
        w.show_all()

class KmpGrid(Gtk.Grid):
    def __init__(self):
        Gtk.Grid.__init__(self)
        installed_kmp = get_installed_kmp()
        self.set_column_homogeneous(True)

        prevbox = None
        shade = False
        for kmp in sorted(installed_kmp):
            print(kmp)
            kbbox = KeyboardBox(installed_kmp[kmp], self)
            if shade:
                kbbox.override_background_color(Gtk.StateType.NORMAL, Gdk.RGBA(.8,.8,.8,.5))
                shade = False
            else:
                shade = True
            if not prevbox:
                self.add(kbbox)
                prevbox = kbbox
            else:
                self.attach_next_to(kbbox, prevbox, Gtk.PositionType.BOTTOM, 1, 1)
                prevbox = kbbox

class ViewInstalledWindow(Gtk.Window):
    def __init__(self):
        Gtk.Window.__init__(self, title="Keyman keyboard")

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)

        self.s = Gtk.ScrolledWindow()
        vbox.pack_start(self.s, True, True, 0)

        hbox = Gtk.Box(spacing=6)
        #hbox.set_halign(Gtk.Align.FILL)
        vbox.pack_start(hbox, False, False, 0)

        self.grid = KmpGrid()
        self.s.add(self.grid)

        #button = Gtk.Button.new_with_label("Click Me")
        #button.connect("clicked", self.on_click_me_clicked)
        #hbox.pack_start(button, False, False, 0)

        #button = Gtk.Button.new_with_mnemonic("_Open")
        #button.connect("clicked", self.on_open_clicked)
        #hbox.pack_start(button, False, False, 0)

        button = Gtk.Button.new_with_mnemonic("_Close")
        button.connect("clicked", self.on_close_clicked)
        hbox.pack_end(button, False, False, 0)

        self.add(vbox)

    def refresh_installed_kmp(self):
        print("need to refresh window after uninstalling a keyboard")
        self.grid.destroy()
        self.grid = KmpGrid()
        self.s.add(self.grid)


    #def on_click_me_clicked(self, button):
    #    print("\"Click me\" button was clicked")

    #def on_open_clicked(self, button):
    #    print("\"Open\" button was clicked")

    def on_close_clicked(self, button):
        print("Closing application")
        Gtk.main_quit()

if __name__ == '__main__':
    w = ViewInstalledWindow()
    #w.set_title("Keyman keyboard")
    w.connect("destroy", Gtk.main_quit)
    w.resize(800, 450)
    w.show_all()
Gtk.main()
