#!/usr/bin/python3

import os.path
import pathlib
import gi
gi.require_version('Gtk', '3.0')
gi.require_version('Gdk', '3.0')
from gi.repository import Gtk, Gdk
from list_installed_kmp import get_installed_kmp
from welcome import WelcomeView
from keyboard_details import KeyboardDetailsView
from downloadkeyboard import DownloadKmpWindow
from install_window import InstallKmpWindow
from uninstall_kmp import uninstall_kmp
from accelerators import bind_accelerator, init_accel

class KeyboardBox(Gtk.Box):
    def __init__(self, kmp, window):
        Gtk.Box.__init__(self)
        self.parent = window

        #print(kmp)
        self.kmp = kmp
        icofile = os.path.join("/usr/local/share/keyman", self.kmp["id"], self.kmp["id"] + ".ico.bmp")
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
        self.uninstallbutton.set_tooltip_text("Uninstall " + kmp["name"] + " (" + kmp["id"] + ")")
        #grid.attach_next_to(uninstallbutton, expandbutton, Gtk.PositionType.RIGHT, 1, 1)
        self.uninstallbutton.connect("clicked", self.on_uninstall_clicked)
        self.pack_end(self.uninstallbutton, False, False, 0)

        welcome_file = os.path.join("/usr/local/share/doc/keyman", self.kmp["id"], "welcome.htm")
        if os.path.isfile(welcome_file):
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
            "Are you sure that you want to uninstall the " + self.kmp["name"] + " keyboard")
        response = dialog.run()
        dialog.destroy()
        if response == Gtk.ResponseType.YES:
            print("Uninstalling keyboard", self.kmp["name"])
            uninstall_kmp(self.kmp["id"])
            print("need to refresh window after uninstalling a keyboard")
            self.parent.refresh_installed_kmp()
        elif response == Gtk.ResponseType.NO:
            print("Not uninstalling keyboard", self.kmp["name"])

    def on_expand_clicked(self, button):
        print("Show keyboard details of", self.kmp["name"])
        w = KeyboardDetailsView(self.kmp)
        w.resize(800, 450)
        w.show_all()

class KmpGrid(Gtk.Grid):
    def __init__(self, window):
        Gtk.Grid.__init__(self)
        installed_kmp = get_installed_kmp()
        self.set_column_homogeneous(True)

        prevbox = None
        shade = False
        for kmp in sorted(installed_kmp):
            #print(kmp)
            kbbox = KeyboardBox(installed_kmp[kmp], window)
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
        self.accelerators = None
        Gtk.Window.__init__(self, title="Keyman keyboard")
        init_accel(self)

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)

        self.s = Gtk.ScrolledWindow()
        vbox.pack_start(self.s, True, True, 0)

        hbox = Gtk.Box(spacing=6)
        #hbox.set_halign(Gtk.Align.FILL)
        vbox.pack_start(hbox, False, False, 0)

        self.grid = KmpGrid(self)
        self.s.add(self.grid)

        #button = Gtk.Button.new_with_label("Click Me")
        #button.connect("clicked", self.on_click_me_clicked)
        #hbox.pack_start(button, False, False, 0)

        button = Gtk.Button.new_with_mnemonic("_Download keyboard...")
        button.connect("clicked", self.on_download_clicked)
        hbox.pack_start(button, False, False, 0)

        button = Gtk.Button.new_with_mnemonic("_Install keyboard...")
        button.connect("clicked", self.on_installfile_clicked)
        hbox.pack_start(button, False, False, 0)

        button = Gtk.Button.new_with_mnemonic("_Close")
        button.connect("clicked", self.on_close_clicked)
        hbox.pack_end(button, False, False, 0)
        bind_accelerator(self.accelerators, button, '<Control>q')
        bind_accelerator(self.accelerators, button, '<Control>w')

        self.add(vbox)

    def refresh_installed_kmp(self):
        print("Refreshing grid")
        self.s.remove(self.s.get_child())
        self.grid = KmpGrid(self)
        self.s.add_with_viewport(self.grid)
        self.s.show_all()


    #def on_click_me_clicked(self, button):
    #    print("\"Click me\" button was clicked")

    #def on_open_clicked(self, button):
    #    print("\"Open\" button was clicked")

    def on_close_clicked(self, button):
        print("Closing application")
        Gtk.main_quit()

    def on_refresh_clicked(self, button):
        print("Refreshing application")
        self.refresh_installed_kmp()

    def on_download_clicked(self, button):
        print("Download")
        w = DownloadKmpWindow(self)
        w.resize(800, 450)
        w.show_all()

    def on_installfile_clicked(self, button):
        print("Install from file")
        dlg = Gtk.FileChooserDialog("Choose a kmp file..", self, Gtk.FileChooserAction.OPEN,
            (Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL, Gtk.STOCK_OPEN, Gtk.ResponseType.OK))
        filter_text = Gtk.FileFilter()
        filter_text.set_name("KMP files")
        filter_text.add_pattern("*.kmp")
        dlg.add_filter(filter_text)
        response = dlg.run()
        if response == Gtk.ResponseType.OK:
            kmpfile = dlg.get_filename()
            w = InstallKmpWindow(kmpfile, viewkmp=self)
            w.resize(800, 450)
            if w.checkcontinue:
                w.show_all()
            else:
                w.destroy()
        dlg.destroy()

if __name__ == '__main__':
    w = ViewInstalledWindow()
    w.connect("destroy", Gtk.main_quit)
    w.resize(800, 450)
    w.show_all()
    Gtk.main()
