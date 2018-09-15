#!/usr/bin/python3

import logging
import os.path
import pathlib
import gi
gi.require_version('Gtk', '3.0')
gi.require_version('Gdk', '3.0')
from gi.repository import Gtk, Gdk
from keyman_config.list_installed_kmp import get_installed_kmp
from keyman_config.welcome import WelcomeView
from keyman_config.keyboard_details import KeyboardDetailsView
from keyman_config.downloadkeyboard import DownloadKmpWindow
from keyman_config.install_window import InstallKmpWindow
from keyman_config.uninstall_kmp import uninstall_kmp
from keyman_config.accelerators import bind_accelerator, init_accel

class KeyboardBox(Gtk.Box):
    def __init__(self, kmp, window):
        Gtk.Box.__init__(self)
        self.parent = window

        self.kmp = kmp
        icofile = os.path.join("/usr/local/share/keyman", self.kmp["id"], self.kmp["id"] + ".ico.bmp")
        if not os.path.isfile(icofile):
            icofile = "/usr/share/keyman/icons/icon_kmp.png"
        if not os.path.isfile(icofile):
            icofile = "keyman_config/icons/icon_kmp.png"
        if not os.path.isfile(icofile):
            icofile = "icon_kmp.png"
        if not os.path.isfile(icofile):
            icofile = "icons/icon_kmp.png"
        self.image = Gtk.Image.new_from_file(icofile)
        self.pack_start(self.image, False, False, 10)

        self.label1 = Gtk.Label()
        labeltext = kmp["name"] + " (" + kmp["version"] + ")"
        self.label1.set_text(labeltext)
        self.label1.set_halign(Gtk.Align.END)
        self.pack_start(self.label1, False, False, 10)

        img_expand = "/usr/share/keyman/icons/expand20.png"
        if not os.path.isfile(img_expand):
            img_expand = "keyman_config/icons/expand20.png"
        if not os.path.isfile(img_expand):
            img_expand = "expand20.png"
        if not os.path.isfile(img_expand):
            img_expand = "icons/expand20.png"
        self.expandbutton = Gtk.Button()
        self.expandimage = Gtk.Image.new_from_file(img_expand)
        self.expandbutton.set_image(self.expandimage)
        self.expandbutton.set_tooltip_text("More information about " + kmp["name"])
        self.expandbutton.connect("clicked", self.on_expand_clicked)
        self.pack_end(self.expandbutton, False, False, 0)

        img_cross = "/usr/share/keyman/icons/cross20.png"
        if not os.path.isfile(img_cross):
            img_cross = "keyman_config/icons/cross20.png"
        if not os.path.isfile(img_cross):
            img_cross = "cross20.png"
        if not os.path.isfile(img_cross):
            img_cross = "icons/cross20.png"
        self.uninstallbutton = Gtk.Button()
        self.uninstallimage = Gtk.Image.new_from_file(img_cross)
        self.uninstallbutton.set_image(self.uninstallimage)
        self.uninstallbutton.set_tooltip_text("Uninstall " + kmp["name"] + " (" + kmp["id"] + ")")
        self.uninstallbutton.connect("clicked", self.on_uninstall_clicked)
        self.pack_end(self.uninstallbutton, False, False, 0)

        welcome_file = os.path.join("/usr/local/share/doc/keyman", self.kmp["id"], "welcome.htm")
        if os.path.isfile(welcome_file):
            img_help = "/usr/share/keyman/icons/help20.png"
            if not os.path.isfile(img_help):
                img_help = "keyman_config/icons/help20.png"
            if not os.path.isfile(img_help):
                img_help = "help20.png"
            if not os.path.isfile(img_help):
                img_help = "icons/help20.png"
            self.helpbutton = Gtk.Button()
            self.helpimage = Gtk.Image.new_from_file(img_help)
            self.helpbutton.set_image(self.helpimage)
            self.helpbutton.set_tooltip_text("Help for " + kmp["name"])
            self.helpbutton.connect("clicked", self.on_help_clicked)
            self.pack_end(self.helpbutton, False, False, 0)

    def on_help_clicked(self, button):
        logging.info("Open welcome.htm for" + self.kmp["name"] + "if available")
        welcome_file = os.path.join("/usr/local/share/doc/keyman", self.kmp["id"], "welcome.htm")
        if os.path.isfile(welcome_file):
            uri_path = pathlib.Path(welcome_file).as_uri()
            logging.info("opening" + uri_path)
            w = WelcomeView(uri_path, self.kmp["id"])
            w.resize(800, 600)
            w.show_all()
        else:
            logging.info("welcome.htm not available")

    def on_uninstall_clicked(self, button):
        logging.info("Uninstall keyboard " +  self.kmp["id"] + "?")
        dialog = Gtk.MessageDialog(self.parent, 0, Gtk.MessageType.QUESTION,
            Gtk.ButtonsType.YES_NO, "Uninstall keyboard?")
        dialog.format_secondary_text(
            "Are you sure that you want to uninstall the " + self.kmp["name"] + " keyboard")
        response = dialog.run()
        dialog.destroy()
        if response == Gtk.ResponseType.YES:
            logging.info("Uninstalling keyboard" + self.kmp["name"])
            uninstall_kmp(self.kmp["id"])
            logging.info("need to refresh window after uninstalling a keyboard")
            self.parent.refresh_installed_kmp()
        elif response == Gtk.ResponseType.NO:
            logging.info("Not uninstalling keyboard " + self.kmp["name"])

    def on_expand_clicked(self, button):
        logging.info("Show keyboard details of " + self.kmp["name"])
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
        Gtk.Window.__init__(self, title="Keyman keyboard packages")
        init_accel(self)

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)

        self.s = Gtk.ScrolledWindow()
        vbox.pack_start(self.s, True, True, 0)

        hbox = Gtk.Box(spacing=6)
        vbox.pack_start(hbox, False, False, 0)

        self.grid = KmpGrid(self)
        self.s.add(self.grid)

        button = Gtk.Button.new_with_mnemonic("_Refresh")
        button.connect("clicked", self.on_refresh_clicked)
        hbox.pack_start(button, False, False, 0)

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
        logging.debug("Refreshing grid")
        self.s.remove(self.s.get_child())
        self.grid = KmpGrid(self)
        self.s.add_with_viewport(self.grid)
        self.s.show_all()

    def on_close_clicked(self, button):
        logging.debug("Close application clicked")
        Gtk.main_quit()

    def on_refresh_clicked(self, button):
        logging.debug("Refresh application clicked")
        self.refresh_installed_kmp()

    def on_download_clicked(self, button):
        logging.debug("Download clicked")
        w = DownloadKmpWindow(self)
        w.resize(800, 450)
        w.show_all()

    def on_installfile_clicked(self, button):
        logging.debug("Install from file clicked")
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
