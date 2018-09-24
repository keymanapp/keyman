#!/usr/bin/python3

import logging
import os.path
import pathlib
import gi
gi.require_version('Gtk', '3.0')
gi.require_version('Gdk', '3.0')
from gi.repository import Gtk, Gdk, GdkPixbuf
from keyman_config.list_installed_kmp import get_installed_kmp, InstallArea
from keyman_config.welcome import WelcomeView
from keyman_config.keyboard_details import KeyboardDetailsView
from keyman_config.downloadkeyboard import DownloadKmpWindow
from keyman_config.install_window import InstallKmpWindow, find_keyman_image
from keyman_config.uninstall_kmp import uninstall_kmp
from keyman_config.accelerators import bind_accelerator, init_accel
from keyman_config.get_kmp import user_keyboard_dir

class KeyboardBox(Gtk.Box):
    def __init__(self, kmp, window, area):
        Gtk.Box.__init__(self)
        self.parent = window

        self.kmp = kmp
        if area == InstallArea.IA_USER:
            self.kbhome = user_keyboard_dir(self.kmp["id"])
            self.kbdoc = self.kbhome
        elif area == InstallArea.IA_SHARED:
            self.kbhome = os.path.join("/usr/local/share/keyman", self.kmp["id"])
            self.kbdoc = os.path.join("/usr/local/share/doc/keyman", self.kmp["id"])
        elif area == InstallArea.IA_OS:
            self.kbhome = os.path.join("/usr/share/keyman", self.kmp["id"])
            self.kbdoc = os.path.join("/usr/share/doc/keyman", self.kmp["id"])

        icofile = os.path.join(self.kbhome, self.kmp["id"] + ".ico.png")
        if not os.path.isfile(icofile):
            icofile = find_keyman_image("icon_kmp.png")
        pixbuf = GdkPixbuf.Pixbuf.new_from_file_at_size(icofile, 16, 16)
        self.image = Gtk.Image.new_from_pixbuf(pixbuf)
        self.pack_start(self.image, False, False, 10)

        self.label1 = Gtk.Label()
        labeltext = kmp["name"] + " (" + kmp["version"] + ")"
        self.label1.set_text(labeltext)
        self.label1.set_halign(Gtk.Align.END)
        self.pack_start(self.label1, False, False, 10)

        img_expand = find_keyman_image("expand20.png")
        self.expandbutton = Gtk.Button()
        self.expandimage = Gtk.Image.new_from_file(img_expand)
        self.expandbutton.set_image(self.expandimage)
        self.expandbutton.set_tooltip_text("More information about " + kmp["name"])
        self.expandbutton.connect("clicked", self.on_expand_clicked)
        self.pack_end(self.expandbutton, False, False, 0)

        if area == InstallArea.IA_USER:
            img_cross = find_keyman_image("cross20.png")
            self.uninstallbutton = Gtk.Button()
            self.uninstallimage = Gtk.Image.new_from_file(img_cross)
            self.uninstallbutton.set_image(self.uninstallimage)
            self.uninstallbutton.set_tooltip_text("Uninstall " + kmp["name"] + " (" + kmp["id"] + ")")
            self.uninstallbutton.connect("clicked", self.on_uninstall_clicked)
            self.pack_end(self.uninstallbutton, False, False, 0)

        welcome_file = os.path.join(self.kbdoc, "welcome.htm")
        if os.path.isfile(welcome_file):
            img_help = find_keyman_image("help20.png")
            self.helpbutton = Gtk.Button()
            self.helpimage = Gtk.Image.new_from_file(img_help)
            self.helpbutton.set_image(self.helpimage)
            self.helpbutton.set_tooltip_text("Help for " + kmp["name"])
            self.helpbutton.connect("clicked", self.on_help_clicked)
            self.pack_end(self.helpbutton, False, False, 0)

    def on_help_clicked(self, button):
        logging.info("Open welcome.htm for" + self.kmp["name"] + "if available")
        welcome_file = os.path.join(self.kbdoc, "welcome.htm")
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
            # can only uninstall with the gui from user area
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
        self.set_column_homogeneous(True)

        prevbox = None
        shade = False
        user_kmp = get_installed_kmp(InstallArea.IA_USER)
        (prevbox, shade) = self.addboxes(user_kmp, window, prevbox, shade, InstallArea.IA_USER)
        shared_kmp = get_installed_kmp(InstallArea.IA_SHARED)
        (prevbox, shade) = self.addboxes(shared_kmp, window, prevbox, shade, InstallArea.IA_SHARED)
        os_kmp = get_installed_kmp(InstallArea.IA_OS)
        (prevbox, shade) = self.addboxes(os_kmp, window, prevbox, shade, InstallArea.IA_OS)

    def addboxes(self, installed_kmp, window, prevbox, shade, area):
        for kmp in sorted(installed_kmp):
            kbbox = KeyboardBox(installed_kmp[kmp], window, area)
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
        return (prevbox, shade)

class ViewInstalledWindow(Gtk.Window):
    def __init__(self):
        self.accelerators = None
        Gtk.Window.__init__(self, title="Keyman keyboard packages")
        init_accel(self)

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=0)

        self.s = Gtk.ScrolledWindow()
        vbox.pack_start(self.s, True, True, 12)

        hbox = Gtk.Box(spacing=6)
        vbox.pack_start(hbox, False, False, 12)

        self.grid = KmpGrid(self)
        self.s.add(self.grid)

        bbox = Gtk.ButtonBox(spacing=12, orientation=Gtk.Orientation.HORIZONTAL)

        button = Gtk.Button.new_with_mnemonic("_Refresh")
        button.connect("clicked", self.on_refresh_clicked)
        bbox.add(button)
        # hbox.pack_start(button, False, False, 12)

        button = Gtk.Button.new_with_mnemonic("_Download keyboard...")
        button.connect("clicked", self.on_download_clicked)
        bbox.add(button)
        # hbox.pack_start(button, False, False, 12)

        button = Gtk.Button.new_with_mnemonic("_Install keyboard...")
        button.connect("clicked", self.on_installfile_clicked)
        bbox.add(button)
        # hbox.pack_start(button, False, False, 5)

        hbox.pack_start(bbox, False, False, 12)

        bbox2 = Gtk.ButtonBox(spacing=12, orientation=Gtk.Orientation.HORIZONTAL)
        bbox2.set_layout(Gtk.ButtonBoxStyle.END)

        button = Gtk.Button.new_with_mnemonic("_Close")
        button.connect("clicked", self.on_close_clicked)
        bbox2.pack_end(button, False, False, 12)
        hbox.pack_end(bbox2, False, False, 12)


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
