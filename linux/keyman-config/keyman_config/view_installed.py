#!/usr/bin/python3

import logging
import os
import subprocess
import sys

import gi


gi.require_version('Gtk', '3.0')
gi.require_version('Gdk', '3.0')

from gi.overrides.GLib import GError
from gi.repository import GdkPixbuf, Gtk

from keyman_config import _
from keyman_config.accelerators import bind_accelerator, init_accel
from keyman_config.dbus_util import get_keyman_config_service
from keyman_config.downloadkeyboard import DownloadKmpWindow
from keyman_config.fcitx_util import is_fcitx_running
from keyman_config.get_kmp import (InstallLocation, get_keyboard_dir,
                                   get_install_area_string)
from keyman_config.ibus_util import IbusDaemon, verify_ibus_daemon
from keyman_config.install_window import InstallKmpWindow, find_keyman_image
from keyman_config.keyboard_layouts_model import create_kbd_layouts_model
from keyman_config.keyboard_layouts_widget import KeyboardLayoutsWidget
from keyman_config.list_installed_kmp import get_installed_kmp
from keyman_config.options_widget import OptionsWidget
from keyman_config.sentry_handling import SentryErrorHandling


class ViewInstalledWindowBase(Gtk.Window):
    def __init__(self):
        self.accelerators = None
        super().__init__(title=_("Keyman Configuration"))
        init_accel(self)
        self._config_service = get_keyman_config_service(self.refresh_installed_kmp)

    def refresh_installed_kmp(self):
        pass

    def on_close_clicked(self, button):
        logging.debug("Close application clicked")
        self.close()

    def on_refresh_clicked(self, button):
        logging.debug("Refresh application clicked")
        self.refresh_installed_kmp()

    def on_download_clicked(self, button):
        logging.debug("Download clicked")
        downloadDlg = DownloadKmpWindow(self)
        response = downloadDlg.run()
        if response != Gtk.ResponseType.OK:
            downloadDlg.destroy()
            return

        file = downloadDlg.downloadfile
        language = downloadDlg.language
        downloadDlg.destroy()
        self.restart(self.install_file(file, language))

    def on_installfile_clicked(self, button):
        logging.debug("Install from file clicked")
        dlg = Gtk.FileChooserDialog(
          _("Choose a kmp file..."), self, Gtk.FileChooserAction.OPEN,
          (Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL, Gtk.STOCK_OPEN, Gtk.ResponseType.OK))
        dlg.resize(640, 480)
        filter_text = Gtk.FileFilter()
        # i18n: file type in file selection dialog
        filter_text.set_name(_("KMP files"))
        filter_text.add_pattern("*.kmp")
        dlg.add_filter(filter_text)
        response = dlg.run()
        if response != Gtk.ResponseType.OK:
            dlg.destroy()
            return

        file = dlg.get_filename()
        dlg.destroy()
        self.restart(self.install_file(file))

    def install_file(self, kmpfile, language=None):
        installDlg = InstallKmpWindow(kmpfile, viewkmp=self, language=language)
        if installDlg.is_error:
            return Gtk.ResponseType.CANCEL
        result = installDlg.run()
        installDlg.destroy()
        return result

    def restart(self, response=Gtk.ResponseType.OK):
        if response != Gtk.ResponseType.CANCEL:
            subprocess.Popen(sys.argv)
            self.close()

    def run(self):
        self.resize(776, 424)
        self.connect("destroy", Gtk.main_quit)
        self.show_all()

        if (not is_fcitx_running()) and (verify_ibus_daemon(False) != IbusDaemon.RUNNING):
            dialog = Gtk.MessageDialog(
                self, 0, Gtk.MessageType.WARNING,
                Gtk.ButtonsType.YES_NO, _("ibus-daemon is not running. Try to start ibus-daemon now?"))
            dialog.format_secondary_text(
                _("If you just recently installed Keyman you might have to reboot or logout and login again."))
            response = dialog.run()
            dialog.destroy()
            if response == Gtk.ResponseType.YES:
                if verify_ibus_daemon(True) != IbusDaemon.RUNNING:
                    dialog = Gtk.MessageDialog(
                        self, 0, Gtk.MessageType.WARNING,
                        Gtk.ButtonsType.OK,
                        _("Unable to start ibus-daemon. Please reboot or logout and login again."))
                    dialog.run()
                    dialog.destroy()
                    self.close()
            else:
                self.close()
        Gtk.main()


class ViewInstalledWindow(ViewInstalledWindowBase):
    def __init__(self):
        super().__init__()

        self.sentry = SentryErrorHandling()

        outmostVBox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL)
        outmostVBox.pack_start(self._add_stack_sidebar(), True, True, 0)
        outmostVBox.pack_end(self._add_app_buttons(), False, True, 12)
        self.add(outmostVBox)

    def _add_stack_sidebar(self):
        outerHbox = Gtk.HBox()
        sidebar = Gtk.StackSidebar()
        stack = Gtk.Stack()

        outerHbox.pack_start(sidebar, False, False, 0)
        outerHbox.pack_end(Gtk.Separator(), False, False, 0)
        outerHbox.pack_end(stack, True, True, 0)
        stack.set_hexpand(True)
        stack.set_vexpand(True)
        sidebar.set_stack(stack)

        self.store = create_kbd_layouts_model()
        self.refresh_installed_kmp()
        stack.add_titled(KeyboardLayoutsWidget(self, self.store, self.restart), "KeyboardLayouts", _("Keyboard Layouts"))
        stack.add_titled(OptionsWidget(self.sentry), "Options", _("Options"))
        return outerHbox

    def _add_app_buttons(self):
        bbox_bottom = Gtk.ButtonBox(spacing=12, orientation=Gtk.Orientation.HORIZONTAL)
        bbox_bottom.set_layout(Gtk.ButtonBoxStyle.START)

        button = Gtk.Button.new_with_mnemonic(_("_Install keyboard..."))
        button.set_tooltip_text(_("Install a keyboard from a file"))
        button.connect("clicked", self.on_installfile_clicked)
        bbox_bottom.add(button)

        button = Gtk.Button.new_with_mnemonic(_("_Download keyboard..."))
        button.set_tooltip_text(_("Download and install a keyboard from the Keyman website"))
        button.connect("clicked", self.on_download_clicked)
        bbox_bottom.add(button)

        button = Gtk.Button.new_with_mnemonic(_("_Refresh"))
        button.set_tooltip_text(_("Refresh keyboard list"))
        button.connect("clicked", self.on_refresh_clicked)
        bbox_bottom.add(button)

        button = Gtk.Button.new_with_mnemonic(_("_Close"))
        button.set_tooltip_text(_("Close window"))
        button.connect("clicked", self.on_close_clicked)
        bind_accelerator(self.accelerators, button, '<Control>q')
        bind_accelerator(self.accelerators, button, '<Control>w')
        bbox_bottom.add(button)

        bbox_hbox = Gtk.HBox(spacing=12)
        bbox_hbox.pack_start(bbox_bottom, True, True, 12)
        return bbox_hbox

    def _addlistitems(self, installed_kmp, store, install_area):
        bmppng = ".bmp.png"  # Icon file extension

        for kmp in sorted(installed_kmp):
            kmpdata = installed_kmp[kmp]
            path = get_keyboard_dir(install_area, kmpdata['packageID'])

            welcome_file = os.path.join(path, "welcome.htm")
            options_file = os.path.join(path, "options.htm")
            icofile_name = os.path.join(path, kmpdata['packageID'] + bmppng)

            if not os.path.isfile(welcome_file):
                welcome_file = None
            if not os.path.isfile(options_file):
                options_file = None
            if not os.path.isfile(icofile_name):
                icofile_name = os.path.join(path, kmpdata['keyboardID'] + bmppng)
                if not os.path.isfile(icofile_name):
                    icofile_name = find_keyman_image("icon_kmp.png")

            try:
                icofile = GdkPixbuf.Pixbuf.new_from_file_at_size(icofile_name, 16, 16)
            except GError:
                _, value, _ = sys.exc_info()
                logging.info(f"Error reading icon file {icofile_name}: {value.message}")
                icofile = None

            store.append([
              icofile,
              kmpdata['name'],
              kmpdata['kmpversion'],
              kmpdata['packageID'],
              install_area,
              get_install_area_string(install_area),
              path.replace(os.path.expanduser('~'), '~'),
              welcome_file,
              options_file])

    def refresh_installed_kmp(self):
        logging.debug("Refreshing listview")
        self.store.clear()
        self.incomplete_kmp = []
        user_kmp = get_installed_kmp(InstallLocation.User)
        for kmp in sorted(user_kmp):
            kmpdata = user_kmp[kmp]
            if kmpdata["has_kbjson"] is False:
                self.incomplete_kmp.append(kmpdata)
        self._addlistitems(user_kmp, self.store, InstallLocation.User)
        shared_kmp = get_installed_kmp(InstallLocation.Shared)
        for kmp in sorted(shared_kmp):
            kmpdata = shared_kmp[kmp]
            if kmpdata["has_kbjson"] is False:
                self.incomplete_kmp.append(kmpdata)
        self._addlistitems(shared_kmp, self.store, InstallLocation.Shared)
        os_kmp = get_installed_kmp(InstallLocation.OS)
        for kmp in sorted(os_kmp):
            kmpdata = os_kmp[kmp]
            if kmpdata["has_kbjson"] is False:
                self.incomplete_kmp.append(kmpdata)
        self._addlistitems(os_kmp, self.store, InstallLocation.OS)


if __name__ == '__main__':
    w = ViewInstalledWindow()
    w.connect("destroy", Gtk.main_quit)
    w.resize(576, 324)
    w.show_all()
    Gtk.main()
