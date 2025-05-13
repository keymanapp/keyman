#!/usr/bin/python3

import logging
import os
import urllib.parse

import gi

gi.require_version('Gtk', '3.0')
gi.require_version('WebKit2', '4.1')
from gi.repository import Gtk, WebKit2

from keyman_config import KeymanComUrl, _, __releaseversion__, __tier__
from keyman_config.accelerators import init_accel
from keyman_config.get_info import GetInfo
from keyman_config.get_kmp import download_kmp_file, get_download_folder
from keyman_config.install_window import InstallKmpWindow


class DownloadKmpWindow(Gtk.Dialog):

    def __init__(self, parent=None):
        self.accelerators = None
        Gtk.Dialog.__init__(self, _("Download Keyman keyboards"), parent)
        self.parentWindow = parent
        self.downloadfile = None
        init_accel(self)

        s = Gtk.ScrolledWindow()
        self.webview = WebKit2.WebView()
        self.webview.connect("decide-policy", self._keyman_policy)
        self.webview.connect("load-changed", self._update_back_button)
        url = KeymanComUrl + "/go/linux/" + __releaseversion__ + "/download-keyboards"
        self.webview.load_uri(url)
        s.add(self.webview)

        self.get_content_area().pack_start(s, True, True, 0)

        hbox = Gtk.Box(spacing=6)
        self.get_content_area().pack_start(hbox, False, False, 0)

        self.back_button = Gtk.Button.new_with_mnemonic(_("_Back"))
        self.back_button.set_tooltip_text(_("Back to search"))
        self.back_button.connect("clicked", self._on_back_clicked)
        self.back_button.set_sensitive(False)
        hbox.pack_start(self.back_button, False, False, 0)

        close_button = Gtk.Button.new_with_mnemonic(_("_Close"))
        close_button.set_tooltip_text(_("Close dialog"))
        close_button.connect("clicked", self._on_close_clicked)
        hbox.pack_end(close_button, False, False, 0)

        if self.parentWindow is not None:
            self.getinfo = GetInfo(self.parentWindow.incomplete_kmp)

        self.resize(800, 450)
        self.show_all()

    def _update_back_button(self, webview, load_event):
        self.back_button.set_sensitive(webview.can_go_back())

    def _on_back_clicked(self, button):
        self.webview.go_back()

    def _on_close_clicked(self, button):
        self.response(Gtk.ResponseType.CLOSE)

    def _process_kmp(self, url, downloadfile: str):
        logging.info("Downloading kmp file to %s", downloadfile)
        if download_kmp_file(url, downloadfile):
            logging.info("File downloaded")
            self.downloadfile = downloadfile
            self.response(Gtk.ResponseType.OK)
            self.close()
            return True
        logging.error(_("Downloading kmp file failed"))
        dialog = Gtk.MessageDialog(
                  self, 0, Gtk.MessageType.ERROR,
                  Gtk.ButtonsType.OK, _("Downloading keyboard file failed"))
        dialog.run()
        dialog.destroy()
        return False

    def _keyman_policy(self, web_view, decision, decision_type):
        logging.info("Checking policy")
        logging.debug("received policy decision request of type: {0}".format(decision_type.value_name))
        if decision_type == WebKit2.PolicyDecisionType.NAVIGATION_ACTION:
            nav_action = decision.get_navigation_action()
            request = nav_action.get_request()
            uri = request.get_uri()
            logging.debug("nav request is for uri %s", uri)
            parsed = urllib.parse.urlparse(uri)
            if parsed.path.startswith('/keyboards/install/'):
                qs = urllib.parse.parse_qs(parsed.query)
                package_id = parsed.path.split('/')[-1]
                downloadfile = os.path.join(get_download_folder(), package_id)
                download_url = KeymanComUrl + '/go/package/download/' + package_id + \
                    '?platform=linux&tier=' + __tier__
                if 'bcp47' in qs:
                    self.language = qs['bcp47'][0]
                    download_url += '&bcp47=' + qs['bcp47'][0]
                else:
                    self.language = None
                if self._process_kmp(download_url, downloadfile):
                    decision.ignore()
                    return True
        return False


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    w = DownloadKmpWindow()
    result = w.run()
    file = None
    if result == Gtk.ResponseType.OK:
        file = w.downloadfile
    w.destroy()

    if file is not None:
        installDlg = InstallKmpWindow(file)
        installDlg.run()
        installDlg.destroy()
