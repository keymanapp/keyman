#!/usr/bin/python3

import logging
import os
import urllib.parse

import gi

gi.require_version('Gtk', '3.0')
try:
    gi.require_version('WebKit2', '4.1')
except ValueError:
    # TODO: Remove once we drop support for Ubuntu 20.04 Focal
    gi.require_version('WebKit2', '4.0')

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
        url = KeymanComUrl + "/go/linux/" + __releaseversion__ + "/download-keyboards"
        self.webview.load_uri(url)
        s.add(self.webview)

        self.get_content_area().pack_start(s, True, True, 0)

        self.add_button(_("_Close"), Gtk.ResponseType.CLOSE)

        if self.parentWindow is not None:
            self.getinfo = GetInfo(self.parentWindow.incomplete_kmp)

        self.resize(800, 450)
        self.show_all()

    def _process_kmp(self, url, downloadfile: str):
        logging.info("Downloading kmp file to %s", downloadfile)
        if download_kmp_file(url, downloadfile):
            logging.info("File downloaded")
            self.downloadfile = downloadfile
            self.response(Gtk.ResponseType.OK)
            self.close()
            return True
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
