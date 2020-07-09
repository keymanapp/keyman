#!/usr/bin/python3

import logging
import os.path
import urllib.parse
import webbrowser

import gi
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit2', '4.0')

from gi.repository import Gtk, WebKit2
from keyman_config.get_kmp import get_download_folder, download_kmp_file
from keyman_config.install_window import InstallKmpWindow
from keyman_config.accelerators import init_accel
from keyman_config.get_info import GetInfo
from keyman_config import __releaseversion__


class DownloadKmpWindow(Gtk.Dialog):

    def __init__(self, parent=None):
        self.accelerators = None
        Gtk.Dialog.__init__(self, "Download Keyman keyboards", parent)
        self.parentWindow = parent
        self.downloadfile = None
        init_accel(self)

        s = Gtk.ScrolledWindow()
        self.webview = WebKit2.WebView()
        self.webview.connect("decide-policy", self.keyman_policy)
        self.webview.load_uri("https://keyman.com/go/linux/" + __releaseversion__ + "/download-keyboards")
        s.add(self.webview)

        self.get_content_area().pack_start(s, True, True, 0)

        self.add_button("_Close", Gtk.ResponseType.CLOSE)
        self.getinfo = GetInfo(self.parentWindow.incomplete_kmp)

        self.resize(800, 450)
        self.show_all()

    def process_kmp(self, url, downloadfile):
        logging.info("Downloading kmp file to %s", downloadfile)
        if download_kmp_file(url, downloadfile):
            logging.info("File downloaded")
            self.downloadfile = downloadfile
            self.response(Gtk.ResponseType.OK)
            self.close()
            return True
        return False

    def keyman_policy(self, web_view, decision, decision_type):
        logging.info("Checking policy")
        logging.debug("received policy decision request of type: {0}".format(decision_type.value_name))
        if decision_type == WebKit2.PolicyDecisionType.NAVIGATION_ACTION:
            nav_action = decision.get_navigation_action()
            request = nav_action.get_request()
            uri = request.get_uri()
            logging.debug("nav request is for uri %s", uri)
            parsed = urllib.parse.urlparse(uri)
            if parsed.scheme == "keyman":
                logging.debug("using keyman scheme")
                if parsed.path == "download":
                    qs = urllib.parse.parse_qs(parsed.query)
                    downloadfile = os.path.join(get_download_folder(), qs['filename'][0])
                    if self.process_kmp(qs['url'][0], downloadfile):
                        decision.ignore()
                        return True
                elif parsed.path == "link":
                    qs = urllib.parse.parse_qs(parsed.query)
                    webbrowser.open(qs['url'][0])
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
