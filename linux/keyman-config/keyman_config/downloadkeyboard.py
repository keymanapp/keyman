#!/usr/bin/python3

import logging
import os.path
import urllib.parse
import pathlib
import subprocess
import webbrowser
import gi
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit2', '4.0')
from gi.repository import Gtk, WebKit2
from keyman_config.get_kmp import get_download_folder, download_kmp_file
from keyman_config.install_window import InstallKmpWindow
from keyman_config.accelerators import bind_accelerator, init_accel

class DownloadKmpWindow(Gtk.Window):

    def __init__(self, view=None):
        self.accelerators = None
        Gtk.Window.__init__(self, title="Download Keyman keyboards")
        self.endonclose = False
        self.viewwindow = view
        init_accel(self)

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL)

        s = Gtk.ScrolledWindow()
        # TODO update (or remove) user_agent once website supports Linux kmp packages
        user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36"
        #user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36"
        webview = WebKit2.WebView()
        settings = WebKit2.Settings()
        settings.set_user_agent(user_agent)
        webview.set_settings(settings)
        webview.connect("decide-policy", self.keyman_policy)
        # TODO update website URI once website supports Linux kmp packages
        webview.load_uri("https://keyman.com/keyboards?embed=macos&version=10")
        s.add(webview)
        vbox.pack_start(s, True, True, 0)

        bbox = Gtk.ButtonBox(spacing=12, orientation=Gtk.Orientation.HORIZONTAL)
        #bbox.set_layout(Gtk.ButtonBoxStyle.END)

        button = Gtk.Button.new_with_mnemonic("_Close")
        button.connect("clicked", self.on_close_clicked)
        bbox.pack_end(button, False, False, 12)
        bind_accelerator(self.accelerators, button, '<Control>w')
        vbox.pack_start(bbox, False, False, 12)

        self.add(vbox)

    def process_kmp(self, url, downloadfile):
        logging.info("Downloading kmp file to %s", downloadfile)
        if download_kmp_file(url, downloadfile):
            logging.info("File downloaded")
            w = InstallKmpWindow(downloadfile, online=True, viewkmp=self.viewwindow, downloadwindow=self)
            if w.checkcontinue:
                w.show_all()
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

    def on_close_clicked(self, button):
        logging.debug("Closing download window")
        if self.endonclose:
            Gtk.main_quit()
        else:
            self.close()

    def connectdestroy(self):
        self.connect("destroy", Gtk.main_quit)
        self.endonclose = True

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    w = DownloadKmpWindow()
    w.connectdestroy()
    w.resize(800, 450)
    w.show_all()
    Gtk.main()
