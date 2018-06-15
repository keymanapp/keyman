#!/usr/bin/python3

import os.path
import urllib.parse
import pathlib
import subprocess
import webbrowser
import gi
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit', '3.0')
from gi.repository import Gtk, WebKit
from get_kmp import get_download_folder, download_kmp_file
from install_window import InstallKmpWindow
from check_mime_type import check_mime_type
from accelerators import bind_accelerator, init_accel

class DownloadKmpWindow(Gtk.Window):

    def __init__(self, view=None):
        self.accelerators = None
        Gtk.Window.__init__(self, title="Keyman keyboard")
        self.endonclose = False
        self.viewwindow = view
        init_accel(self)

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)

        s = Gtk.ScrolledWindow()
        user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36"
        #user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36"
        webview = WebKit.WebView()
        settings = WebKit.WebSettings()
        settings.set_property('user-agent', user_agent)
        webview.set_settings(settings)
        webview.connect("navigation-policy-decision-requested", self.check)
        webview.connect("mime-type-policy-decision-requested", check_mime_type)
        webview.load_uri("https://keyman.com/keyboards?embed=macos&version=10")
        #webview.load_uri("https://keyman.com/keyboards?embed=windows&version=10.0")
        #webview.load_uri("https://keyman.com/keyboards?embed=linux&version=11")
        s.add(webview)
        vbox.pack_start(s, True, True, 0)

        hbox = Gtk.Box(spacing=6)
        #hbox.set_halign(Gtk.Align.FILL)
        vbox.pack_start(hbox, False, False, 0)

        #button = Gtk.Button.new_with_label("Click Me")
        #button.connect("clicked", self.on_click_me_clicked)
        #hbox.pack_start(button, False, False, 0)

        #button = Gtk.Button.new_with_mnemonic("_Open")
        #button.connect("clicked", self.on_open_clicked)
        #hbox.pack_start(button, False, False, 0)

        self.statuslabel = Gtk.Label()
        hbox.pack_start(self.statuslabel, False, False, 0)

        button = Gtk.Button.new_with_mnemonic("_Close")
        button.connect("clicked", self.on_close_clicked)
        hbox.pack_end(button, False, False, 0)
        bind_accelerator(self.accelerators, button, '<Control>w')

        self.add(vbox)

    def process_kmp(self, view, url, downloadfile, verbose=False):
        if verbose:
            print("Downloading file to", downloadfile)
        if download_kmp_file(url, downloadfile, True):
            if verbose:
                print("File downloaded")
            #self.statuslabel.set_text("Package downloaded to " + downloadfile)

            w = InstallKmpWindow(downloadfile, online=True, viewkmp=self.viewwindow)
            if w.checkcontinue:
                w.show_all()
            #install_kmp(downloadfile, True)
            #keyboardid = os.path.basename(os.path.splitext(downloadfile)[0])
            #welcome_file = os.path.join("/usr/local/share/doc/keyman", keyboardid, "welcome.htm")
            #if os.path.isfile(welcome_file):
            #    uri_path = pathlib.Path(welcome_file).as_uri()
            #    view.load_uri(uri_path)
            return True
        return False

    def check(self, view, frame, req, nav, policy):
        uri = req.get_uri()
        parsed = urllib.parse.urlparse(uri)
        if parsed.scheme == "keyman":
            if parsed.path == "download":
                qs = urllib.parse.parse_qs(parsed.query)
                downloadfile = os.path.join(get_download_folder(), qs['filename'][0])
                #self.statuslabel.set_text("Downloading package to " + downloadfile)
                #self.show_all()
                if self.process_kmp(view, qs['url'][0], downloadfile, True):
                    policy.ignore()
                    return True
            elif parsed.path == "link":
                qs = urllib.parse.parse_qs(parsed.query)
                webbrowser.open(qs['url'][0])
                return True
        return False


    #def on_click_me_clicked(self, button):
    #    print("\"Click me\" button was clicked")

    #def on_open_clicked(self, button):
    #    print("\"Open\" button was clicked")

    def on_close_clicked(self, button):
        print("Closing download window")
        if self.endonclose:
            Gtk.main_quit()
        else:
            self.close()

    def connectdestroy(self):
        self.connect("destroy", Gtk.main_quit)
        self.endonclose = True

if __name__ == '__main__':
    w = DownloadKmpWindow()
    w.connectdestroy()
    w.resize(800, 450)
    w.show_all()
    Gtk.main()