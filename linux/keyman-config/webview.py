#!/usr/bin/python3

import os
import webbrowser
import urllib.parse
import pathlib
import gi
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit', '3.0')
from gi.repository import Gtk, WebKit
from get_kmp import get_download_folder, download_kmp_file
from install_kmp import install_kmp

def process_kmp(view, url, downloadfile, verbose=False):
    if verbose:
        print("Downloading file to", downloadfile)
    if download_kmp_file(url, downloadfile, True):
        if verbose:
            print("File downloaded")
        install_kmp(downloadfile, True)
        keyboardid = os.path.basename(os.path.splitext(downloadfile)[0])
        welcome_file = os.path.join("/usr/local/share/doc/keyman", keyboardid, "welcome.htm")
        if os.path.isfile(welcome_file):
            uri_path = pathlib.Path(welcome_file).as_uri()
            view.load_uri(uri_path)
        return True
    return False

def check(view, frame, req, nav, policy):
    uri = req.get_uri()
    parsed = urllib.parse.urlparse(uri)
    if parsed.scheme == "keyman":
        if parsed.path == "download":
            qs = urllib.parse.parse_qs(parsed.query)
            downloadfile = os.path.join(get_download_folder(), qs['filename'][0])
            if process_kmp(view, qs['url'][0], downloadfile, True):
                policy.ignore()
                return True
        elif parsed.path == "link":
            qs = urllib.parse.parse_qs(parsed.query)
            webbrowser.open(qs['url'][0])
            return True
    return False


if __name__ == '__main__':
    w = Gtk.Window()
    w.set_title("Keyman keyboard")
    w.connect("destroy", Gtk.main_quit)
    w.resize(800, 800)
    s = Gtk.ScrolledWindow()
    user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36"
    #user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36"
    webview = WebKit.WebView()
    settings = WebKit.WebSettings()
    settings.set_property('user-agent', user_agent)
    webview.set_settings(settings)
    webview.connect("navigation-policy-decision-requested", check)
    webview.load_uri("https://keyman.com/keyboards?embed=macos&version=10")
    #webview.load_uri("https://keyman.com/keyboards?embed=windows&version=10.0")
    #webview.load_uri("https://keyman.com/keyboards?embed=linux&version=11")
    s.add(webview)
    w.add(s)
    w.show_all()
Gtk.main()