#!/usr/bin/python3

import gi
import subprocess
import webbrowser
import urllib.parse

gi.require_version('Gtk', '3.0')
gi.require_version('WebKit', '3.0')
from gi.repository import Gtk, WebKit

class WelcomeView(Gtk.Window):

    def __init__(self, welcomeurl, keyboardname):
        kbtitle = keyboardname + " installed"
        Gtk.Window.__init__(self, title=kbtitle)

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)

        s = Gtk.ScrolledWindow()
        #user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36"
        #user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36"
        webview = WebKit.WebView()
        #settings = WebKit.WebSettings()
        #settings.set_property('user-agent', user_agent)
        #webview.set_settings(settings)
        #webview.connect("navigation-policy-decision-requested", check)
        webview.connect("mime-type-policy-decision-requested", self.check_mime_type)
        webview.load_uri(welcomeurl)
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

        button = Gtk.Button.new_with_mnemonic("_Print")
        button.connect("clicked", self.on_print_clicked)
        hbox.pack_start(button, False, False, 0)

        button = Gtk.Button.new_with_mnemonic("_OK")
        button.connect("clicked", self.on_ok_clicked)
        hbox.pack_end(button, False, False, 0)

        self.add(vbox)

    def check_mime_type(self, webview, frame, request, mimetype, policy_decision):
        """Handle downloads and PDF files."""
        if mimetype == 'application/pdf':
            print("Download and run ", request.get_uri())
            parse_url = urllib.parse.urlparse(request.get_uri())
            if parse_url.scheme == "file":
                subprocess.call(['xdg-open', parse_url.path])
            else:
                webbrowser.open(request.get_uri())
            policy_decision.ignore()
            return True
        return False

    #def on_click_me_clicked(self, button):
    #    print("\"Click me\" button was clicked")

    def on_print_clicked(self, button):
        print("\"Print\" button was clicked")

    def on_ok_clicked(self, button):
        print("Closing welcome window")
        self.close()