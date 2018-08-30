#!/usr/bin/python3

import gi
import subprocess
import webbrowser
import urllib.parse

import webbrowser
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit', '3.0')
from gi.repository import Gtk, WebKit
from keymanconfig.check_mime_type import check_mime_type
from keymanconfig.accelerators import bind_accelerator, init_accel

class WelcomeView(Gtk.Window):

    def __init__(self, welcomeurl, keyboardname):
        self.accelerators = None
        kbtitle = keyboardname + " installed"
        self.welcomeurl = welcomeurl
        Gtk.Window.__init__(self, title=kbtitle)
        init_accel(self)

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)

        s = Gtk.ScrolledWindow()
        #user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36"
        #user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36"
        self.webview = WebKit.WebView()
        #settings = WebKit.WebSettings()
        #settings.set_property('user-agent', user_agent)
        #webview.set_settings(settings)
        self.webview.connect("navigation-policy-decision-requested", self.check)
        self.webview.connect("mime-type-policy-decision-requested", check_mime_type)
        self.webview.load_uri(welcomeurl)
        #webview.load_uri("https://keyman.com/keyboards?embed=windows&version=10.0")
        #webview.load_uri("https://keyman.com/keyboards?embed=linux&version=11")
        s.add(self.webview)
        vbox.pack_start(s, True, True, 0)

        hbox = Gtk.Box(spacing=6)
        #hbox.set_halign(Gtk.Align.FILL)
        vbox.pack_start(hbox, False, False, 0)

        #button = Gtk.Button.new_with_label("Click Me")
        #button.connect("clicked", self.on_click_me_clicked)
        #hbox.pack_start(button, False, False, 0)

        button = Gtk.Button.new_with_mnemonic("Open in _Web browser")
        button.connect("clicked", self.on_openweb_clicked)
        button.set_tooltip_text("Open in the default web browser to do things like printing")
        hbox.pack_start(button, False, False, 0)

        button = Gtk.Button.new_with_mnemonic("_OK")
        button.connect("clicked", self.on_ok_clicked)
        hbox.pack_end(button, False, False, 0)
        bind_accelerator(self.accelerators, button, '<Control>w')

        self.add(vbox)

    def check(self, view, frame, req, nav, policy):
        uri = req.get_uri()
        if not "welcome.htm" in uri:
            webbrowser.open(uri)
            policy.ignore()
            return True
        return False

    #def on_click_me_clicked(self, button):
    #    print("\"Click me\" button was clicked")

    def on_openweb_clicked(self, button):
        print("\"Open in Web browser\" button was clicked")
        webbrowser.open(self.welcomeurl)

    def on_ok_clicked(self, button):
        print("Closing welcome window")
        self.close()