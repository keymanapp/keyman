#!/usr/bin/python3

import gi
import logging
import subprocess
import webbrowser
import urllib.parse

import webbrowser
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit', '3.0')
from gi.repository import Gtk, WebKit
from keyman_config.check_mime_type import check_mime_type
from keyman_config.accelerators import bind_accelerator, init_accel

class WelcomeView(Gtk.Window):

    def __init__(self, welcomeurl, keyboardname):
        self.accelerators = None
        kbtitle = keyboardname + " installed"
        self.welcomeurl = welcomeurl
        Gtk.Window.__init__(self, title=kbtitle)
        init_accel(self)

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)

        s = Gtk.ScrolledWindow()
        self.webview = WebKit.WebView()
        self.webview.connect("navigation-policy-decision-requested", self.check)
        self.webview.connect("mime-type-policy-decision-requested", check_mime_type)
        self.webview.load_uri(welcomeurl)
        s.add(self.webview)
        vbox.pack_start(s, True, True, 0)

        hbox = Gtk.Box(spacing=12)
        vbox.pack_start(hbox, False, False, 6)

        button = Gtk.Button.new_with_mnemonic("Open in _Web browser")
        button.connect("clicked", self.on_openweb_clicked)
        button.set_tooltip_text("Open in the default web browser to do things like printing")
        hbox.pack_start(button, False, False, 12)

        button = Gtk.Button.new_with_mnemonic("_OK")
        button.connect("clicked", self.on_ok_clicked)
        hbox.pack_end(button, False, False, 12)
        bind_accelerator(self.accelerators, button, '<Control>w')

        self.add(vbox)

    def check(self, view, frame, req, nav, policy):
        uri = req.get_uri()
        if not "welcome.htm" in uri:
            webbrowser.open(uri)
            policy.ignore()
            return True
        return False

    def on_openweb_clicked(self, button):
        logging.info("\"Open in Web browser\" button was clicked")
        webbrowser.open(self.welcomeurl)

    def on_ok_clicked(self, button):
        logging.info("Closing welcome window")
        self.close()
