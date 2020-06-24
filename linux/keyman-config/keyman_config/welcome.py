#!/usr/bin/python3

import gi
import logging
import webbrowser

from gi.repository import Gtk, WebKit2
from keyman_config.accelerators import bind_accelerator, init_accel

gi.require_version('Gtk', '3.0')
gi.require_version('WebKit2', '4.0')

# NOTE: WebKit2 is not able to load XHTML files nor files with an encoding other
# than ASCII or UTF-8


class WelcomeView(Gtk.Dialog):

    def __init__(self, parent, welcomeurl, keyboardname):
        self.accelerators = None
        kbtitle = keyboardname + " installed"
        self.welcomeurl = welcomeurl
        Gtk.Dialog.__init__(self, kbtitle, parent)
        init_accel(self)

        s = Gtk.ScrolledWindow()
        self.webview = WebKit2.WebView()
        self.webview.connect("decide-policy", self.doc_policy)
        self.webview.load_uri(welcomeurl)
        s.add(self.webview)

        self.get_content_area().pack_start(s, True, True, 0)

        hbox = Gtk.Box(spacing=12)
        self.get_content_area().pack_start(hbox, False, False, 6)

        button = Gtk.Button.new_with_mnemonic("Open in _Web browser")
        button.connect("clicked", self.on_openweb_clicked)
        button.set_tooltip_text("Open in the default web browser to do things like printing")
        hbox.pack_start(button, False, False, 12)

        button = Gtk.Button.new_with_mnemonic("_OK")
        button.connect("clicked", self.on_ok_clicked)
        hbox.pack_end(button, False, False, 12)
        bind_accelerator(self.accelerators, button, '<Control>w')

        self.resize(800, 600)
        self.show_all()

    def doc_policy(self, web_view, decision, decision_type):
        logging.info("Checking policy")
        logging.debug("received policy decision request of type: {0}".format(decision_type.value_name))
        if decision_type == WebKit2.PolicyDecisionType.NAVIGATION_ACTION or \
                decision_type == WebKit2.PolicyDecisionType.NEW_WINDOW_ACTION:
            nav_action = decision.get_navigation_action()
            request = nav_action.get_request()
            uri = request.get_uri()
            logging.debug("nav request is for uri %s", uri)
            if "welcome.htm" not in uri:
                logging.debug("opening uri %s in webbrowser")
                webbrowser.open(uri)
                decision.ignore()
                return True
        return False

    def on_openweb_clicked(self, button):
        logging.info("\"Open in Web browser\" button was clicked")
        webbrowser.open(self.welcomeurl)

    def on_ok_clicked(self, button):
        logging.info("Closing welcome window")
        self.response(Gtk.ResponseType.OK)
        self.close()
