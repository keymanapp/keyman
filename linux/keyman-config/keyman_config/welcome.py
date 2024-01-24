#!/usr/bin/python3

import logging
import os
import webbrowser

import gi

gi.require_version('Gtk', '3.0')
try:
    gi.require_version('WebKit2', '4.1')
except ValueError:
    # TODO: Remove once we drop support for Ubuntu 20.04 Focal
    gi.require_version('WebKit2', '4.0')
from gi.repository import Gtk, WebKit2

from keyman_config import _
from keyman_config.accelerators import bind_accelerator, init_accel

# NOTE: WebKit2 is not able to load XHTML files nor files with an encoding other
# than ASCII or UTF-8


class WelcomeView(Gtk.Dialog):

    def __init__(self, parent, welcomeurl, keyboardname, newlyInstalled=False):
        self.accelerators = None
        if newlyInstalled:
            kbtitle = _("{name} installed").format(name=keyboardname)
        else:
            kbtitle = keyboardname
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

        button = Gtk.Button.new_with_mnemonic(_("Open in _Web browser"))
        button.connect("clicked", self.on_openweb_clicked)
        button.set_tooltip_text(_("Open in the default web browser to do things like printing"))
        hbox.pack_start(button, False, False, 12)

        button = Gtk.Button.new_with_mnemonic(_("_OK"))
        button.connect("clicked", self.on_ok_clicked)
        hbox.pack_end(button, False, False, 12)
        bind_accelerator(self.accelerators, button, '<Control>w')

        self.resize(800, 600)
        self.show_all()

    def doc_policy(self, web_view, decision, decision_type):
        logging.info("Checking policy")
        logging.debug("received policy decision request of type: {0}".format(decision_type.value_name))
        if decision_type in [
            WebKit2.PolicyDecisionType.NAVIGATION_ACTION,
            WebKit2.PolicyDecisionType.NEW_WINDOW_ACTION,
        ]:
            nav_action = decision.get_navigation_action()
            request = nav_action.get_request()
            uri = request.get_uri()
            logging.debug("nav request is for uri %s", uri)
            if not uri.startswith("file://"):
                logging.debug("opening external uri %s in webbrowser", uri)
                webbrowser.open(uri)
                decision.ignore()
                return True
            elif ".htm" not in uri:
                cmd = f'xdg-open {uri}'
                logging.debug('opening uri in default app: %s', cmd)
                os.system(cmd)
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
