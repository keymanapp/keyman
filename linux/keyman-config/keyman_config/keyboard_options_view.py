#!/usr/bin/python3

import logging
import urllib.parse
import webbrowser
from urllib.parse import parse_qsl, urlencode

import gi

gi.require_version('Gtk', '3.0')
try:
    gi.require_version('WebKit2', '4.1')
except ValueError:
    # TODO: Remove once we drop support for Ubuntu 20.04 Focal
    gi.require_version('WebKit2', '4.0')

from gi.repository import Gtk, WebKit2

from keyman_config import _
from keyman_config.accelerators import init_accel
from keyman_config.dconf_util import get_option, set_option


class KeyboardOptionsView(Gtk.Window):

    def __init__(self, info):
        self.accelerators = None
        self.optionurl = info["optionurl"]
        self.packageID = info["packageID"]
        self.keyboardID = info["keyboardID"]
        kbtitle = _("{packageId} Settings").format(packageId=self.packageID)
        super().__init__(title=kbtitle)
        init_accel(self)

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)

        # Keyman Desktop gets current option settings from the registry.
        # Similarly, we'll read Keyman options from DConf and update optionurl
        info = {"packageID": self.packageID, "keyboardID": self.keyboardID}
        self.options = get_option(info)
        params = ""

        if self.options:
            # Convert dictionary to query
            params = "?" + urlencode(self.options)

        s = Gtk.ScrolledWindow()
        self.webview = WebKit2.WebView()
        self.webview.connect("decide-policy", self.doc_policy)
        self.webview.load_uri(self.optionurl + params)
        s.add(self.webview)
        vbox.pack_start(s, True, True, 0)

        hbox = Gtk.Box(spacing=12)
        vbox.pack_start(hbox, False, False, 6)

        self.add(vbox)

    def doc_policy(self, web_view, decision, decision_type):
        logging.info("Checking policy")
        logging.debug("received policy decision request of type: {0}".format(decision_type.value_name))
        if decision_type == WebKit2.PolicyDecisionType.NAVIGATION_ACTION or \
                decision_type == WebKit2.PolicyDecisionType.NEW_WINDOW_ACTION:
            nav_action = decision.get_navigation_action()
            request = nav_action.get_request()
            uri = request.get_uri()
            logging.debug("nav request is for uri %s", uri)
            parsed = urllib.parse.urlparse(uri)
            if parsed.scheme == "keyman":
                logging.debug("using keyman scheme")
                if parsed.path == "cancel":
                    pass
                elif parsed.path == "ok":
                    logging.debug("submit options form")
                    # Parse the response for the option_key value and merge the updates
                    if parsed.query:
                        updated_options = parse_qsl(parsed.query, keep_blank_values=True)
                        self.options.update(updated_options)
                        self.process_option()

                self.close()
                return True

            if "options.htm" not in uri:
                logging.debug("opening uri %s in webbrowser")
                webbrowser.open(uri)
                decision.ignore()
                return True
        return False

    def process_option(self):
        # Write the Keyman options to DConf
        info = {"packageID": self.packageID, "keyboardID": self.keyboardID}
        set_option(info, self.options)
