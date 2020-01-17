#!/usr/bin/python3

import ast
import gi
import logging
import webbrowser
import urllib.parse
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit2', '4.0')
from gi.repository import Gtk, WebKit2
from keyman_config.accelerators import bind_accelerator, init_accel
from keyman_config.dconf_util import get_option, set_option

class OptionsView(Gtk.Window):

    def __init__(self, info):
        self.accelerators = None
        self.optionurl = info["optionurl"]
        self.packageID = info["packageID"]
        self.keyboardID = info["keyboardID"]
        kbtitle = self.packageID + " Settings"
        Gtk.Window.__init__(self, title=kbtitle)
        init_accel(self)

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)

        # Keyman Desktop gets current option settings from the registry.
        # Similarly, we'll read Keyman options from dconf and update optionurl
        info = {"packageID": self.packageID, "keyboardID": self.keyboardID}
        array_option = get_option(info)
        params = ""
        if array_option:
            # Convert options from dconf into form parameters to pass to options.htm
            for index, param in enumerate(array_option):
                if index == 0:
                    params = "?"
                else:
                    params += "&"
                params += param

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
                    # Parse the response for the option_key value
                    if parsed.query:
                        array_option = urllib.parse.parse_qsl(parsed.query, keep_blank_values=True)
                        list_option = []
                        for index, tuple_option in enumerate(array_option):
                            list_option.append(tuple_option[0] + "=" + tuple_option[1])
                        self.process_option(list_option)

                self.close()
                return True

            if not "options.htm" in uri:
                logging.debug("opening uri %s in webbrowser")
                webbrowser.open(uri)
                decision.ignore()
                return True
        return False

    def process_option(self, list_option):
        #print(self.packageID + " list_option " + str(list_option))

        # Write the Keyman option to dconf
        info = { "packageID": self.packageID, "keyboardID": self.keyboardID, "list": list_option }
        set_option(info)

