#!/usr/bin/python3

import gi

gi.require_version('Gtk', '3.0')

from gi.repository import Gtk

from keyman_config import _
from keyman_config.keyman_option import KeymanOption


class OptionsWidget(Gtk.Box):
    def __init__(self, sentry) -> None:
        super().__init__(orientation=Gtk.Orientation.VERTICAL)
        label = Gtk.Label(_("General"))
        label.set_padding(5, 5)
        label.set_halign(Gtk.Align.START)
        self.pack_start(label, False, False, 10)

        (sentry_enabled, reason) = sentry.is_sentry_enabled()
        disabledByVariable = sentry.is_sentry_disabled_by_variable()

        self.errorReportingButton = Gtk.CheckButton(_("Automatically report errors to keyman.com"))
        self.errorReportingButton.set_active(sentry_enabled)
        self.errorReportingButton.set_sensitive(not disabledByVariable)
        sentry.bind_checkbutton(self.errorReportingButton)
        self.pack_start(self.errorReportingButton, False, False, 0)

        if disabledByVariable:
            label = Gtk.Label(reason)
            label.set_halign(Gtk.Align.START)
            label.set_padding(25, 0)
            label.set_sensitive(False)
            self.pack_start(label, False, False, 0)

        self.simulate_altgr_option = KeymanOption('simulate-altgr')
        self.simulate_altgr_enabled = self.simulate_altgr_option.get()
        self.simulateAltGrButton = Gtk.CheckButton(_("Simulate AltGr with Ctrl + Alt"))
        self.simulateAltGrButton.set_active(sentry_enabled)
        self.simulate_altgr_option.bind_checkbutton(self.simulateAltGrButton)
        self.pack_start(self.simulateAltGrButton, False, False, 0)
