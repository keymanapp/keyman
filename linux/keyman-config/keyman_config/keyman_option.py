#!/usr/bin/python3
'''
 Keyman is copyright (C) SIL Global. MIT License.

 Implementation of the Keyman DConf options
'''
import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gio, Gtk


class KeymanOption:
    '''
    Methods to set and read Keyman options
    '''
    def __new__(cls, option_name):
        if not hasattr(cls, 'settings'):
            cls.settings = Gio.Settings.new('com.keyman.options')  # type: ignore
        obj = super().__new__(cls)
        obj.option_name = option_name
        obj.settings = cls.settings
        return obj

    def bind_checkbutton(self, button: Gtk.CheckButton, handler = None) -> None:
        '''
        Bind the option to the a button, optionally also adding an additional handler.
        '''
        self.settings.bind(self.option_name, button, 'active', Gio.SettingsBindFlags.NO_SENSITIVITY)
        if handler:
            self.settings.connect(f'changed::{self.option_name}', handler)

    def set(self, enabled: bool) -> None:
        '''
        Set the value of the option
        '''
        self.settings.set_boolean(self.option_name, enabled)

    def get(self) -> bool:
        '''
        Get the value of the option
        '''
        return self.settings.get_boolean(self.option_name)
