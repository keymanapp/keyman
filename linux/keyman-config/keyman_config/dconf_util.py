#!/usr/bin/python3

import logging
from gi.repository import Gio, Gtk

# DConf path destkop/ibus/keyman/options
DCONF_BASE = "com.keyman.options"

# Utilities to get and set Keyman options in DConf:
# /desktop/ibus/keyman/options/packageID/keyboardID/options


def get_child_schema(info):
    settings = Gio.Settings.new(DCONF_BASE)
    path = settings.get_property('path')
    if not path.endswith('/'):
        path += '/'
    path += info['packageID'] + '/' + info['keyboardID'] + '/'
    return Gio.Settings(DCONF_BASE + '.child', path)


def get_option(info):
    """
    Get the Keyman keyboard options from DConf

    Args:
        info: dictionary
            packageID (str): package ID
            keyboardID (str): keyboard ID

    Returns:
        keyboard options (list)
    """
    if "packageID" in info and "keyboardID" in info:
        child_schema = get_child_schema(info)
        result = child_schema.get_strv("options")
        return result


def set_option(info):
    """
    Store the Keyman keyboard option in DConf

    Args:
        info: dictionary
            packageID (str): package ID
            keyboardID (str): keyboard ID
            list (str): list of keyboard options
    """
    if "packageID" in info and "keyboardID" in info and "list" in info:
        child_schema = get_child_schema(info)
        child_schema.set_strv("options", info["list"])


if __name__ == '__main__':
    info = {"packageID": "sil_cipher_music", "keyboardID": "sil_cipher_music", "list": ['option_key=jianpu', 'option_three=two'] }
    set_option(info)
    result = get_option(info)
