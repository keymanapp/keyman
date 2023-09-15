#!/usr/bin/python3

from gi.repository import Gio

# GSettings path destkop/ibus/keyman/options
GSETTINGS_BASE = 'com.keyman.options'

# Utilities to get and set Keyman options in GSettings:
# /desktop/ibus/keyman/options/packageID/keyboardID/options


def get_child_schema(info):
    if 'packageID' not in info or 'keyboardID' not in info or not info['packageID'] or not info['keyboardID']:
        return None

    settings = Gio.Settings.new(GSETTINGS_BASE)
    path = settings.get_property('path')
    if not path.endswith('/'):
        path += '/'
    path += info['packageID'] + '/' + info['keyboardID'] + '/'
    return Gio.Settings.new_with_path(f'{GSETTINGS_BASE}.child', path)


def get_option(info):
    """
    Get the Keyman keyboard options from GSettings
    Convert from list of comma-separated strings into dictionary

    Args:
        info: dictionary
            packageID (str): Package ID
            keyboardID(str): Keyboard ID

    Returns:
        result (dictionary): Keyboard options
    """
    result = {}
    if 'packageID' in info and 'keyboardID' in info:
        if child_schema := get_child_schema(info):
            list_options = child_schema.get_strv('options')
            result = dict(option.split("=") for option in list_options)
    return result


def set_option(info, options):
    """
    Store the Keyman keyboard options in GSettings as a list of strings

    Args:
        info: dictionary
            packageID (str): package ID
            keyboardID (str): keyboard ID
        options: dictionary
            key and values to store
    """
    if 'packageID' in info and 'keyboardID' in info and options:
        # Convert dictionary of options into a list of option strings
        list_options = [f'{key}={value}' for key, value in options.items()]
        if child_schema := get_child_schema(info):
            child_schema.set_strv('options', list_options)


if __name__ == '__main__':
    info = {"packageID": "invalid", "keyboardID": "invalid"}
    options = get_option(info)
    assert(options == {})

    info = {"packageID": "sil_cipher_music", "keyboardID": "sil_cipher_music"}
    options = get_option(info)

    options["set_nfc"] = "1"
    set_option(info, options)
