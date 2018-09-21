#!/usr/bin/python3

import argparse
import logging
import os
import json
from keyman_config.kmpmetadata import parsemetadata, parseinfdata

def get_installed_kmp_os():
    """
    Get list of installed keyboards in /usr/share.

    Returns:
        list: Installed keyboards
            dict: Keyboard
                id (str): Keyboard ID
                name (str): Keyboard name
                kmpname (str): Keyboard name in local
                version (str): Keyboard version
                kmpversion (str):
                path (str): base path where keyboard is installed
                description (str): Keyboard description
    """
    check_paths = [ "/usr/share/keyman" ]
    return get_installed_kmp(check_paths)


def get_installed_kmp_shared():
    """
    Get list of installed keyboards in /usr/local/share.

    Returns:
        list: Installed keyboards
            dict: Keyboard
                id (str): Keyboard ID
                name (str): Keyboard name
                kmpname (str): Keyboard name in local
                version (str): Keyboard version
                kmpversion (str):
                path (str): base path where keyboard is installed
                description (str): Keyboard description
    """
    check_paths = [ "/usr/local/share/keyman" ]
    return get_installed_kmp(check_paths)

def get_installed_kmp_user():
    """
    Get list of installed keyboards in user areas.

    Returns:
        list: Installed keyboards
            dict: Keyboard
                id (str): Keyboard ID
                name (str): Keyboard name
                kmpname (str): Keyboard name in local
                version (str): Keyboard version
                kmpversion (str):
                path (str): base path where keyboard is installed
                description (str): Keyboard description
    """
    home = os.path.expanduser("~")
    datahome = os.environ.get("XDG_DATA_HOME", os.path.join(home, ".local", "share"))
    check_paths = [ os.path.join(datahome, "keyman"), os.path.join(home, ".kmfl") ]
    return get_installed_kmp(check_paths)


def get_installed_kmp(check_paths):
    """
    Get list of installed keyboards.

	Args:
		check_paths (list): list of paths to check

    Returns:
        list: Installed keyboards
            dict: Keyboard
                id (str): Keyboard ID
                name (str): Keyboard name
                kmpname (str): Keyboard name in local
                version (str): Keyboard version
                kmpversion (str):
                path (str): base path where keyboard is installed
                description (str): Keyboard description
    """
    installed_keyboards = {}
    for keymanpath in check_paths:
        if os.path.isdir(keymanpath):
            for o in os.listdir(keymanpath):
                if os.path.isdir(os.path.join(keymanpath,o)) and o != "icons":
                    name = md_name = version = md_version = description = kbdata = None
                    metadata = parsemetadata(os.path.join(keymanpath, o, "kmp.json"))
                    if not metadata[0]:
                        metadata = parseinfdata(os.path.join(keymanpath, o, "kmp.inf"))
                    kbjson = os.path.join(keymanpath, o, o + ".json")
                    if os.path.isfile(kbjson):
                        with open(kbjson, "r") as read_file:
                            kbdata = json.load(read_file)
                    if kbdata:
                        if 'description' in kbdata:
                            description = kbdata['description']
                        version = kbdata['version']
                        name = kbdata['name']
                    if metadata[0]:
                        info = metadata[0]
                        md_version = info['version']['description']
                        md_name = info['name']['description']
                    if not name:
                        version = md_version
                        name = md_name

                    installed_keyboards[o] = { "id" : o, "name" : name, "kmpname" : md_name, "version" : version, "kmpversion" : md_version, "path" : keymanpath, "description" : description}
    return installed_keyboards


def get_kmp_version(keyboardid):
    """
    Get version of the kmp for a keyboard ID.

    Args:
        keyboardid (dict): Keyboard ID
    Returns:
        str: kmp version if keyboard ID is installed
        None: if not found
    """
    installed_kmp = get_installed_kmp()
    if keyboardid in installed_kmp:
        return installed_kmp[keyboardid]['version']
    else:
        return None