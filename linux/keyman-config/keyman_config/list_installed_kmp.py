#!/usr/bin/python3

import argparse
import logging
import os
import json
from enum import Enum, auto
from keyman_config.kmpmetadata import parsemetadata, parseinfdata
from keyman_config.install_kmp import user_keyman_dir

class InstallArea(Enum):
    IA_OS = auto()
    IA_SHARED = auto()
    IA_USER = auto()
    IA_UNKNOWN = auto()

def get_installed_kmp(area):
    """
    Get list of installed keyboards in an install area.

	Args:
		area (InstallArea): install area to check
            InstallArea.IA_USER: ~/.local/share/keyman and ~/.kmfl
            InstallArea.IA_SHARED: /usr/local/share/keyman
            InstallArea.IA_OS: /usr/share/keyman
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
    check_paths = []
    if area == InstallArea.IA_USER:
        home = os.path.expanduser("~")
        check_paths = [ user_keyman_dir(), os.path.join(home, ".kmfl") ]
    elif area == InstallArea.IA_SHARED:
        check_paths = [ "/usr/local/share/keyman" ]
    elif area == InstallArea.IA_OS:
        check_paths = [ "/usr/share/keyman" ]

    return get_installed_kmp_paths(check_paths)


def get_installed_kmp_paths(check_paths):
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
    This return the highest version if installed in more than one area

    Args:
        keyboardid (dict): Keyboard ID
    Returns:
        str: kmp version if keyboard ID is installed
        None: if not found
    """
    version = None
    user_kmp = get_installed_kmp_user()
    shared_kmp = get_installed_kmp_shared()
    os_kmp = get_installed_kmp_os()

    if keyboardid in os_kmp:
        version = os_kmp[keyboardid]['version']

    if keyboardid in shared_kmp:
        shared_version = shared_kmp[keyboardid]['version']
        if version:
            if version < shared_version:
                version = shared_version
        else:
            version = shared_version

    if keyboardid in user_kmp:
        user_version = user_kmp[keyboardid]['version']
        if version:
            if version < user_version:
                version = user_version
        else:
            version = shared_version

    return version

def get_kmp_version_user(keyboardid):
    """
    Get version of the kmp for a keyboard ID.
    This only checks the user area.

    Args:
        keyboardid (dict): Keyboard ID
    Returns:
        str: kmp version if keyboard ID is installed
        None: if not found
    """
    user_kmp = get_installed_kmp_user()
    if keyboardid in user_kmp:
        return user_kmp[keyboardid]['version']
    else:
        return None
