#!/usr/bin/python3

import json
import logging
import os

from keyman_config import secure_lookup
from keyman_config.deprecated_decorator import deprecated
from keyman_config.get_kmp import InstallLocation, get_keyman_dir
from keyman_config.kmpmetadata import parseinfdata, parsemetadata


@deprecated('Use get_keyman_dir(area) instead')
def get_install_area_path(area):
    return get_keyman_dir(area)


def get_installed_kmp(area):
    """
    Get list of installed kmp in an install area.

    Args:
        area (InstallLocation): install area to check
            InstallLocation.User: ~/.local/share/keyman
            InstallLocation.Shared: /usr/local/share/keyman
            InstallLocation.OS: /usr/share/keyman
    Returns:
        list: Installed kmp
            dict: Keyboard
                id (str): Keyboard ID
                name (str): Keyboard name
                kmpname (str): Keyboard name in local
                version (str): Keyboard version
                kmpversion (str):
                path (str): base path where keyboard is installed
                description (str): Keyboard description
    """
    check_paths = [get_keyman_dir(area)]

    return get_installed_kmp_paths(check_paths)


def get_installed_kmp_paths(check_paths):
    """
    Get list of installed kmp.

    Args:
        check_paths (list): list of paths to check

    Returns:
        list: Installed kmp
            dict: Keyboard
                packageID (str): kmp ID
                keyboardID (str): Keyboard ID
                name (str): Keyboard name
                kmpname (str): Keyboard name in local
                version (str): Keyboard version
                kmpversion (str):
                areapath (str): base path of area where kmp is installed
                description (str): Keyboard description
                has_kbjson (bool): Keyboard is in the json file
                has_kboptions (bool): Keyboard uses options.htm form
    """
    installed_keyboards = {}
    for keymanpath in check_paths:
        if os.path.isdir(keymanpath):
            for o in os.listdir(keymanpath):
                if os.path.isdir(os.path.join(keymanpath, o)) and o != "icons":
                    name = md_name = version = md_version = keyboardID = description = kbdata = None
                    info, system, options, keyboards, files = parsemetadata(os.path.join(keymanpath, o, "kmp.json"))
                    if not info:
                        info, system, options, keyboards, files = parseinfdata(os.path.join(keymanpath, o, "kmp.inf"))
                    has_kbjson = False
                    kbjson = os.path.join(keymanpath, o, o + ".json")
                    if os.path.isfile(kbjson):
                        try:
                            with open(kbjson, "r") as read_file:
                                kbdata = json.load(read_file)
                            if kbdata:
                                if 'description' in kbdata:
                                    description = kbdata['description']
                                version = kbdata['version']
                                name = kbdata['name']
                                has_kbjson = True
                        except Exception as e:
                            logging.warning('Exception %s loading %s %s', type(e), kbjson, e.args)
                    if info:
                        md_version = secure_lookup(info, 'version', 'description')
                        md_name = secure_lookup(info, 'name', 'description')
                    if keyboards:
                        keyboardID = keyboards[0]['id']
                    else:
                        keyboardID = o

                    if not name:
                        version = md_version
                        name = md_name

                    has_kboptions = False
                    if files:
                        for fileinfo in files:
                            if fileinfo['name'] == "options.htm":
                                has_kboptions = True
                                break
                    installed_keyboards[o] = {
                        "packageID": o, "keyboardID": keyboardID, "name": name,
                        "kmpname": md_name, "version": version, "kmpversion": md_version,
                        "areapath": keymanpath, "description": description,
                        "has_kbjson": has_kbjson, "has_kboptions": has_kboptions}
    return installed_keyboards


def get_kmp_version(packageID):
    """
    Get version of the kmp for a package ID.
    This return the highest version if installed in more than one area

    Args:
        packageID (dict): kmp ID
    Returns:
        str: kmp version if kmp ID is installed
        None: if not found
    """
    version = None
    user_kmp = get_installed_kmp(InstallLocation.User)
    shared_kmp = get_installed_kmp(InstallLocation.Shared)
    os_kmp = get_installed_kmp(InstallLocation.OS)

    if packageID in os_kmp:
        version = os_kmp[packageID]['version']

    if packageID in shared_kmp:
        shared_version = shared_kmp[packageID]['version']
        if version:
            if version < shared_version:
                version = shared_version
        else:
            version = shared_version

    if packageID in user_kmp:
        user_version = user_kmp[packageID]['version']
        if version:
            if version < user_version:
                version = user_version
        else:
            version = user_version

    return version


def get_kmp_version_user(packageID):
    """
    Get version of the kmp for a kmp ID.
    This only checks the user area.

    Args:
        packageID (dict): kmp ID
    Returns:
        str: kmp version if kmp ID is installed
        None: if not found
    """
    user_kmp = get_installed_kmp(InstallLocation.User)
    if packageID in user_kmp:
        return user_kmp[packageID]['version']
    else:
        return None
