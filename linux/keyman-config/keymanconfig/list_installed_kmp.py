#!/usr/bin/python3

import argparse
import os
import json
from keymanconfig.kmpmetadata import parsemetadata, parseinfdata
from keymanconfig.get_kmp import get_keyboard_data

def get_installed_kmp():
    """
    Get list of installed keyboards.

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
    check_paths = [ "/usr/share/keyman", "/usr/local/share/keyman" ]
    for keymanpath in check_paths:
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
#                else:
#                    kbdata = get_keyboard_data(o)
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


def main():
    parser = argparse.ArgumentParser(description='Show installed keyman keyboards.')
    parser.add_argument('-s', "--short", help='short format', action="store_true")

    args = parser.parse_args()

    installed_kmp = get_installed_kmp()
#    print(installed_kmp)
#    print(sorted(installed_kmp))
    print("--- Installed keyboards ---")
    for kmp in sorted(installed_kmp):
        print(installed_kmp[kmp]['name'] + ", version:", installed_kmp[kmp]['version'] + ", id:", kmp)
        if not args.short:
            if installed_kmp[kmp]['version'] != installed_kmp[kmp]['kmpversion']:
                print("Version mismatch. Installed keyboard is %s. Website says it is %s." % (installed_kmp[kmp]['kmpversion'], installed_kmp[kmp]['version']))
            if installed_kmp[kmp]['name'] != installed_kmp[kmp]['kmpname']:
                print("Name mismatch. Installed keyboard is %s. Website says it is %s." % (installed_kmp[kmp]['name'], installed_kmp[kmp]['kmpname']))

            if installed_kmp[kmp]['description']:
                print(installed_kmp[kmp]['description'])
            else:
                print("No description")
            print(os.linesep)


if __name__ == "__main__":
    main()
