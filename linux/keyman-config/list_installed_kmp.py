#!/usr/bin/python3

import os
#import configparser
import json
from kmpmetadata import parsemetadata
from get_kmp import get_keyboard_data

def get_installed_kmp():
    installed_keyboards = {}
    check_paths = [ "/usr/share/keyman", "/usr/local/share/keyman" ]
    for keymanpath in check_paths:
        for o in os.listdir(keymanpath):
            if os.path.isdir(os.path.join(keymanpath,o)):
                metadata = parsemetadata(os.path.join(keymanpath, o, "kmp.json"))
                name = md_name = version = md_version = description = kbdata = None
                kbjson = os.path.join(keymanpath, o, o + ".json")
                if os.path.isfile(kbjson):
                    with open(kbjson, "r") as read_file:
                        kbdata = json.load(read_file)
                else:
                    kbdata = get_keyboard_data(o)
                if kbdata:
                    if 'description' in kbdata:
                        description = kbdata['description']
                    version = kbdata['version']
                    name = kbdata['name']
                if metadata:
                    info = metadata[0]
                    md_version = info['version']['description']
                    md_name = info['name']['description']
                if not name:
                    version = md_version
                    name = md_name

                installed_keyboards[o] = { "id" : o, "name" : name, "kmpname" : md_name, "version" : version, "kmpversion" : md_version, "path" : keymanpath, "description" : description}
    return installed_keyboards    


def kmp_version(keyboardid):
    installed_kmp = get_installed_kmp()
    if keyboardid in installed_kmp:
        return installed_kmp[keyboardid]['version']
    else:
        return None


def main():
    installed_kmp = get_installed_kmp()
#    print(installed_kmp)
#    print(sorted(installed_kmp))
    print("--- Installed keyboards ---")
    for kmp in sorted(installed_kmp):
        print(installed_kmp[kmp]['name'] + ", version:", installed_kmp[kmp]['version'] + ", id:", kmp)
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