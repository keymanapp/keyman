#!/usr/bin/python3

import configparser
import json
import logging
import os
import sys
from enum import Enum
from json.decoder import JSONDecodeError

import magic

from keyman_config import secure_lookup


class KMFileTypes(Enum):
    KM_ICON = 1
    KM_SOURCE = 2
    KM_OSK_SOURCE = 3
    KM_KMX = 4
    KM_OSK = 5
    KM_TOUCH = 6
    KM_FONT = 7
    KM_DOC = 8
    KM_META = 9
    KM_IMAGE = 10
    KM_TECKIT = 11
    KM_CC = 12
    KM_XML = 13
    KM_UNKNOWN = 99


def print_info(info):
    try:
        print("---- Info ----")
        if not info:
            return
        print("Name: ", secure_lookup(info, 'name', 'description'))
        print("Copyright: ", secure_lookup(info, 'copyright', 'description'))
        if 'version' in info:
            print("Version: ", secure_lookup(info, 'version', 'description'))
        if 'author' in info:
            print("Author: ", secure_lookup(info, 'author', 'description'))
            if secure_lookup(info, 'author', 'url'):
                print("Author URL: ", secure_lookup(info, 'author', 'url'))
        if 'website' in info:
            print("Website description: ", secure_lookup(info, 'website', 'description'))
            if secure_lookup(info, 'website', 'url'):
                print("Website URL: ", secure_lookup(info, 'website', 'url'))
    except Exception as e:
        print(type(e))    # the exception instance
        print(e.args)     # arguments stored in .args
        print(e)          # __str__ allows args to be printed directly,		pass


def print_system(system):
    try:
        print("---- System ----")
        if not system:
            return
        if 'fileVersion' in system:
            print("File Version: ", system['fileVersion'])
        if 'keymanDeveloperVersion' in system:
            print("Keyman Developer Version: ", system['keymanDeveloperVersion'])
    except Exception as e:
        print(type(e))    # the exception instance
        print(e.args)     # arguments stored in .args
        print(e)          # __str__ allows args to be printed directly,		pass


def print_options(options):
    try:
        print("---- Options ----")
        if not options:
            return
        if 'readmeFile' in options:
            print("Readme File: ", options['readmeFile'])
        if 'graphicFile' in options:
            print("Graphic File: ", options['graphicFile'])
    except Exception as e:
        print(type(e))    # the exception instance
        print(e.args)     # arguments stored in .args
        print(e)          # __str__ allows args to be printed directly,		pass


def print_keyboards(keyboards):
    try:
        print("---- Keyboards ----")
        if not keyboards:
            return
        for kb in keyboards:
            print("Keyboard Name: ", kb['name'])
            print("Keyboard Id: ", kb['id'])
            if 'version' in kb:
                print("Keyboard Version: ", kb['version'])
            if 'oskFont' in kb:
                print("Keyboard On screen keyboard Font: ", kb['oskFont'])
            if 'oskFont' in kb:
                print("Keyboard Display Font: ", kb['displayFont'])
            print("Languages")
            for lang in kb['languages']:
                print("  Name: ", lang['name'], "Id: ", lang['id'])
    except Exception as e:
        print(type(e))    # the exception instance
        print(e.args)     # arguments stored in .args
        print(e)          # __str__ allows args to be printed directly,		pass


def determine_filetype(kblist, filename):
    """
    Determine file type of a filename in a kmp from the extension

    Args:
        kblist (list): list of keyboard ids
        filename (str): File name

    Returns:
        KMFileTypes: Enum of file type
            KM_ICON: Keyboard icon
            KM_SOURCE: Keyboard source
            KM_OSK_SOURCE: Keyboard on-screen keyboard source
            KM_KMX: Compiled keyboard
            KM_OSK: Compiled on screen keyboard
            KM_TOUCH: JS touch keyboard
            KM_FONT: Font
            KM_DOC: Documentation
            KM_META: Metadata
            KM_IMAGE: Image
            KM_TECKIT: Files to use with teckit
            KM_CC: Consistent changes tables
            KM_XML: unspecified xml files
            KM_UNKNOWN: Unknown
    """
    name, ext = os.path.splitext(filename)
    if not ext:
        return KMFileTypes.KM_UNKNOWN
    elif ext.lower() == ".ico":
        return KMFileTypes.KM_ICON
    elif ext.lower() == ".kmn":
        return KMFileTypes.KM_SOURCE
    elif ext.lower() == ".kvks":
        return KMFileTypes.KM_OSK_SOURCE
    elif ext.lower() == ".kmx":
        return KMFileTypes.KM_KMX
    elif ext.lower() == ".kvk":
        return KMFileTypes.KM_OSK
    elif ext.lower() in [".ttf", ".otf"]:
        return KMFileTypes.KM_FONT
    elif ext.lower() == ".js":
        if kblist is None:
            return KMFileTypes.KM_UNKNOWN
        if name in kblist:
            return KMFileTypes.KM_TOUCH
        else:
            return KMFileTypes.KM_DOC if name == "keyrenderer" else KMFileTypes.KM_UNKNOWN
    elif ext.lower() in [
      ".txt",
      ".pdf",
      ".htm",
      ".html",
      ".doc",
      ".docx",
      ".css",
      ".chm",
      "",
      ".md",
      ".odt",
      ".rtf",
      ".dot",
      ".mht",
      ".woff",
      ".php",
    ]:
        return KMFileTypes.KM_DOC
    elif ext.lower() in [".inf", ".json"]:
        return KMFileTypes.KM_META
    elif ext.lower() in [".png", ".jpeg", ".jpg", ".gif", ".bmp"]:
        return KMFileTypes.KM_IMAGE
    elif ext.lower() in [".tec", ".map"]:
        return KMFileTypes.KM_TECKIT
    elif ext.lower() == ".cct":
        return KMFileTypes.KM_CC
    elif ext.lower() == ".xml":
        return KMFileTypes.KM_XML
    else:
        return KMFileTypes.KM_UNKNOWN


def print_files(files, extracted_dir):
    try:
        print("---- Files ----")
        if not files:
            return
        for kbfile in files:
            print("* File name: ", kbfile['name'])
            print("    Description: ", kbfile['description'])
            print("    Type: ", kbfile['type'])
            file = os.path.join(extracted_dir, kbfile['name'])
            if os.path.isfile(file):
                print("    File", file, "exists")
                ms = magic.open(magic.MAGIC_NONE)
                ms.load()
                ftype = ms.file(file)
                print("        Type: ", ftype)
                ms.close()
            else:
                print("    File", file, "does not exist")
    except Exception as e:
        print(type(e))    # the exception instance
        print(e.args)     # arguments stored in .args
        print(e)          # __str__ allows args to be printed directly,		pass


def get_fonts(files):
    fonts = []
    if not files:
        return fonts
    for kbfile in files:
        if kbfile['type'] == KMFileTypes.KM_FONT:
            fonts.append(kbfile)
    return fonts


def parseinfdata(inffile, verbose=False):
    """
    Parse the metadata in a kmp.inf file.

    Args:
        jsonfile (str): Path to kmp.inf
        verbose (bool, default False): verbose output

    Returns:
        list[5]: info, system, options, keyboards, files
            info (dict):
                name (dict):
                    description (str): KMP name
                copyright (dict):
                    description (str): KMP copyright
                version (dict):
                    description (str): KMP version
                author (dict):
                    description (str): KMP author
                    url (str): contact url for the author
            system (dict): System info
                fileVersion (str): Keyman file format version
                keymanDeveloperVersion (str): Keyman Developer version that compiled keyboard
            options (dict): Keyboard options
                readmeFile (str): README for the keyboard
            keyboards (list): Keyboards in the kmp
                name (str): Keyboard name
                id (str): Keyboard ID
                version (str): Keyboard version
                oskFont (str, optional): Recommended on screen keyboard font
                displayFont (str, optional): Recommended display font
                languages (list): Languages the keyboard is used for
                    name (str): Language name
                    id (str): Language ID
            files (list): Files in the kmp
                name (str): File name
                description (str): File description
                type (KMFileTypes): Keyman file type
    """
    info = system = keyboards = files = options = None
    extracted_dir = os.path.dirname(inffile)

    if os.path.isfile(inffile):
        config = configparser.ConfigParser()
        config.optionxform = str
        logging.debug("parseinfdata: reading file:%s dir:%s", inffile, extracted_dir)

        with open(inffile, 'r', encoding='latin_1') as f:
            config.read_file(f)

        info = None
        for section in config.sections():
            if section == 'Info':
                if not info:
                    info = {}
                for item in config.items('Info'):
                    if item[0] in ['Name', 'name']:
                        info['name'] = {'description': item[1].split("\"")[1]}
                    elif item[0] in ['Copyright', 'copyright']:
                        info['copyright'] = {'description': item[1].split("\"")[1]}
                    elif item[0] == 'Version':
                        info['version'] = {'description': item[1].split("\"")[1]}
                    elif item[0] == 'Author':
                        info['author'] = {'description': item[1].split("\"")[1], 'url': item[1].split("\"")[3]}
                    elif item[0] == "WebSite":
                        info['website'] = {'description': item[1].split("\"")[1], 'url': item[1].split("\"")[3]}
                    else:
                        logging.warning("Unknown item in Info: %s", item[0])
            if section == 'PackageInfo':
                if not info:
                    info = {}
                info['version'] = {'description': "1.0"}
                for item in config.items('PackageInfo'):
                    if item[0] in ['Name', 'name']:
                        if item[1].split("\"")[1]:
                            info['name'] = {'description': item[1].split("\"")[1]}
                        else:
                            info['name'] = {'description': item[1].split("\"")[2]}
                    elif item[0] in ['Copyright', 'copyright']:
                        if item[1].split("\"")[1]:
                            info['copyright'] = {'description': item[1].split("\"")[1]}
                        else:
                            info['copyright'] = {'description': item[1].split("\"")[2]}
                    elif item[0] == 'Version':
                        info['version'] = {'description': item[1].split("\"")[1]}
                    elif item[0] == 'Author':
                        info['author'] = {'description': item[1].split("\"")[1], 'url': item[1].split("\"")[3]}
                    elif item[0] == "WebSite":
                        if item[1].split("\"")[1]:
                            info['website'] = {'description': item[1].split("\"")[1]}
                        else:
                            info['website'] = {'description': item[1].split("\"")[2]}
                    else:
                        logging.warning("Unknown item in Info: %s", item[0])
            elif section == 'Package':
                system = {}
                if not options:
                    options = {}
                for item in config.items('Package'):
                    if item[0] == 'Version':
                        # intentionally storing in system because that's where fileVersion lives in kmp.json
                        system['fileVersion'] = item[1]
                    elif item[0] == 'ReadMeFile':
                        options['readmeFile'] = item[1]
                    elif item[0] == 'GraphicFile':
                        options['graphicFile'] = item[1]
                    elif item[0] == 'DisableKeepFonts':
                        options['disableKeepFonts'] = item[1]
                    elif item[0] == 'BothVersionsIncluded':
                        options['bothVersionsIncluded'] = item[1]
                    elif item[0] != 'ExecuteProgram':
                        print("Unknown item in Package:", item[0])
                system['keymanDeveloperVersion'] = ""
            elif "Keyboard" in section:
                keyboards = []
                keyboard = {}
                languages = []
                for item in config.items(section):
                    if item[0] == 'Name':
                        keyboard['name'] = item[1]
                    elif item[0] == 'ID':
                        keyboard['id'] = item[1]
                    elif item[0] == 'Version':
                        keyboard['version'] = item[1]
                    elif item[0] == 'OSKFont':
                        keyboard['oskFont'] = item[1]
                    elif item[0] == 'DisplayFont':
                        keyboard['displayFont'] = item[1]
                    elif item[0] == 'RTL':
                        keyboard['RTL'] = item[1]
                    elif "Language" in item[0]:
                        # only split on first ','
                        langid, langname = item[1].split(",", 1)
                        languages.append({'name': langname, 'id': langid})
                    else:
                        logging.warning("Unknown item in keyboard: %s", item[0])
                keyboard['languages'] = languages
                keyboards.append(keyboard)
            elif section == "Files":
                files = []
                for item in config.items(section):
                    splititem = item[1].split("\"")
                    kbfile = {'name': splititem[3], 'description': splititem[1],
                              'type': determine_filetype(None, splititem[3])}
                    files.append(kbfile)
            elif section == "InstallFiles":
                files = []
                for item in config.items(section):
                    kbfile = {'name': item[0], 'description': item[1], 'type': determine_filetype(None, item[0])}
                    files.append(kbfile)
            elif section == 'Install':
                if not options:
                    options = {}
                for item in config.items('Install'):
                    if item[0] == 'ReadmeFile':
                        options['readmeFile'] = item[1]
        kblist = []

        if not info:
            info = {}
        if 'version' not in info:
            info['version'] = {'description': "1.0"}
        # inf file may not have keyboards in legacy kmps so generate it if needed
        if files and not keyboards:
            id = "unknown"
            keyboards = []
            for kbfile in files:
                if kbfile['type'] == KMFileTypes.KM_KMX:
                    id = os.path.basename(os.path.splitext(kbfile['name'])[0])
                    keyboards = [{
                      'name': id,
                      'id': id,
                      'version': secure_lookup(info, 'version', 'description')
                    }]

        kblist = []
        if files:
            for k in keyboards:
                kblist.append(k['id'])
            for kbfile in files:
                if kbfile['type'] == KMFileTypes.KM_UNKNOWN:
                    kbfile['type'] = determine_filetype(kblist, kbfile['name'])
        logging.debug("finished parsing %s", inffile)

        if verbose:
            print_info(info)
            print_system(system)
            print_options(options)
            print_keyboards(keyboards)
            print_files(files, extracted_dir)

    return info, system, options, keyboards, files


def parsemetadata(jsonfile, verbose=False):
    """
    Parse the metadata in a kmp.json file.

    Args:
        jsonfile (str): Path to kmp.json
        verbose (bool, default False): verbose output

    Returns:
        list[5]: info, system, options, keyboards, files
            info (dict):
                name (dict):
                    description (str): KMP name
                copyright (dict):
                    description (str): KMP copyright
                version (dict):
                    description (str): KMP version
                author (dict):
                    description (str): KMP author
                    url (str): contact url for the author
            system (dict): System info
                fileVersion (str): Keyman file format version
                keymanDeveloperVersion (str): Keyman Developer version that compiled keyboard
            options (dict): Keyboard options
                readmeFile (str): README for the keyboard
            keyboards (list): Keyboards in the kmp
                name (str): Keyboard name
                id (str): Keyboard ID
                version (str): Keyboard version
                oskFont (str, optional): Recommended on screen keyboard font
                displayFont (str, optional): Recommended display font
                languages (list): Languages the keyboard is used for
                    name (str): Language name
                    id (str): Language ID
            files (list): Files in the kmp
                name (str): File name
                description (str): File description
    """
    info = system = keyboards = files = options = nonexistent = None
    extracted_dir = os.path.dirname(jsonfile)

    logging.debug("parsemetadata: reading file:%s dir:%s", jsonfile, extracted_dir)

    if os.path.isfile(jsonfile):
        with open(jsonfile, "r") as read_file:
            try:
                data = json.load(read_file)
            except JSONDecodeError as e:
                logging.error("parsemetadata: %s invalid: %s (line %d, col %d)",
                                 jsonfile, e.msg, e.lineno, e.colno)
                # Use empty details
                data = {"nonexistent": "none"}

            for x in data:
                if x == 'files':
                    files = data[x]
                elif x == 'info':
                    info = data[x]
                elif x == 'keyboards':
                    keyboards = data[x]
                elif x == 'nonexistent':
                    nonexistent = data[x]
                elif x == 'options':
                    options = data[x]
                elif x == 'system':
                    system = data[x]
            kblist = []
            if keyboards:
                kblist.extend(k['id'] for k in keyboards)
            if files:
                for kbfile in files:
                    kbfile['type'] = determine_filetype(kblist, kbfile['name'])
            if nonexistent is not None:
                logging.warning("This should not happen")
            if verbose:
                print_info(info)
                print_system(system)
                if options:
                    print_options(options)
                print_keyboards(keyboards)
                print_files(files, extracted_dir)
    return info, system, options, keyboards, files


def get_metadata(tmpdirname):
    """
    Get metadata from kmp.json if it exists.
    If it does not exist then will return get_and_convert_infdata

    Args:
        tmpdirname(str): temp directory to extract kmp

    Returns:
        list[5]: info, system, options, keyboards, files
            see kmpmetadata.parsemetadata for details
    """
    kmpjson = os.path.join(tmpdirname, "kmp.json")
    if os.path.isfile(kmpjson):
        return parsemetadata(kmpjson, False)
    else:
        return get_and_convert_infdata(tmpdirname)


def get_and_convert_infdata(tmpdirname):
    """
    Get metadata from kmp.inf if it exists.
    Convert it to kmp.json if possible

    Args:
        inputfile (str): path to kmp file
        tmpdirname(str): temp directory to extract kmp

    Returns:
        list[5]: info, system, options, keyboards, files
            see kmpmetadata.parseinfdata for details
    """
    kmpinf = os.path.join(tmpdirname, "kmp.inf")
    if os.path.isfile(kmpinf):
        info, system, options, keyboards, files = parseinfdata(kmpinf, False)
        j = infmetadata_to_json(info, system, options, keyboards, files)
        kmpjson = os.path.join(tmpdirname, "kmp.json")
        try:
            with open(kmpjson, "w") as write_file:
                print(j, file=write_file)
        except Exception as e:
            logging.warning('Exception %s writing metadata %s %s', type(e), kmpjson, e.args)
        return info, system, options, keyboards, files
    else:
        return None, None, None, None, None


def infmetadata_to_json(info, system, options, keyboards, files):
    jsonfiles = []
    if files:
        for entry in files:
            jsonfiles.append({"name": entry["name"], "description": entry["description"]})
    d = {"system": system, "options": options,
         "info": info, "keyboards": keyboards, "files": jsonfiles}
    return json.dumps(d, indent=2)


def main(argv):
    if len(sys.argv) != 2:
        logging.error("kmpmetadata.py <kmp.json> or <kmp.inf>")
        sys.exit(2)
    inputfile = sys.argv[1]

    if not os.path.isfile(inputfile):
        logging.error("kmpmetadata.py Input file ", inputfile, " not found.")
        logging.error("kmpmetadata.py <kmp.json> or <kmp.inf>")
        sys.exit(2)

    name, ext = os.path.splitext(inputfile)
    if ext == ".json":
        parsemetadata(inputfile, True)
    elif ext == ".inf":
        info, system, options, keyboards, files = parseinfdata(inputfile, True)
        jsontest = infmetadata_to_json(info, system, options, keyboards, files)
        print(jsontest)
    else:
        logging.error("kmpmetadata.py Input file must be json or inf.")
        logging.error("kmpmetadata.py <kmp.json> or <kmp.inf>")
        sys.exit(2)


if __name__ == "__main__":
    main(sys.argv[1:])
