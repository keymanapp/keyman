#!/usr/bin/python3

import json
import logging
import os.path
import zipfile
from shutil import rmtree
from enum import Enum

from keyman_config import _
from keyman_config.canonical_language_code_utils import CanonicalLanguageCodeUtils
from keyman_config.get_kmp import get_keyboard_data, user_keyboard_dir, user_keyman_font_dir
from keyman_config.kmpmetadata import get_metadata, KMFileTypes
from keyman_config.convertico import extractico, checkandsaveico
from keyman_config.kvk2ldml import convert_kvk_to_ldml, output_ldml
from keyman_config.ibus_util import install_to_ibus, restart_ibus, get_ibus_bus
from keyman_config.gnome_keyboards_util import GnomeKeyboardsUtil, get_ibus_keyboard_id, is_gnome_shell

# TODO userdir install
# special processing for kmn if needed
# TODO optionally standardise throughout on variable names
# packageID for kmps and keyboardID for keyboards
# see https://docs.google.com/\
#   document/d/1sj7W6pCiN-_iRss5iRdib1aHaSTmYoLIueQSKJeNy8Q/edit#heading=h.mq0rc28mf031


class InstallStatus(Enum):
    Continue = 0
    Warning = 1
    Abort = 2


class InstallError(Exception):
    """Exception raised for errors in KMP installation.

    Attributes:
        status -- InstallStatus for what to do when the error occurrs
        message -- explanation of the error
    """

    def __init__(self, status, message):
        self.status = status
        self.message = message


class InstallKmp():
    def __init__(self):
        self.packageID = ''
        self.packageDir = ''
        self.kmpdocdir = ''
        self.kmpfontdir = ''

    def _check_keyman_dir(basedir, error_message):
        # check if keyman subdir exists
        keyman_dir = os.path.join(basedir, "keyman")
        if os.path.isdir(keyman_dir):
            # Check for write access of keyman dir to be able to create subdir
            if not os.access(keyman_dir, os.X_OK | os.W_OK):
                raise InstallError(InstallStatus.Abort, error_message)
        else:
            # Check for write access of basedir and create keyman subdir if we can
            if not os.access(basedir, os.X_OK | os.W_OK):
                raise InstallError(InstallStatus.Abort, error_message)
            os.mkdir(keyman_dir)

    def _extract_package_id(inputfile):
        packageID, ext = os.path.splitext(os.path.basename(inputfile))
        return packageID.lower()

    def install_kmp_shared(self, inputfile, online=False, language=None):
        """
        Install a kmp file to /usr/local/share/keyman

        Args:
            inputfile (str): path to kmp file
            online (bool, default=False): whether to attempt to get online keyboard data
        """
        self._check_keyman_dir(
            '/usr/local/share',
            _("You do not have permissions to install the keyboard files to the shared area "
              "/usr/local/share/keyman"))
        self._check_keyman_dir(
            '/usr/local/share/doc',
            _("You do not have permissions to install the documentation to the shared "
              "documentation area /usr/local/share/doc/keyman"))
        self._check_keyman_dir(
            '/usr/local/share/fonts',
            _("You do not have permissions to install the font files to the shared font area "
              "/usr/local/share/fonts"))

        self._install_kmp(inputfile, online, language)

    def install_kmp_user(self, inputfile, online=False, language=None):
        self.packageID = self._extract_package_id(inputfile)
        self.packageDir = user_keyboard_dir(self.packageID)
        self.kmpdocdir = self.packageDir
        self.kmpfontdir = os.path.join(user_keyman_font_dir(), self.packageID)

        self._install_kmp(inputfile, online, language)

    def _install_kmp(self, inputfile, online, language):
        if not os.path.isdir(self.packageDir):
            os.makedirs(self.packageDir)

        if not os.path.isfile(inputfile):
            message = _("File {kmpfile} doesn't exist").format(kmpfile=inputfile)
            logging.error("install_kmp.py: %s", message)
            raise InstallError(InstallStatus.Abort, message)

        self.extract_kmp(inputfile, self.packageDir)
        restart_ibus()

        info, system, options, keyboards, files = get_metadata(self.packageDir)

        if keyboards:
            logging.info("Installing %s", info['name']['description'])
            if online:
                process_keyboard_data(self.packageID, self.packageDir)
                for kb in keyboards:
                    if kb['id'] != self.packageID:
                        process_keyboard_data(kb['id'], self.packageDir)

            for f in files:
                fpath = os.path.join(self.packageDir, f['name'])
                ftype = f['type']

                if ftype == KMFileTypes.KM_DOC or ftype == KMFileTypes.KM_IMAGE:
                    # Special handling of doc and images to hard link them into doc dir
                    logging.info("Installing %s as documentation", f['name'])
                    if not os.path.isdir(self.kmpdocdir):
                        os.makedirs(self.kmpdocdir)
                    kmpdocpath = os.path.join(self.kmpdocdir, f['name'])
                    if not os.path.isfile(kmpdocpath):
                        os.link(fpath, kmpdocpath)
                elif ftype == KMFileTypes.KM_FONT:
                    # Special handling of font to hard link it into font dir
                    logging.info("Installing %s as font", f['name'])
                    if not os.path.isdir(self.kmpfontdir):
                        os.makedirs(self.kmpfontdir)
                    fontpath = os.path.join(self.kmpfontdir, f['name'])
                    if not os.path.isfile(fontpath):
                        os.link(fpath, fontpath)
                elif ftype == KMFileTypes.KM_OSK:
                    # Special handling to convert kvk into LDML
                    logging.info("Converting %s to LDML and installing both as as keyman file",
                                 f['name'])
                    ldml = convert_kvk_to_ldml(fpath)
                    name, ext = os.path.splitext(f['name'])
                    ldmlfile = os.path.join(self.packageDir, name + ".ldml")
                    output_ldml(ldmlfile, ldml)
                elif ftype == KMFileTypes.KM_ICON:
                    # Special handling of icon to convert to PNG
                    logging.info("Converting %s to PNG and installing both as keyman files",
                                 f['name'])
                    checkandsaveico(fpath)
                elif ftype == KMFileTypes.KM_SOURCE:
                    # TODO for the moment just leave it for ibus-kmfl to ignore if it doesn't load
                    pass
                elif ftype == KMFileTypes.KM_KMX:
                    # Sanitize keyboard filename if not lower case
                    kmx_id, ext = os.path.splitext(os.path.basename(f['name']))
                    for kb in keyboards:
                        if kmx_id.lower() == kb['id'] and kmx_id != kb['id']:
                            os.rename(os.path.join(self.packageDir, f['name']),
                                      os.path.join(self.packageDir, kb['id'] + '.kmx'))
                            fpath = os.path.join(self.packageDir, kb['id'] + '.kmx')
                    extractico(fpath)

            self.install_keyboards(keyboards, self.packageDir, language)
        else:
            logging.error("install_kmp.py: error: No kmp.json or kmp.inf found in %s", inputfile)
            logging.info("Contents of %s:", inputfile)
            for o in os.listdir(self.packageDir):
                logging.info(o)
            rmtree(self.packageDir)
            message = _("No kmp.json or kmp.inf found in {packageFile}").format(
                packageFile=inputfile)
            raise InstallError(InstallStatus.Abort, message)

    def _normalize_language(self, supportedLanguages, language):
        if len(supportedLanguages) <= 0:
            return ''

        if not language:
            return language

        language = CanonicalLanguageCodeUtils.findBestTag(language, False, True)
        for supportedLanguage in supportedLanguages:
            id = CanonicalLanguageCodeUtils.findBestTag(supportedLanguage['id'], False, True)
            if id == language:
                return id
        return None

    def install_keyboards(self, keyboards, packageDir, language=None):
        firstKeyboard = keyboards[0]
        if firstKeyboard and 'languages' in firstKeyboard and len(firstKeyboard['languages']) > 0:
                language = self._normalize_language(firstKeyboard['languages'], language)

        if is_gnome_shell():
                self._install_keyboards_to_gnome(keyboards, packageDir, language)
        else:
                self._install_keyboards_to_ibus(keyboards, packageDir, language)

    def _install_keyboards_to_ibus(self, keyboards, packageDir, language=None):
        bus = get_ibus_bus()
        if bus:
            # install all kmx for first lang not just packageID
            for kb in keyboards:
                ibus_keyboard_id = get_ibus_keyboard_id(kb, packageDir, language)
                install_to_ibus(bus, ibus_keyboard_id)
            restart_ibus(bus)
            bus.destroy()
        else:
            logging.debug("could not install keyboards to IBus")

    def _install_keyboards_to_gnome(self, keyboards, packageDir, language=None):
        gnomeKeyboardsUtil = GnomeKeyboardsUtil()
        sources = gnomeKeyboardsUtil.read_input_sources()

        # install all kmx for first lang not just packageID
        for kb in keyboards:
            ibus_keyboard_id = get_ibus_keyboard_id(kb, packageDir, language)
            sources.append(('ibus', ibus_keyboard_id))

        gnomeKeyboardsUtil.write_input_sources(sources)


def extract_kmp(kmpfile, directory):
    with zipfile.ZipFile(kmpfile, "r") as zip_ref:
        zip_ref.extractall(directory)


def process_keyboard_data(keyboardID, packageDir):
    kbdata = get_keyboard_data(keyboardID)
    if kbdata:
        if not os.path.isdir(packageDir):
            os.makedirs(packageDir)

        with open(os.path.join(packageDir, keyboardID + '.json'), 'w') as outfile:
            json.dump(kbdata, outfile)
            logging.info("Installing api data file %s.json as keyman file", keyboardID)
    # else:
    # 	message = "install_kmp.py: error: cannot download keyboard data so not installing."
    # 	rmtree(kbdir)
    # 	raise InstallError(InstallStatus.Abort, message)


def install_kmp(inputfile, online=False, sharedarea=False, language=None):
    """
    Install a kmp file

    Args:
        inputfile (str): path to kmp file
        online(bool, default=False): whether to attempt to get online keyboard data
        sharedarea(bool, default=False): whether install kmp to shared area or user directory
    """
    if sharedarea:
        InstallKmp().install_kmp_shared(inputfile, online, language)
    else:
        InstallKmp().install_kmp_user(inputfile, online, language)
