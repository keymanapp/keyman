#!/usr/bin/python3

import json
import logging
import os
import packaging.version
import zipfile
from enum import Enum
from shutil import rmtree

from keyman_config import _, __version__, secure_lookup
from keyman_config.canonical_language_code_utils import CanonicalLanguageCodeUtils
from keyman_config.convertico import checkandsaveico, extractico
from keyman_config.custom_keyboards import CustomKeyboards
from keyman_config.dbus_util import get_keyman_config_service
from keyman_config.fcitx_util import is_fcitx_running
from keyman_config.get_kmp import (InstallLocation, get_keyboard_data,
                                   get_keyboard_dir, get_keyman_doc_dir,
                                   get_keyman_font_dir)
from keyman_config.gnome_keyboards_util import (GnomeKeyboardsUtil,
                                                get_ibus_keyboard_id,
                                                is_gnome_shell)
from keyman_config.ibus_util import get_ibus_bus, install_to_ibus, restart_ibus
from keyman_config.kmpmetadata import KMFileTypes, get_metadata
from keyman_config.kvk2ldml import convert_kvk_to_ldml, output_ldml

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

    def _check_keyman_dir(self, basedir, error_message):
        # check if keyman subdir exists
        keyman_dir = os.path.join(basedir, "keyman")
        if os.path.isdir(keyman_dir):
            # Check for write access of keyman dir to be able to create subdir
            if not os.access(keyman_dir, os.X_OK | os.W_OK):
                raise InstallError(InstallStatus.Abort, error_message)
        elif os.access(basedir, os.X_OK | os.W_OK):
            os.mkdir(keyman_dir)
        else:
            raise InstallError(InstallStatus.Abort, error_message)

    def _extract_package_id(self, inputfile):
        packageID, ext = os.path.splitext(os.path.basename(inputfile))
        return packageID.lower()

    def install_kmp_shared(self, inputfile, language=None):
        """
        Install a kmp file to /usr/local/share/keyman

        Args:
            inputfile (str): path to kmp file
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

        return self._install_kmp(inputfile, language, InstallLocation.Shared)

    def install_kmp_user(self, inputfile, language=None):
        return self._install_kmp(inputfile, language, InstallLocation.User)

    def _install_kmp(self, inputfile, language, area):
        self.packageID = self._extract_package_id(inputfile)
        self.packageDir = get_keyboard_dir(area, self.packageID)
        self.kmpdocdir = get_keyman_doc_dir(area, self.packageID)
        self.kmpfontdir = get_keyman_font_dir(area, self.packageID)

        if not os.path.isfile(inputfile):
            message = _("File {kmpfile} doesn't exist").format(kmpfile=inputfile)
            logging.error("install_kmp.py: %s", message)
            raise InstallError(InstallStatus.Abort, message)

        if not self._safeMakeDirs(self.packageDir):
            return

        extract_kmp(inputfile, self.packageDir)

        info, system, options, keyboards, files = get_metadata(self.packageDir)

        self._check_version(inputfile, system)

        if not keyboards:
            logging.error("install_kmp.py: error: No kmp.json or kmp.inf found in %s", inputfile)
            logging.info("Contents of %s:", inputfile)
            for o in os.listdir(self.packageDir):
                logging.info(o)
            rmtree(self.packageDir)
            message = _("No kmp.json or kmp.inf found in {packageFile}").format(
                packageFile=inputfile)
            raise InstallError(InstallStatus.Abort, message)

        # sourcery skip: extract-method
        logging.info("Installing %s", secure_lookup(info, 'name', 'description'))
        process_keyboard_data(self.packageID, self.packageDir)
        for kb in keyboards:
            if kb['id'] != self.packageID:
                process_keyboard_data(kb['id'], self.packageDir)

        if files is not None:
            self._install_files(keyboards, files)

        return self._install_keyboards(keyboards, self.packageDir, language)

    def _check_version(self, inputfile, system):
        if not system:
            return

        fileVersion = secure_lookup(system, 'fileVersion')
        if not fileVersion:
            fileVersion = '7.0'
        if packaging.version.parse(fileVersion) > packaging.version.parse(__version__):
            logging.error("install_kmp.py: error: %s requires a newer version of Keyman (%s)",
                          inputfile, fileVersion)
            rmtree(self.packageDir)
            message = _("{packageFile} requires Keyman {keymanVersion} or higher").format(
              packageFile=inputfile, keymanVersion=fileVersion)
            raise InstallError(InstallStatus.Abort, message)

    def _install_files(self, keyboards, files):
        for f in files:
            fpath = os.path.join(self.packageDir, f['name'])
            ftype = f['type']

            if ftype in [KMFileTypes.KM_DOC, KMFileTypes.KM_IMAGE]:
                # Special handling of doc and images to hard link them into doc dir
                logging.info("Installing %s as documentation", f['name'])
                if self._safeMakeDirs(self.kmpdocdir):
                    kmpdocpath = os.path.join(self.kmpdocdir, f['name'])
                    self._safeLinkFile(fpath, kmpdocpath)
            elif ftype == KMFileTypes.KM_FONT:
                # Special handling of font to hard link it into font dir
                logging.info("Installing %s as font", f['name'])
                if self._safeMakeDirs(self.kmpfontdir):
                    fontpath = os.path.join(self.kmpfontdir, f['name'])
                    self._safeLinkFile(fpath, fontpath)
            elif ftype == KMFileTypes.KM_OSK:
                # Special handling to convert kvk into LDML
                logging.info("Converting %s to LDML and installing both as as keyman file",
                             f['name'])
                name, ext = os.path.splitext(f['name'])
                ldml = convert_kvk_to_ldml(name, fpath)
                ldmlfile = os.path.join(self.packageDir, f"{name}.ldml")
                output_ldml(ldmlfile, ldml)
            elif ftype == KMFileTypes.KM_ICON:
                # Special handling of icon to convert to PNG
                logging.info("Converting %s to PNG and installing both as keyman files",
                             f['name'])
                checkandsaveico(fpath)
            elif ftype == KMFileTypes.KM_SOURCE:
                # TODO for the moment just leave it for ibus-keyman to ignore if it doesn't load
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

    def _safeMakeDirs(self, dir):
        if not os.path.isdir(dir):
            try:
                os.makedirs(dir)
            except NotADirectoryError:
                logging.error("Can't create directory %s", dir)
                return None
            except PermissionError:
                logging.error("No permissions to create directory %s", dir)
                return None
            except Exception as e:
                logging.warning('Exception %s creating %s %s', type(e), dir, e.args)
                return None
        return dir

    def _safeLinkFile(self, source, target):
        if not os.path.isfile(source):
            logging.error("Can't link file %s - source file doesn't exist", source)
            return
        if os.path.isfile(target):
            return
        if os.path.exists(target):
            logging.error("Can't link file %s to %s - something exists at the target", source, target)
            return
        os.link(source, target)

    def _normalize_language(self, supportedLanguages, language):
        if len(supportedLanguages) <= 0:
            return ''

        if not language:
            return language

        language = CanonicalLanguageCodeUtils.findBestTag(language, False, False)
        for supportedLanguage in supportedLanguages:
            tag = CanonicalLanguageCodeUtils.findBestTag(supportedLanguage['id'], False, False)
            if tag == language:
                return tag
        return None

    def _add_custom_keyboard(self, keyboard, package_dir, requested_language):
        if not requested_language:
            return None
        language = CanonicalLanguageCodeUtils.findBestTag(requested_language, False, False)
        new_keyboard = get_ibus_keyboard_id(keyboard, package_dir, language)
        CustomKeyboards().add(new_keyboard)
        return language

    def _install_keyboards(self, keyboards, packageDir, requested_language=None):
        language = requested_language
        # TODO: add other keyboards as well (#9757)
        firstKeyboard = keyboards[0]
        if secure_lookup(firstKeyboard, 'languages') and firstKeyboard['languages']:
            language = self._normalize_language(firstKeyboard['languages'], language)

        if not language:
            language = self._add_custom_keyboard(firstKeyboard, packageDir, requested_language)

        if is_fcitx_running():
            return self._install_keyboards_to_fcitx()

        restart_ibus()
        if is_gnome_shell():
            return self._install_keyboards_to_gnome(keyboards, packageDir, language)
        else:
            return self._install_keyboards_to_ibus(keyboards, packageDir, language)

    def _install_keyboards_to_ibus(self, keyboards, packageDir, language=None):
        # sourcery skip: split-or-ifs
        bus = get_ibus_bus()
        if bus or os.environ.get('SUDO_USER'):
            # install all kmx for first lang not just packageID
            for kb in keyboards:
                ibus_keyboard_id = get_ibus_keyboard_id(kb, packageDir, language)
                install_to_ibus(bus, ibus_keyboard_id)
            restart_ibus(bus)
            if bus:
                bus.destroy()
        else:
            logging.debug("could not install keyboards to IBus")
        return ''

    def _install_keyboards_to_gnome(self, keyboards, packageDir, language=None):
        gnomeKeyboardsUtil = GnomeKeyboardsUtil()
        sources = gnomeKeyboardsUtil.read_input_sources()

        # install all kmx for first lang not just packageID
        for kb in keyboards:
            ibus_keyboard_id = get_ibus_keyboard_id(kb, packageDir, language)
            input_source = ('ibus', ibus_keyboard_id)
            if input_source not in sources:
                sources.append(input_source)

        gnomeKeyboardsUtil.write_input_sources(sources)
        return ''

    def _install_keyboards_to_fcitx(self):
        return _('Please use fcitx5-configtool to add the keyboard to the desired group')


def extract_kmp(kmpfile, directory):
    try:
        with zipfile.ZipFile(kmpfile, "r") as zip_ref:
            zip_ref.extractall(directory)
    except zipfile.BadZipFile as e:
        raise InstallError(InstallStatus.Abort, e) from e


def process_keyboard_data(keyboardID, packageDir) -> None:
    if not (kbdata := get_keyboard_data(keyboardID)):
        return
    if not os.path.isdir(packageDir) and os.access(os.path.join(packageDir, os.pardir), os.X_OK | os.W_OK):
        try:
            os.makedirs(packageDir)
        except Exception as e:
            logging.warning('Exception %s creating %s %s', type(e), packageDir, e.args)

    if os.access(packageDir, os.X_OK | os.W_OK):
        try:
            with open(os.path.join(packageDir, f'{keyboardID}.json'), 'w') as outfile:
                json.dump(kbdata, outfile)
                logging.info("Installing api data file %s.json as keyman file", keyboardID)
        except Exception as e:
            logging.warning('Exception %s writing %s/%s.json %s', type(e), packageDir, keyboardID, e.args)


def install_kmp(inputfile, sharedarea=False, language=None):
    """
    Install a kmp file

    Args:
        inputfile (str): path to kmp file
        sharedarea(bool, default=False): whether install kmp to shared area or user directory
        language(str, default=None): language to install keyboard for
        has_ui(bool, default=True): whether we're displaying a window or running UI less from the command line
    """
    if sharedarea:
        return_value = InstallKmp().install_kmp_shared(inputfile, language)
    else:
        return_value = InstallKmp().install_kmp_user(inputfile, language)

    get_keyman_config_service().keyboard_list_changed()
    return return_value
