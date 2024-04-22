#!/usr/bin/python3

import getpass
import logging
import os
from shutil import rmtree

from keyman_config import _
from keyman_config.custom_keyboards import CustomKeyboards
from keyman_config.dbus_util import get_keyman_config_service
from keyman_config.fcitx_util import is_fcitx_running
from keyman_config.get_kmp import (InstallLocation, get_keyboard_dir,
                                   get_keyman_doc_dir, get_keyman_font_dir)
from keyman_config.gnome_keyboards_util import (GnomeKeyboardsUtil,
                                                get_ibus_keyboard_id,
                                                is_gnome_desktop)
from keyman_config.gsettings import GSettings
from keyman_config.ibus_util import IbusUtil, get_ibus_bus, restart_ibus
from keyman_config.kmpmetadata import get_metadata


error_dirs = set()


def _delete_dir(dir: str) -> bool:
    if not os.path.isdir(dir):
        logging.error("%s is not a directory", dir)
        return False
    if os.path.islink(dir):
        os.unlink(dir)
    else:
        rmtree(dir)
    return True


def _uninstall_dir(what, dir):
    if os.path.isdir(dir):
        if not os.access(dir, os.X_OK | os.W_OK):
            error_dirs.add(dir)
            return

        _delete_dir(dir)
        logging.info('Removed %s directory %s', what, dir)
    else:
        logging.info('No %s directory %s', what, dir)

def _uninstall_kmp_common(location, packageID, removeLanguages):
    kbdir = get_keyboard_dir(location, packageID)
    kbdocdir = get_keyman_doc_dir(location, packageID)
    kbfontdir = get_keyman_font_dir(location, packageID)

    where = 'shared' if location == InstallLocation.Shared else 'user'
    info, system, options, keyboards, files = get_metadata(kbdir)
    if removeLanguages:
        logging.info(f'Uninstalling {where} keyboard: "{packageID}"')
        if keyboards:
            if is_fcitx_running():
                _uninstall_keyboards_from_fcitx5()
            elif is_gnome_desktop():
                _uninstall_keyboards_from_gnome(keyboards, kbdir)
            else:
                _uninstall_keyboards_from_ibus(keyboards, kbdir)

            logging.debug('Removing custom keyboards:')
            custom_keyboards = CustomKeyboards()
            for kb in keyboards:
                kb_path = os.path.join(kbdir, kb["id"] + ".kmx")
                logging.debug(f'    removing {kb["id"]} ({kb_path})')
                custom_keyboards.remove(kb_path)
        else:
            logging.info(f'Could not uninstall {where} keyboard "{packageID}" from list of keyboards')

    else:
        logging.info(f'Replacing {where} keyboard: "{packageID}"')

    if not os.path.isdir(kbdir):
        logging.info(f'Keyboard directory {kbdir} for "{packageID}" does not exist.')
        logging.warning(f'Cannot uninstall non-existing {where} keyboard "{packageID}" for user {getpass.getuser()}')

    _uninstall_dir('Keyman keyboards', kbdir)
    _uninstall_dir('documentation', kbdocdir)
    _uninstall_dir('font', kbfontdir)

    if error_dirs:
        msg = _('You do not have permission to uninstall the files in %s. You need to run this with `sudo`.') % ', '.join(error_dirs)
        logging.error(msg)
        return msg

    logging.info(f'Finished uninstalling {where} keyboard: "{packageID}"')
    return ''


def _uninstall_keyboards_from_ibus(keyboards, packageDir):
    bus = get_ibus_bus()
    ibusUtil = IbusUtil()
    sources = ibusUtil.read_preload_engines()
    if not sources:
        return

    # uninstall all specified keyboards for all languages
    for kb in keyboards:
        # keyboard ids are similar to `km:/path/to/keyman/khmer_angkor/khmer_angkor.kmx`
        match_id = ":%s" % get_ibus_keyboard_id(kb, packageDir, ignore_language=True)
        toRemove = [id for id in sources if id.endswith(match_id)]
        for val in toRemove:
            sources.remove(val)

    ibusUtil.write_preload_engines(bus, sources)
    restart_ibus(bus)


def _uninstall_keyboards_from_gnome(keyboards, packageDir):
    gnomeKeyboardsUtil = GnomeKeyboardsUtil()
    sources = gnomeKeyboardsUtil.read_input_sources()

    # uninstall all kmx for all languages
    for kb in keyboards:
        ibus_keyboard_id = get_ibus_keyboard_id(kb, packageDir)
        _tuple = ('ibus', ibus_keyboard_id)
        if _tuple in sources:
            sources.remove(_tuple)

        match_id = ":%s" % get_ibus_keyboard_id(kb, packageDir, ignore_language=True)
        toRemove = []
        for (type, id) in sources:
            if type == 'ibus' and id.endswith(match_id):
                toRemove.append((type, id))

        for val in toRemove:
            sources.remove(val)

    gnomeKeyboardsUtil.write_input_sources(sources)


def _uninstall_keyboards_from_fcitx5():
    return _('Please use fcitx5-configtool to remove the keyboard from the group')


def uninstall_kmp(packageID, sharedarea=False, removeLanguages=True):
    """
    Uninstall a Keyman keyboard package

    Args:
        packageID (str): Keyboard package ID
        sharedarea (boolean): whether to uninstall from shared /usr/local or ~/.local
        removeLanguages (boolean): if True also uninstall the languages associated with
            the keyboard, otherwise only remove keyboard (i.e. in preparation for
            replacing the keyboard with a newer version)
    """
    if sharedarea:
        msg = _uninstall_kmp_common(InstallLocation.Shared, packageID, removeLanguages)
    else:
        msg = _uninstall_kmp_common(InstallLocation.User, packageID, removeLanguages)

    get_keyman_config_service().keyboard_list_changed()
    return msg
