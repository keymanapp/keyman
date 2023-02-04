#!/usr/bin/python3

import logging
import os
from shutil import rmtree

from keyman_config import _
from keyman_config.dbus_util import get_keyman_config_service
from keyman_config.fcitx_util import is_fcitx_running
from keyman_config.get_kmp import (InstallLocation, get_keyboard_dir,
                                   get_keyman_doc_dir, get_keyman_font_dir)
from keyman_config.gnome_keyboards_util import (GnomeKeyboardsUtil,
                                                get_ibus_keyboard_id,
                                                is_gnome_shell)
from keyman_config.ibus_util import (get_ibus_bus, restart_ibus,
                                     uninstall_from_ibus)
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


def _uninstall_kmp_common(location, packageID):
    kbdir = get_keyboard_dir(location, packageID)
    kbdocdir = get_keyman_doc_dir(location, packageID)
    kbfontdir = get_keyman_font_dir(location, packageID)

    where = 'local'
    if location == InstallLocation.Shared:
        where = 'shared'

    logging.info("Uninstalling %s keyboard: %s", where, packageID)
    info, system, options, keyboards, files = get_metadata(kbdir)
    if keyboards:
        if is_fcitx_running():
            _uninstall_keyboards_from_fcitx5()
        elif is_gnome_shell():
            _uninstall_keyboards_from_gnome(keyboards, kbdir)
        else:
            _uninstall_keyboards_from_ibus(keyboards, kbdir)
    else:
        logging.warning("could not uninstall keyboards")

    if not os.path.isdir(kbdir):
        logging.error('Keyboard directory %s for %s does not exist.', kbdir, packageID)

    _uninstall_dir('Keyman keyboards', kbdir)
    _uninstall_dir('documentation', kbdocdir)
    _uninstall_dir('font', kbfontdir)

    if error_dirs:
        msg = _('You do not have permission to uninstall the files in %s. You need to run this with `sudo`.') % ', '.join(error_dirs)
        logging.error(msg)
        return msg

    logging.info("Finished uninstalling %s keyboard: %s", where, packageID)
    return ''

def _uninstall_keyboards_from_ibus(keyboards, packageDir):
    bus = get_ibus_bus()
    if bus or os.environ.get('SUDO_USER'):
        # install all kmx for first lang not just packageID
        for kb in keyboards:
            ibus_keyboard_id = get_ibus_keyboard_id(kb, packageDir)
            uninstall_from_ibus(bus, ibus_keyboard_id)
        restart_ibus(bus)
    else:
        logging.warning("could not uninstall keyboards from IBus")


def _uninstall_keyboards_from_gnome(keyboards, packageDir):
    gnomeKeyboardsUtil = GnomeKeyboardsUtil()
    sources = gnomeKeyboardsUtil.read_input_sources()

    # uninstall all kmx for all languages
    for kb in keyboards:
        ibus_keyboard_id = get_ibus_keyboard_id(kb, packageDir)
        tuple = ('ibus', ibus_keyboard_id)
        if tuple in sources:
            sources.remove(tuple)

        toRemove = []
        match_id = ":%s" % get_ibus_keyboard_id(kb, packageDir, ignore_language=True)
        for (type, id) in sources:
            if type == 'ibus' and id.endswith(match_id):
                toRemove.append((type, id))

        for val in toRemove:
            sources.remove(val)

    gnomeKeyboardsUtil.write_input_sources(sources)


def _uninstall_keyboards_from_fcitx5():
    return _('Please use fcitx5-configtool to remove the keyboard from the group')


def uninstall_kmp(packageID, sharedarea=False):
    """
    Uninstall a kmp

    Args:
        packageID (str): Keyboard package ID
        sharedarea (str): whether to uninstall from shared /usr/local or ~/.local
    """
    if sharedarea:
        msg = _uninstall_kmp_common(InstallLocation.Shared, packageID)
    else:
        msg = _uninstall_kmp_common(InstallLocation.User, packageID)

    get_keyman_config_service().keyboard_list_changed()
    return msg
