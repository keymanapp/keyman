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


def delete_dir(dir: str) -> bool:
    if not os.path.isdir(dir):
        logging.error("%s is not a directory", dir)
        return False
    if os.path.islink(dir):
        os.unlink(dir)
    else:
        rmtree(dir)
    return True


def uninstall_kmp_shared(packageID):
    """
    Uninstall a kmp from /usr/local/share/keyman

    Args:
        packageID (str): Keyboard package ID
    """
    kbdir = get_keyboard_dir(InstallLocation.Shared, packageID)
    kbdocdir = get_keyman_doc_dir(InstallLocation.Shared, packageID)
    kbfontdir = get_keyman_font_dir(InstallLocation.Shared, packageID)

    logging.info("Uninstalling shared keyboard: %s", packageID)

    # need to uninstall from ibus for all lang and all kmx in kmp
    info, system, options, keyboards, files = get_metadata(kbdir)
    if keyboards:
        if is_gnome_shell():
            uninstall_keyboards_from_gnome(keyboards, kbdir)
        else:
            uninstall_keyboards_from_ibus(keyboards, kbdir)
    else:
        logging.warning("could not uninstall keyboards")

    if not os.path.isdir(kbdir):
        msg = _("Keyboard directory for %s does not exist." % packageID)
        logging.error(msg)
        return msg

    if not os.access(kbdir, os.X_OK | os.W_OK):  # Check for write access of keyman dir
        msg = _("You do not have permissions to uninstall the keyboard files. You need to run this with `sudo`")
        logging.error(msg)
        return msg
    if os.path.isdir(kbdocdir):
        if not os.access(kbdocdir, os.X_OK | os.W_OK):  # Check for write access of keyman doc dir
            msg = _("You do not have permissions to uninstall the documentation. You need to run this with `sudo`")
            logging.error(msg)
            return msg
        delete_dir(kbdocdir)
        logging.info("Removed documentation directory: %s", kbdocdir)
    else:
        logging.info("No documentation directory")
    if os.path.isdir(kbfontdir):
        if not os.access(kbfontdir, os.X_OK | os.W_OK):  # Check for write access of keyman fonts
            msg = _("You do not have permissions to uninstall the font files. You need to run this with `sudo`")
            logging.error(msg)
            return msg
        delete_dir(kbfontdir)
        logging.info("Removed font directory: %s", kbfontdir)
    else:
        logging.info("No font directory")

    delete_dir(kbdir)
    logging.info("Removed keyman directory: %s", kbdir)
    logging.info("Finished uninstalling shared keyboard: %s", packageID)
    return ''


def uninstall_keyboards_from_ibus(keyboards, packageDir):
    bus = get_ibus_bus()
    if bus or os.environ.get('SUDO_USER'):
        # install all kmx for first lang not just packageID
        for kb in keyboards:
            ibus_keyboard_id = get_ibus_keyboard_id(kb, packageDir)
            uninstall_from_ibus(bus, ibus_keyboard_id)
        restart_ibus(bus)
    else:
        logging.warning("could not uninstall keyboards from IBus")


def uninstall_keyboards_from_gnome(keyboards, packageDir):
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


def uninstall_kmp_user(packageID):
    """
    Uninstall a kmp from ~/.local/share/keyman

    Args:
        packageID (str): Keyboard package ID
    """
    msg = ''
    kbdir = get_keyboard_dir(InstallLocation.User, packageID)
    logging.info("Uninstalling local keyboard: %s", packageID)
    info, system, options, keyboards, files = get_metadata(kbdir)
    if keyboards:
        if is_fcitx_running():
            _uninstall_keyboards_from_fcitx5()
        elif is_gnome_shell():
            uninstall_keyboards_from_gnome(keyboards, kbdir)
        else:
            uninstall_keyboards_from_ibus(keyboards, kbdir)
    else:
        logging.warning("could not uninstall keyboards")
    if not os.path.isdir(kbdir):
        msg = _("Keyboard directory for %s does not exist." % packageID)
        logging.error(msg)
    else:
        delete_dir(kbdir)
        logging.info("Removed user keyman directory: %s", kbdir)
    fontdir = get_keyman_font_dir(InstallLocation.User, packageID)
    if os.path.isdir(fontdir):
        delete_dir(fontdir)
        logging.info("Removed user keyman font directory: %s", fontdir)
    logging.info("Finished uninstalling local keyboard: %s", packageID)
    return msg


def uninstall_kmp(packageID, sharedarea=False):
    """
    Uninstall a kmp

    Args:
        packageID (str): Keyboard package ID
        sharedarea (str): whether to uninstall from shared /usr/local or ~/.local
    """
    if sharedarea:
        msg = uninstall_kmp_shared(packageID)
    else:
        msg = uninstall_kmp_user(packageID)

    get_keyman_config_service().keyboard_list_changed()
    return msg
