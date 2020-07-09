#!/usr/bin/python3

import logging
import os.path
from shutil import rmtree

from keyman_config.get_kmp import user_keyboard_dir, user_keyman_font_dir
from keyman_config.kmpmetadata import get_metadata
from keyman_config.ibus_util import uninstall_from_ibus, get_ibus_bus, restart_ibus
from keyman_config.gnome_keyboards_util import GnomeKeyboardsUtil, get_ibus_keyboard_id, is_gnome_shell


def uninstall_kmp_shared(packageID):
    """
    Uninstall a kmp from /usr/local/share/keyman

    Args:
        packageID (str): Keyboard package ID
    """
    kbdir = os.path.join('/usr/local/share/keyman', packageID)
    if not os.path.isdir(kbdir):
        logging.error("Keyboard directory for %s does not exist. Aborting", packageID)
        exit(3)

    kbdocdir = os.path.join('/usr/local/share/doc/keyman', packageID)
    kbfontdir = os.path.join('/usr/local/share/fonts/keyman', packageID)

    logging.info("Uninstalling shared keyboard: %s", packageID)
    if not os.access(kbdir, os.X_OK | os.W_OK):  # Check for write access of keyman dir
        logging.error(
            "You do not have permissions to uninstall the keyboard files. You need to run this with `sudo`")
        exit(3)
    if os.path.isdir(kbdocdir):
        if not os.access(kbdocdir, os.X_OK | os.W_OK):  # Check for write access of keyman doc dir
            logging.error(
                "You do not have permissions to uninstall the documentation. You need to run this with `sudo`")
            exit(3)
        rmtree(kbdocdir)
        logging.info("Removed documentation directory: %s", kbdocdir)
    else:
        logging.info("No documentation directory")
    if os.path.isdir(kbfontdir):
        if not os.access(kbfontdir, os.X_OK | os.W_OK):  # Check for write access of keyman fonts
            logging.error(
                "You do not have permissions to uninstall the font files. You need to run this with `sudo`")
            exit(3)
        rmtree(kbfontdir)
        logging.info("Removed font directory: %s", kbfontdir)
    else:
        logging.info("No font directory")

    # need to uninstall from ibus for all lang and all kmx in kmp
    info, system, options, keyboards, files = get_metadata(kbdir)
    if keyboards:
        if is_gnome_shell():
            uninstall_keyboards_from_gnome(keyboards, kbdir)
        else:
            uninstall_keyboards_from_ibus(keyboards, kbdir)
    else:
        logging.warning("could not uninstall keyboards")

    rmtree(kbdir)
    logging.info("Removed keyman directory: %s", kbdir)
    logging.info("Finished uninstalling shared keyboard: %s", packageID)


def uninstall_keyboards_from_ibus(keyboards, packageDir):
    bus = get_ibus_bus()
    if bus:
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
        match_id = ":%s" % get_ibus_keyboard_id(kb, packageDir, True)
        for (type, id) in sources:
            if type == 'ibus' and id.endswith(match_id):
                toRemove.append((type, id))

        for val in toRemove:
            sources.remove(val)

    gnomeKeyboardsUtil.write_input_sources(sources)


def uninstall_kmp_user(packageID):
    """
    Uninstall a kmp from ~/.local/share/keyman

    Args:
        packageID (str): Keyboard package ID
    """
    kbdir = user_keyboard_dir(packageID)
    if not os.path.isdir(kbdir):
        logging.error("Keyboard directory for %s does not exist. Aborting", packageID)
        exit(3)
    logging.info("Uninstalling local keyboard: %s", packageID)
    info, system, options, keyboards, files = get_metadata(kbdir)
    if keyboards:
        if is_gnome_shell():
            uninstall_keyboards_from_gnome(keyboards, kbdir)
        else:
            uninstall_keyboards_from_ibus(keyboards, kbdir)
    else:
        logging.warning("could not uninstall keyboards")
    rmtree(kbdir)
    logging.info("Removed user keyman directory: %s", kbdir)
    fontdir = os.path.join(user_keyman_font_dir(), packageID)
    if os.path.isdir(fontdir):
        rmtree(fontdir)
        logging.info("Removed user keyman font directory: %s", fontdir)
    logging.info("Finished uninstalling local keyboard: %s", packageID)


def uninstall_kmp(packageID, sharedarea=False):
    """
    Uninstall a kmp

    Args:
        packageID (str): Keyboard package ID
        sharedarea (str): whether to uninstall from shared /usr/local or ~/.local
    """
    if sharedarea:
        uninstall_kmp_shared(packageID)
    else:
        uninstall_kmp_user(packageID)
