#!/usr/bin/python3

from enum import Enum
import getpass
import logging
import os
import re
import subprocess
import time

import gi

from keyman_config.dbus_util import DBusSessionBusAddress, XdgRuntimeDir

gi.require_version('IBus', '1.0')
from gi.repository import IBus
from packaging import version

from keyman_config.gnome_keyboards_util import is_gnome_desktop
from keyman_config.gsettings import GSettings


class IbusDaemon(Enum):
    """
    States of the ibus-daemon
    """
    RUNNING = 0,        'ibus-daemon is running'
    NOT_RUNNING = 1,    'ibus-daemon is not running'
    MORE_THAN_ONE = 2,  'Error: more than one instance of ibus-daemon is running'
    ERROR_USER = 3,     'Error: invalid/unknown user'
    ERROR = 4,          'Error: got exception finding ibus-daemon'


class IbusUtil():
    def __init__(self) -> None:
        self.ibus_settings = GSettings('org.freedesktop.ibus.general')

    def read_preload_engines(self):
        return self.ibus_settings.get('preload-engines')

    def write_preload_engines(self, bus, preload_engines):
        self.ibus_settings.set('preload-engines', preload_engines, 'as')
        if bus:
            bus.preload_engines(preload_engines)


def get_ibus_bus():
    try:
        for i in range(5):
            bus = IBus.Bus()
            if bus.is_connected() and bus.is_global_engine_enabled():
                return bus
            bus.destroy()
            time.sleep(1)
    except Exception as e:
        logging.warning("Failed get bus")
        logging.warning(e)
    logging.info("could not find connected IBus.Bus")
    return None


def install_to_ibus(bus, keyboard_id):
    try:
        logging.debug("installing to ibus")
        ibusUtil = IbusUtil()
        preload_engines = ibusUtil.read_preload_engines()
        logging.debug('preload_engines before: %s', preload_engines)
        if keyboard_id not in preload_engines:
            # TODO: in the event preload_engines contains upper-case keyboards, we'll need to uninstall_from_ibus #1601
            preload_engines.append(keyboard_id)
        logging.debug('Setting preload_engines to: %s', preload_engines)
        ibusUtil.write_preload_engines(bus, preload_engines)
    except Exception as e:
        logging.warning("Failed to set up install %s to IBus", keyboard_id)
        logging.warning(e)


def uninstall_from_ibus(bus, keyboard_id):
    # need to uninstall for all installed langs
    try:
        logging.debug('Uninstalling from ibus')
        ibusUtil = IbusUtil()
        preload_engines = ibusUtil.read_preload_engines()
        logging.debug('preload_engines before: %s', preload_engines)
        if keyboard_id in preload_engines:
            preload_engines.remove(keyboard_id)
        logging.debug('Setting preload_engines to: %s', preload_engines)
        ibusUtil.write_preload_engines(bus, preload_engines)
    except Exception as e:
        logging.warning("Failed to uninstall keyboard %s", keyboard_id)
        logging.warning(e)


def restart_ibus_subp():
    logging.info("restarting IBus by subprocess")
    subprocess.run(["ibus", "restart"], check=True)


def verify_ibus_daemon(start):
    """
    Verify if ibus-daemon is running and optionally start it.

    Args:
        start (bool): True to start ibus-daemon if it is not running, otherwise False

    Returns:
        One of the IbusDaemon values
    """
    realuser = os.environ.get('SUDO_USER')
    user = realuser or getpass.getuser()
    retval = IbusDaemon.RUNNING

    if not user:
        logging.debug('SUDO_USER not set and getpass.getuser() returned None. '
                      'Skipping ibus-daemon check.')
        return IbusDaemon.ERROR_USER

    try:
        ibus_daemons = subprocess.run(('pgrep', '-u', user, 'ibus-daemon'),
                                      check=False, stdout=subprocess.PIPE).stdout.split()
        if len(ibus_daemons) <= 0:
            if start:
                _start_ibus_daemon(realuser)
                # give ibus a chance to start
                time.sleep(1)  # 1s
                retval = verify_ibus_daemon(False)
            else:
                retval = IbusDaemon.NOT_RUNNING
        elif len(ibus_daemons) > 1:
            logging.error('More than one ibus-daemon instance running! Keyman keyboards might '
                          'not work as expected. Please reboot your machine.')
            retval = IbusDaemon.MORE_THAN_ONE
        else:
            logging.debug('ibus already running')
    except subprocess.CalledProcessError as e:
        # Log critical error in order to track down #6237
        logging.critical('getting ibus-daemon failed (%s: %s)', type(e), e.args)
        if start:
            _start_ibus_daemon(realuser)
            return verify_ibus_daemon(False)
        retval = IbusDaemon.ERROR

    return retval


def get_ibus_version():
    """
    Get the installed IBus version string if available. This checks the `ibus` command
    output and parses the reported version.

    Returns the version as a string on success, an empty string if the version
    cannot be determined, or None if the `ibus` command is not available.

    Returns:
        str | None: The detected IBus version string, an empty string if
                    parsing fails, or None if the `ibus` command is not found.
    """
    try:
        ibus_version = subprocess.run(('ibus', 'version'), check=False,
                                      stdout=subprocess.PIPE).stdout
        if match := re.search(r'^IBus (.*)\n$', ibus_version.decode('utf-8')):
            logging.info('Running IBus version %s', match[1])
            return match[1]
        logging.warning('Unable to determine IBus version')
        return ''
    except FileNotFoundError:
        logging.warning('ibus command not found')
        return None


def _start_ibus_daemon(realuser):
    try:
        ibus_ver = get_ibus_version()
        if not ibus_ver:
            return

        if version.parse(ibus_ver) >= version.parse('1.5.28'):
            # IBus ~1.5.28 added the `start` command, so we use that if possible
            # and let IBus deal with the necessary parameters
            args = ['ibus', 'start', '-d']
        else:
            # If IBus is too old we have to start ibus-daemon directly and pass
            # what we think are the correct parameters
            args = ['ibus-daemon', '-d', '-r', '--xim']
            if is_gnome_desktop():
                # on Ubuntu 21.10 with Gnome the keyboards don't show in dropdown
                # list if we don't disable the panel
                args.extend(['--panel', 'disable'])

        if realuser:
            # we have been called with `sudo`. Start ibus-daemon for the real user.
            args = ['sudo', '-u', realuser,
                    DBusSessionBusAddress, XdgRuntimeDir, *args]
            logging.info(f'starting ibus-daemon for user {realuser} with args: {args}')
        else:
            logging.info(f'ibus-daemon not running. Starting it with args: {args}...')
        subprocess.run(args, check=True)
        logging.info('ibus-daemon started successfully')
    except Exception as e:
        logging.warning(f'Failed to start ibus-daemon: {e}')


def restart_ibus(bus=None):
    verify_ibus_daemon(False)
    if realuser := os.environ.get('SUDO_USER'):
        # we have been called with `sudo`. Restart ibus for the real user.
        logging.info(f'restarting IBus by subprocess for user {realuser}')
        subprocess.run(['sudo', '-u', realuser,
                        DBusSessionBusAddress, XdgRuntimeDir,
                        'ibus', 'restart'], check=False)
    else:
        logging.info('restarting IBus by subprocess')
        subprocess.run(['ibus', 'restart'], check=False)
    # give ibus a chance to shutdown (#6237)
    time.sleep(1)  # 1s
    verify_ibus_daemon(True)


def bus_has_engine(bus, name):
    engines = bus.get_engines_by_names([name])
    return len(engines)


def get_current_engine(bus):
    try:
        contextname = bus.current_input_context()
        inputContext = IBus.InputContext.get_input_context(contextname, bus.get_connection())
        if engine := inputContext.get_engine():
            return engine.get_name()
    except Exception as e:
        logging.warning("Failed to get current engine")
        logging.warning(e)


def change_to_keyboard(bus, keyboard_id):
    try:
        contextname = bus.current_input_context()
        inputContext = IBus.InputContext.get_input_context(contextname, bus.get_connection())
        if bus_has_engine(bus, keyboard_id) <= 0:
            logging.warning(f"Could not find engine {keyboard_id}")
        else:
            inputContext.set_engine(keyboard_id)
    except Exception as e:
        logging.warning("Failed to change keyboard")
        logging.warning(e)
