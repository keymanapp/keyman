#!/usr/bin/python3

import os
import time
import gi
import logging
import subprocess

gi.require_version('IBus', '1.0')
from gi.repository import IBus, Gio


class IbusUtil():
    def __init__(self) -> None:
        if os.environ.get('SUDO_USER'):
            self.ibus_settings = None
            self.is_sudo = True
            logging.debug('Running with sudo. Real user: %s', os.environ.get('SUDO_USER'))
        else:
            self.ibus_settings = Gio.Settings.new("org.freedesktop.ibus.general")
            self.is_sudo = False
            logging.debug('Running as regular user')

    def read_preload_engines(self):
        if os.environ.get('SUDO_USER'):
            process = subprocess.run(
                ['sudo', '-H', '-u', os.environ.get('SUDO_USER'),
                 'DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/%s/bus' % os.environ.get('SUDO_UID'),
                 'gsettings', 'get', 'org.freedesktop.ibus.general', 'preload-engines'],
                capture_output=True)
            if process.returncode == 0:
                preload_engines = eval(process.stdout)
            else:
                preload_engines = None
                logging.warning('Could not convert to preload_engines')
        else:
            preload_engines = self.ibus_settings.get_strv("preload-engines")
        return preload_engines

    def write_preload_engines(self, bus, preload_engines):
        if os.environ.get('SUDO_USER'):
            sourcesVal = str(preload_engines)
            subprocess.run(
                ['sudo', '-H', '-u', os.environ.get('SUDO_USER'),
                 'DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/%s/bus' % os.environ.get('SUDO_UID'),
                 'gsettings', 'set', 'org.freedesktop.ibus.general', 'preload-engines', sourcesVal])
        else:
            self.ibus_settings.set_strv("preload-engines", preload_engines)
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
    logging.warning("could not find connected IBus.Bus")
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
    subprocess.run(["ibus", "restart"])


def restart_ibus(bus=None):
    realuser = os.environ.get('SUDO_USER')
    if realuser:
        # we have been called with `sudo`. Restart ibus for the real user.
        logging.info('restarting IBus by subprocess for user %s', realuser)
        subprocess.run(['sudo', '-u', realuser, 'ibus', 'restart'])
    else:
        try:
            if not bus:
                bus = get_ibus_bus()
            logging.info("restarting IBus")
            bus.exit(True)
            bus.destroy()
        except Exception as e:
            logging.warning("Failed to restart IBus")
            logging.warning(e)


def bus_has_engine(bus, name):
    engines = bus.get_engines_by_names([name])
    return len(engines)


def get_current_engine(bus):
    try:
        contextname = bus.current_input_context()
        ic = IBus.InputContext.get_input_context(contextname, bus.get_connection())
        engine = ic.get_engine()
        if engine:
            return engine.get_name()
    except Exception as e:
        logging.warning("Failed to get current engine")
        logging.warning(e)


def change_to_keyboard(bus, keyboard_id):
    try:
        contextname = bus.current_input_context()
        ic = IBus.InputContext.get_input_context(contextname, bus.get_connection())
        if bus_has_engine(bus, keyboard_id) <= 0:
            logging.warning("Could not find engine %s" % keyboard_id)
        else:
            ic.set_engine(keyboard_id)
    except Exception as e:
        logging.warning("Failed to change keyboard")
        logging.warning(e)
