#!/usr/bin/python3

import gi
import logging
import subprocess

gi.require_version('IBus', '1.0')
from gi.repository import IBus, Gio


def get_ibus_bus():
    try:
        for i in range(10000):
            bus = IBus.Bus()
            if bus.is_connected() and bus.is_global_engine_enabled():
                return bus
            bus.destroy()
    except Exception as e:
        logging.warning("Failed get bus")
        logging.warning(e)
    logging.warning("could not find connected IBus.Bus")
    return None


def install_to_ibus(bus, keyboard_id):
    try:
        # keyboard_id = "%s:%s" % (lang, kmx_file)
        # logging.debug("getting bus")
        # bus = IBus.Bus()
        logging.debug("installing to ibus")
        ibus_settings = Gio.Settings.new("org.freedesktop.ibus.general")
        preload_engines = ibus_settings.get_strv("preload-engines")
        logging.debug(preload_engines)
        if keyboard_id not in preload_engines:
            # TODO: in the event preload_engines contains upper-case keyboards, we'll need to uninstall_from_ibus #1601
            preload_engines.append(keyboard_id)
        logging.debug(preload_engines)
        ibus_settings.set_strv("preload-engines", preload_engines)
        bus.preload_engines(preload_engines)
    except Exception as e:
        logging.warning("Failed to set up install %s to IBus", keyboard_id)
        logging.warning(e)


def uninstall_from_ibus(bus, keyboard_id):
    # need to uninstall for all installed langs
    try:
        # logging.debug("getting bus")
        # bus = IBus.Bus()
        ibus_settings = Gio.Settings.new("org.freedesktop.ibus.general")
        preload_engines = ibus_settings.get_strv("preload-engines")
        logging.debug(preload_engines)
        if keyboard_id in preload_engines:
            preload_engines.remove(keyboard_id)
        logging.debug(preload_engines)
        ibus_settings.set_strv("preload-engines", preload_engines)
        bus.preload_engines(preload_engines)
    except Exception as e:
        logging.warning("Failed to uninstall keyboard %s", keyboard_id)
        logging.warning(e)


def restart_ibus_subp():
    logging.info("restarting IBus by subprocess")
    subprocess.run(["ibus", "restart"])


def restart_ibus(bus=None):
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
