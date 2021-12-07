#!/usr/bin/python3
import logging
import os
import subprocess
from gi.repository import Gio

from gi.overrides.GLib import Variant


class GnomeKeyboardsUtil():
    def __init__(self):
        if os.environ.get('SUDO_USER'):
            self.input_sources = None
            self.is_sudo = True
            logging.debug('Running with sudo. Real user: %s', os.environ.get('SUDO_USER'))
        else:
            self.input_sources = Gio.Settings.new("org.gnome.desktop.input-sources")
            self.is_sudo = False
            logging.debug('Running as regular user')

    def read_input_sources(self):
        if self.is_sudo:
            process = subprocess.run(
                ['sudo', '-H', '-u', os.environ.get('SUDO_USER'),
                 'DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/%s/bus' % os.environ.get('SUDO_UID'),
                 'gsettings', 'get', 'org.gnome.desktop.input-sources', 'sources'],
                capture_output=True)
            if process.returncode == 0:
                sources = eval(process.stdout)
                logging.debug('sources before change: %s', sources)
            else:
                sources = None
                logging.warning('Could not convert to sources')
        else:
            sourcesVal = self.input_sources.get_value("sources")
            sources = self._convert_variant_to_array(sourcesVal)
        return sources

    def write_input_sources(self, sources):
        if self.is_sudo:
            logging.debug('Setting sources to: %s', sources)
            sourcesVal = str(sources)
            subprocess.run(
                ['sudo', '-H', '-u', os.environ.get('SUDO_USER'),
                 'DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/%s/bus' % os.environ.get('SUDO_UID'),
                 'gsettings', 'set', 'org.gnome.desktop.input-sources', 'sources', sourcesVal])
        else:
            sourcesVal = self._convert_array_to_variant(sources)
            self.input_sources.set_value("sources", sourcesVal)

    def _convert_variant_to_array(self, variant):
        if variant is None:
            return []

        values = []
        # Process variant of type "a(ss)" (array of tuples with two strings)
        nChildren = variant.n_children()
        for i in range(nChildren):
            # Process variant of type "(ss)" (tuple with two strings)
            val = variant.get_child_value(i)
            typeVariant = val.get_child_value(0)
            type = typeVariant.get_string()
            idVariant = val.get_child_value(1)
            id = idVariant.get_string()
            values.append((type, id))
        return values

    def _convert_array_to_variant(self, array):
        if len(array) == 0:
            return Variant('a(ss)', None)

        children = []
        for (type, id) in array:
            typeVariant = Variant.new_string(type)
            idVariant = Variant.new_string(id)
            child = Variant.new_tuple(typeVariant, idVariant)
            children.append(child)
        return Variant.new_array(None, children)


__is_gnome_shell = None


def is_gnome_shell():
    global __is_gnome_shell

    if __is_gnome_shell is None:
        code = os.system('pidof gnome-shell >/dev/null 2>&1')
        __is_gnome_shell = (code == 0)
    return __is_gnome_shell


def _reset_gnome_shell():
    # used in unit tests
    global __is_gnome_shell

    __is_gnome_shell = None


def get_ibus_keyboard_id(keyboard, packageDir, language=None, ignore_language=False):
    kmx_file = os.path.join(packageDir, keyboard['id'] + ".kmx")
    if ignore_language:
        return kmx_file
    if language is not None and language != '':
        logging.debug(language)
        return "%s:%s" % (language, kmx_file)
    if "languages" in keyboard and len(keyboard["languages"]) > 0:
        logging.debug(keyboard["languages"][0])
        return "%s:%s" % (keyboard["languages"][0]['id'], kmx_file)
    return kmx_file
