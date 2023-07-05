#!/usr/bin/python3
import logging
import os
import subprocess
import sys

from gi.repository import Gio  # needs to come before gi.overrides.GLib!
from gi.overrides.GLib import Variant

class GSettings():
    def __init__(self, schema_id):
        """
        Wrapper around Gio Settings that deals with running under sudo
        """
        self.schema_id = schema_id
        if os.environ.get('SUDO_USER'):
            self.is_sudo = True
            logging.debug('Running with sudo. Real user: %s', os.environ.get('SUDO_USER'))
        else:
            self.schema = Gio.Settings.new(self.schema_id)
            self.is_sudo = False
            logging.debug('Running as regular user')

    def _convert_variant_to_array(self, variant):
        if variant is None:
            return []

        if variant.get_type_string() == 'as':
            return variant.get_strv()

        assert variant.get_type_string() == 'a(ss)'

        values = []
        # Process variant of type "a(ss)" (array of tuples with two strings)
        nChildren = variant.n_children()
        for i in range(nChildren):
            # Process variant of type "(ss)" (tuple with two strings)
            val = variant.get_child_value(i)
            typeVariant = val.get_child_value(0)
            idVariant = val.get_child_value(1)
            values.append((typeVariant.get_string(), idVariant.get_string()))
        return values

    def _convert_array_to_variant(self, array, type):
        if len(array) == 0:
            return Variant(type, None)

        if type == 'as':
            return Variant('as', array)

        assert type == 'a(ss)'

        children = []
        for (type, id) in array:
            typeVariant = Variant.new_string(type)
            idVariant = Variant.new_string(id)
            child = Variant.new_tuple(typeVariant, idVariant)
            children.append(child)
        return Variant.new_array(None, children)

    def get(self, key):
        if self.is_sudo:
            args = ['sudo', '-H', '-u', os.environ.get('SUDO_USER'),
                    f"DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/{os.environ.get('SUDO_UID')}/bus",
                    'gsettings', 'get', self.schema_id, key]
            if sys.version_info.major <= 3 and sys.version_info.minor < 7:
                # capture_output got added in Python 3.7
                try:
                    output = subprocess.check_output(args)
                    value = eval(output)
                except subprocess.CalledProcessError:
                    value = None
                    logging.warning('Could not convert to sources')
            else:
                process = subprocess.run(args, capture_output=True)
                if process.returncode == 0:
                    value = eval(process.stdout)
                else:
                    value = None
                    logging.warning('Could not convert to sources')
        else:
            variant = self.schema.get_value(key)
            value = self._convert_variant_to_array(variant)
        return value

    def set(self, key, value, type) -> None:
        if self.is_sudo:
            variant = str(value)
            subprocess.run(
              ['sudo', '-H', '-u', os.environ.get('SUDO_USER'),
               f"DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/{os.environ.get('SUDO_UID')}/bus",
               'gsettings', 'set', self.schema_id, key, variant])
        else:
            variant = self._convert_array_to_variant(value, type)
            self.schema.set_value(key, variant)
