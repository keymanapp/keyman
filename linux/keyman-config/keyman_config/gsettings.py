#!/usr/bin/python3
import configparser
import logging
import os
import subprocess
import sys

from xdg.BaseDirectory import xdg_config_home
from gi.repository import Gio  # needs to come before gi.overrides.GLib!
from gi.overrides.GLib import Variant

from keyman_config import file_cleanup, get_dbus_started_for_session

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

    def _convert_array_to_variant(self, array, type_string):
        if len(array) == 0:
            return Variant(type_string, None)

        if type_string == 'as':
            return Variant('as', array)

        assert type_string == 'a(ss)'

        children = []
        for (type_string, id) in array:
            typeVariant = Variant.new_string(type_string)
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
                    output = process.stdout
                    if output.decode('utf-8').startswith('['):
                        value = eval(output)
                        return value

                logging.debug('Could not convert to sources')
                return None
        else:
            variant = self.schema.get_value(key)
            value = self._convert_variant_to_array(variant)
        return value

    def _get_schema_path(self):
        return self.schema_id.replace('.', '/')

    def _set_key_file(self, key, value, type_string):
        dconf_config_dir = os.path.join(xdg_config_home, 'dconf')
        os.makedirs(os.path.join(dconf_config_dir, 'user.d'), exist_ok=True)
        keyfile = file_cleanup.get('keyfile')
        if not keyfile:
            keyfile = os.path.join(dconf_config_dir, 'user.d', 'keyman-settings')
            file_cleanup.register('keyfile', keyfile)
        keyman_settings = configparser.ConfigParser()
        keyman_settings.read(keyfile, encoding='utf-8')
        keyman_settings[self._get_schema_path()] = {}
        keyman_settings[self._get_schema_path()][key] = f'@{type_string} {value}'
        with open(keyfile, mode='w', encoding='utf-8') as configfile:
            keyman_settings.write(configfile)
        # Update dconf database immediately for current dbus session, i.e.
        # current process, so that further reads and writes will work.
        # Cleanup, i.e. removal of the settings file, will be done when
        # the process exits. With the next login the user will see the
        # updated values.
        subprocess.run(['dconf', 'update', dconf_config_dir], check=False)

    def set(self, key, value, type_string) -> None:
        if self.is_sudo:
            variant = str(value)
            subprocess.run(
              ['sudo', '-H', '-u', os.environ.get('SUDO_USER'),
               f"DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/{os.environ.get('SUDO_UID')}/bus",
               'gsettings', 'set', self.schema_id, key, variant], check=False)
        elif get_dbus_started_for_session():
            self._set_key_file(key, value, type_string)
        else:
            variant = self._convert_array_to_variant(value, type_string)
            self.schema.set_value(key, variant)
            self.schema.sync()
