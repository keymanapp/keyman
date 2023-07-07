#!/usr/bin/python3
import logging
import os

from keyman_config.gsettings import GSettings


class GnomeKeyboardsUtil():
    def __init__(self):
        self.input_sources = GSettings('org.gnome.desktop.input-sources')

    def read_input_sources(self):
        sources = self.input_sources.get('sources')
        logging.debug('read sources: %s', sources)
        if sources is None:
            return []
        return sources

    def write_input_sources(self, sources):
        logging.debug('Setting sources to: %s', sources)
        self.input_sources.set('sources', sources, 'a(ss)')


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
    if not keyboard:
        return None
    kmx_file = os.path.join(packageDir, keyboard['id'] + ".kmx")
    if ignore_language:
        return kmx_file
    if language is not None and language != '':
        logging.debug(language)
        return f"{language}:{kmx_file}"
    if "languages" in keyboard and len(keyboard["languages"]) > 0:
        logging.debug(keyboard["languages"][0])
        return f"{keyboard['languages'][0]['id']}:{kmx_file}"
    return kmx_file
