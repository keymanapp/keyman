#!/usr/bin/env python3
import os
import unittest
from unittest import mock
from unittest.mock import patch

from keyman_config.gnome_keyboards_util import get_ibus_keyboard_id, is_gnome_desktop, _reset_gnome_shell


class GnomeKeyboardsUtilTests(unittest.TestCase):
    def setUp(self):
        _reset_gnome_shell()

    @patch('os.system')
    @mock.patch.dict(os.environ, {'XDG_CURRENT_DESKTOP': 'ubuntu:GNOME'})
    def test_IsGnomeDesktop_RunningGnomeShell(self, mockSystem):
        # Setup
        mockSystem.return_value = 0
        # Execute/Verify
        self.assertEqual(is_gnome_desktop(), True)

    @patch('os.system')
    @mock.patch.dict(os.environ, {'XDG_CURRENT_DESKTOP': 'X-Cinnamon'})
    def test_IsGnomeDesktop_NotRunningGnomeShell(self, mockSystem):
        # Setup
        mockSystem.return_value = 1
        # Execute/Verify
        self.assertEqual(is_gnome_desktop(), False)

    @patch('os.system')
    @mock.patch.dict(os.environ, {'XDG_CURRENT_DESKTOP': 'budgie:GNOME'})
    def test_IsGnomeDesktop_RunningBudgie(self, mockSystem):
        # Setup
        mockSystem.return_value = 1
        # Execute/Verify
        self.assertEqual(is_gnome_desktop(), True)

    def test_GetIbusKeyboardId_NoKeyboard(self):
        self.assertIsNone(get_ibus_keyboard_id(None, '/tmp'))

    def test_GetIbusKeyboardId_IgnoreLanguage(self):
        keyboard = {'id': 'foo'}
        self.assertEqual(get_ibus_keyboard_id(keyboard, '/tmp', ignore_language=True), '/tmp/foo.kmx')

    def test_GetIbusKeyboardId_NoLanguage(self):
        keyboard = {'id': 'foo'}
        self.assertEqual(get_ibus_keyboard_id(keyboard, '/tmp'), '/tmp/foo.kmx')

    def test_GetIbusKeyboardId_EmptyLanguage(self):
        keyboard = {'id': 'foo'}
        self.assertEqual(get_ibus_keyboard_id(keyboard, '/tmp', ''), '/tmp/foo.kmx')

    def test_GetIbusKeyboardId_Language(self):
        keyboard = {'id': 'foo'}
        self.assertEqual(get_ibus_keyboard_id(keyboard, '/tmp', 'en'), 'en:/tmp/foo.kmx')

    def test_GetIbusKeyboardId_LanguagesInKeyboard(self):
        keyboard = {'id': 'foo', 'languages': [ {'id': 'fr'}]}
        self.assertEqual(get_ibus_keyboard_id(keyboard, '/tmp'), 'fr:/tmp/foo.kmx')

    def test_GetIbusKeyboardId_MultipleLanguagesInKeyboard(self):
        keyboard = {'id': 'foo', 'languages': [{'id': 'km'}, {'id': 'fr'}]}
        self.assertEqual(get_ibus_keyboard_id(keyboard, '/tmp'), 'km:/tmp/foo.kmx')

    def test_GetIbusKeyboardId_InvalidLanguagesInKeyboard(self):  # 14748
        keyboard = {'id': 'foo', 'languages': ['es']}
        self.assertEqual(get_ibus_keyboard_id(keyboard, '/tmp'), '/tmp/foo.kmx')


if __name__ == '__main__':
    unittest.main()
