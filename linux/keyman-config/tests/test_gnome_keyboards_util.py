#!/usr/bin/python3
import os
import unittest
from unittest import mock
from unittest.mock import patch

from keyman_config.gnome_keyboards_util import is_gnome_desktop, _reset_gnome_shell


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


if __name__ == '__main__':
    unittest.main()
