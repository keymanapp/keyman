#!/usr/bin/python3
import unittest
from unittest.mock import patch

from keyman_config.gnome_keyboards_util import is_gnome_shell, _reset_gnome_shell


class GnomeKeyboardsUtilTests(unittest.TestCase):
    def setUp(self):
        _reset_gnome_shell()

    @patch('os.system')
    def test_IsGnomeShell_RunningGnomeShell(self, mockSystem):
        # Setup
        mockSystem.return_value = 0
        # Execute/Verify
        self.assertEqual(is_gnome_shell(), True)

    @patch('os.system')
    def test_IsGnomeShell_NotRunningGnomeShell(self, mockSystem):
        # Setup
        mockSystem.return_value = 1
        # Execute/Verify
        self.assertEqual(is_gnome_shell(), False)


if __name__ == '__main__':
    unittest.main()
