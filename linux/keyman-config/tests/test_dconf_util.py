#!/usr/bin/python3
import unittest
from gi.repository import Gio

from keyman_config.dconf_util import get_option, set_option, GSETTINGS_BASE


class TestKeymanOptions(unittest.TestCase):
    def setUp(self):
        self.settings = Gio.Settings.new(GSETTINGS_BASE)

    def tearDown(self):
        # Reset the GSettings to its original state
        self.settings.reset("options")

    def test_get_option_invalid_info(self):
        # Test getting options with invalid info
        info = {"packageID": "invalid", "keyboardID": "invalid"}
        options = get_option(info)
        self.assertEqual(options, {})

    def test_get_option_valid_info(self):
        # Test getting options with valid info
        info = {"packageID": "sil_cipher_music", "keyboardID": "sil_cipher_music"}
        options = get_option(info)
        self.assertIsInstance(options, dict)

    def test_set_option(self):
        # Test setting options
        info = {"packageID": "sil_cipher_music", "keyboardID": "sil_cipher_music"}
        options = {"set_nfc": "1"}
        set_option(info, options)
        retrieved_options = get_option(info)
        self.assertEqual(retrieved_options, options)


if __name__ == '__main__':
    unittest.main()
