#!/usr/bin/python3
import os
import tempfile
import unittest
from unittest.mock import patch

import keyman_config
from keyman_config import _set_dbus_started_for_session, file_cleanup
from keyman_config.gsettings import GSettings

# pylint: disable=unused-argument


class GSettingsTests(unittest.TestCase):
    def setUp(self):
        patcher = patch('keyman_config.gsettings.Gio.Settings.new')
        self.MockSettingsClass = patcher.start()
        self.addCleanup(patcher.stop)

    def tearDown(self) -> None:
        file_cleanup.unregister('keyfile')
        return super().tearDown()

    def test_ConvertArrayToVariantToArray_Empty_Gnome(self):
        # Setup
        children = []
        sut = GSettings('org.gnome.desktop.input-sources')
        # Execute
        variant = sut._convert_array_to_variant(children, 'a(ss)')
        array = sut._convert_variant_to_array(variant)

        # Verify
        self.assertEqual(array, children)

    def test_ConvertArrayToVariantToArray_OneElement_Gnome(self):
        # Setup
        children = [('t1', 'id1')]
        sut = GSettings('org.gnome.desktop.input-sources')
        # Execute
        variant = sut._convert_array_to_variant(children, 'a(ss)')
        array = sut._convert_variant_to_array(variant)

        # Verify
        self.assertEqual(array, children)

    def test_ConvertArrayToVariantToArray_MultipleElements_Gnome(self):
        # Setup
        children = [('t1', 'id1'), ('t2', 'id2')]
        sut = GSettings('org.gnome.desktop.input-sources')
        # Execute
        variant = sut._convert_array_to_variant(children, 'a(ss)')
        array = sut._convert_variant_to_array(variant)

        # Verify
        self.assertEqual(array, children)

    def test_ConvertArrayToVariantToArray_Empty_Ibus(self):
        # Setup
        children = []
        sut = GSettings('org.gnome.desktop.input-sources')
        # Execute
        variant = sut._convert_array_to_variant(children, 'as')
        array = sut._convert_variant_to_array(variant)

        # Verify
        self.assertEqual(array, children)

    def test_ConvertArrayToVariantToArray_OneElement_Ibus(self):
        # Setup
        children = ['t1']
        sut = GSettings('org.gnome.desktop.input-sources')
        # Execute
        variant = sut._convert_array_to_variant(children, 'as')
        array = sut._convert_variant_to_array(variant)

        # Verify
        self.assertEqual(array, children)

    def test_ConvertArrayToVariantToArray_MultipleElements_Ibus(self):
        # Setup
        children = ['t1', 'id1', 't2', 'id2']
        sut = GSettings('org.gnome.desktop.input-sources')
        # Execute
        variant = sut._convert_array_to_variant(children, 'as')
        array = sut._convert_variant_to_array(variant)

        # Verify
        self.assertEqual(array, children)

    @patch.object(GSettings, '_convert_variant_to_array')
    def test_GSettings_get(self, convertVariantToArrayMethod):
        # Setup
        keyboards = [('xkb', 'en')]
        mock_settingsInstance = self.MockSettingsClass.return_value
        mock_settingsInstance.get_value.return_value = keyboards
        convertVariantToArrayMethod.side_effect = lambda value: value
        sut = GSettings('org.gnome.desktop.input-sources')
        # Execute
        result = sut.get('sources')
        # Verify
        self.assertEqual(result, keyboards)

    @patch.object(GSettings, '_convert_array_to_variant')
    def test_GSettings_set_gnome(self, convertArrayToVariantMethod):
        # Setup
        convertArrayToVariantMethod.side_effect = lambda value, type: value
        sut = GSettings('org.gnome.desktop.input-sources')
        keyboards = [('xkb', 'en'), ('ibus', 'fooDir/foo1.kmx'), ('ibus', 'fooDir/foo2.kmx')]
        # Execute
        sut.set('sources', keyboards, 'a(ss)')
        # Verify
        self.MockSettingsClass.return_value.set_value.assert_called_once_with(
            'sources', keyboards)

    @patch.object(GSettings, '_convert_array_to_variant')
    def test_GSettings_set_ibus(self, convertArrayToVariantMethod):
        # Setup
        convertArrayToVariantMethod.side_effect = lambda value, type: value
        sut = GSettings('org.freedesktop.ibus.general')
        keyboards = ['xkb:us::eng', 'de:fooDir/foo1.kmx', 'fr:fooDir/foo2.kmx']
        # Execute
        sut.set('sources', keyboards, 'as')
        # Verify
        self.MockSettingsClass.return_value.set_value.assert_called_once_with(
            'sources', keyboards)

    @patch.object(GSettings, '_convert_array_to_variant')
    @patch('pathlib.Path.unlink')
    @patch('subprocess.run')
    def test_GSettings_set_ibus_keyfile(self, patched_run, patched_unlink,
                                        convertArrayToVariantMethod):
        with tempfile.TemporaryDirectory() as configdir:
            mock_xdg_config_home = patch.object(keyman_config.gsettings,
                                                'xdg_config_home', configdir)
            with mock_xdg_config_home:
                # Setup
                convertArrayToVariantMethod.side_effect = lambda value, type: value
                os.environ['XDG_CONFIG_HOME'] = configdir
                os.makedirs(os.path.join(configdir, 'dconf'))
                sut = GSettings('org.freedesktop.ibus.general')
                keyboards = ['xkb:us::eng', 'de:fooDir/foo1.kmx', 'fr:fooDir/foo2.kmx']
                # Simulate running under a different user account
                _set_dbus_started_for_session(True)

                # Execute
                sut.set('sources', keyboards, 'as')

                # Verify
                keyfile = os.path.join(configdir, 'dconf', 'user.d', 'keyman-settings')
                self.assertTrue(os.path.exists(keyfile))
                with open(keyfile, encoding='utf-8') as file:
                    content = file.read()
                    self.assertEqual(content, '''[org/freedesktop/ibus/general]
sources = @as ['xkb:us::eng', 'de:fooDir/foo1.kmx', 'fr:fooDir/foo2.kmx']

''')

    @patch.object(GSettings, '_convert_array_to_variant')
    @patch('pathlib.Path.unlink')
    @patch('subprocess.run')
    def test_GSettings_set_ibus_keyfile_update(self, patched_run, patched_unlink,
                                               convertArrayToVariantMethod):
        with tempfile.TemporaryDirectory() as configdir:
            mock_xdg_config_home = patch.object(keyman_config.gsettings,
                                                'xdg_config_home', configdir)
            with mock_xdg_config_home:
                # Setup
                convertArrayToVariantMethod.side_effect = lambda value, type: value
                # Simulate running under a different user account
                _set_dbus_started_for_session(True)
                os.environ['XDG_CONFIG_HOME'] = configdir
                os.makedirs(os.path.join(configdir, 'dconf', 'user.d'))
                keyfile = os.path.join(configdir, 'dconf', 'user.d', 'keyman-settings')
                with open(keyfile, mode='w', encoding='utf-8') as file:
                    file.write('''[com/keyman/engine]
additional-keyboards = []

[org/freedesktop/ibus/general]
sources=['xkb:us::eng']
''')
                sut = GSettings('org.freedesktop.ibus.general')
                keyboards = ['xkb:us::eng', 'de:fooDir/foo1.kmx', 'fr:fooDir/foo2.kmx']

                # Execute
                sut.set('sources', keyboards, 'as')

                # Verify
                with open(keyfile, encoding='utf-8') as file:
                    content = file.read()
                    self.assertEqual(content, '''[com/keyman/engine]
additional-keyboards = []

[org/freedesktop/ibus/general]
sources = @as ['xkb:us::eng', 'de:fooDir/foo1.kmx', 'fr:fooDir/foo2.kmx']

''')

    @patch.object(GSettings, '_convert_array_to_variant')
    @patch('pathlib.Path.unlink')
    @patch('subprocess.run')
    def test_GSettings_set_ibus_keyfile_empty(self, patched_run, patched_unlink,
                                              convertArrayToVariantMethod):
        with tempfile.TemporaryDirectory() as configdir:
            mock_xdg_config_home = patch.object(keyman_config.gsettings,
                                                'xdg_config_home', configdir)
            with mock_xdg_config_home:
                # Setup
                convertArrayToVariantMethod.side_effect = lambda value, type: value
                os.environ['XDG_CONFIG_HOME'] = configdir
                os.makedirs(os.path.join(configdir, 'dconf'))
                sut = GSettings('org.freedesktop.ibus.general')
                keyboards = []
                # Simulate running under a different user account
                _set_dbus_started_for_session(True)

                # Execute
                sut.set('sources', keyboards, 'as')

                # Verify
                keyfile = os.path.join(configdir, 'dconf', 'user.d', 'keyman-settings')
                self.assertTrue(os.path.exists(keyfile))
                with open(keyfile, encoding='utf-8') as file:
                    content = file.read()
                    self.assertEqual(content, '''[org/freedesktop/ibus/general]
sources = @as []

''')
