#!/usr/bin/python3
import unittest
from unittest.mock import patch

from keyman_config.gnome_keyboards_util import GnomeKeyboardsUtil
from keyman_config.gnome_keyboards_util import is_gnome_shell, _reset_gnome_shell


class GnomeKeyboardsUtilTests(unittest.TestCase):
    def setUp(self):
        _reset_gnome_shell()
        patcher = patch('keyman_config.ibus_util.Gio.Settings.new')
        self.MockSettingsClass = patcher.start()
        self.addCleanup(patcher.stop)

    def test_ConvertArrayToVariantToArray_Empty(self):
        # Setup
        children = []
        sut = GnomeKeyboardsUtil()
        # Execute
        variant = sut._convert_array_to_variant(children)
        array = sut._convert_variant_to_array(variant)

        # Verify
        self.assertEqual(array, children)

    def test_ConvertArrayToVariantToArray_OneElement(self):
        # Setup
        children = [('t1', 'id1')]
        sut = GnomeKeyboardsUtil()
        # Execute
        variant = sut._convert_array_to_variant(children)
        array = sut._convert_variant_to_array(variant)

        # Verify
        self.assertEqual(array, children)

    def test_ConvertArrayToVariantToArray_MultipleElements(self):
        # Setup
        children = [('t1', 'id1'), ('t2', 'id2')]
        sut = GnomeKeyboardsUtil()
        # Execute
        variant = sut._convert_array_to_variant(children)
        array = sut._convert_variant_to_array(variant)

        # Verify
        self.assertEqual(array, children)

    @patch.object(GnomeKeyboardsUtil, '_convert_variant_to_array')
    def test_ReadInputSources(self, convertVariantToArrayMethod):
        # Setup
        keyboards = [('xkb', 'en')]
        mock_settingsInstance = self.MockSettingsClass.return_value
        mock_settingsInstance.get_value.return_value = keyboards
        convertVariantToArrayMethod.side_effect = lambda value: value
        sut = GnomeKeyboardsUtil()
        # Execute
        result = sut.read_input_sources()
        # Verify
        self.assertEqual(result, keyboards)

    @patch.object(GnomeKeyboardsUtil, '_convert_array_to_variant')
    def test_WriteInputSources(self, convertArrayToVariantMethod):
        # Setup
        convertArrayToVariantMethod.side_effect = lambda value: value
        sut = GnomeKeyboardsUtil()
        keyboards = [('xkb', 'en'), ('ibus', 'fooDir/foo1.kmx'), ('ibus', 'fooDir/foo2.kmx')]
        # Execute
        sut.write_input_sources(keyboards)
        # Verify
        self.MockSettingsClass.return_value.set_value.assert_called_once_with(
            "sources", keyboards)

    @patch('keyman_config.install_kmp.os.system')
    def test_IsGnomeShell_RunningGnomeShell(self, mockSystem):
        # Setup
        mockSystem.return_value = 0
        # Execute/Verify
        self.assertEqual(is_gnome_shell(), True)

    @patch('keyman_config.install_kmp.os.system')
    def test_IsGnomeShell_NotRunningGnomeShell(self, mockSystem):
        # Setup
        mockSystem.return_value = 1
        # Execute/Verify
        self.assertEqual(is_gnome_shell(), False)


if __name__ == '__main__':
    unittest.main()
