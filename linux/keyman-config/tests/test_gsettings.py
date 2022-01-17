#!/usr/bin/python3
import unittest
from unittest.mock import patch

from keyman_config.gsettings import GSettings

class GSettingsTests(unittest.TestCase):
    def setUp(self):
        patcher = patch('keyman_config.gsettings.Gio.Settings.new')
        self.MockSettingsClass = patcher.start()
        self.addCleanup(patcher.stop)

    def test_ConvertArrayToVariantToArray_Empty(self):
        # Setup
        children = []
        sut = GSettings('org.gnome.desktop.input-sources')
        # Execute
        variant = sut._convert_array_to_variant(children)
        array = sut._convert_variant_to_array(variant)

        # Verify
        self.assertEqual(array, children)

    def test_ConvertArrayToVariantToArray_OneElement(self):
        # Setup
        children = [('t1', 'id1')]
        sut = GSettings('org.gnome.desktop.input-sources')
        # Execute
        variant = sut._convert_array_to_variant(children)
        array = sut._convert_variant_to_array(variant)

        # Verify
        self.assertEqual(array, children)

    def test_ConvertArrayToVariantToArray_MultipleElements(self):
        # Setup
        children = [('t1', 'id1'), ('t2', 'id2')]
        sut = GSettings('org.gnome.desktop.input-sources')
        # Execute
        variant = sut._convert_array_to_variant(children)
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
    def test_GSettings_set(self, convertArrayToVariantMethod):
        # Setup
        convertArrayToVariantMethod.side_effect = lambda value: value
        sut = GSettings('org.gnome.desktop.input-sources')
        keyboards = [('xkb', 'en'), ('ibus', 'fooDir/foo1.kmx'), ('ibus', 'fooDir/foo2.kmx')]
        # Execute
        sut.set('sources', keyboards)
        # Verify
        self.MockSettingsClass.return_value.set_value.assert_called_once_with(
            'sources', keyboards)
