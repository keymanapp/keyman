#!/usr/bin/python3
import unittest
from unittest.mock import patch
from keyman_config import __version__
from keyman_config.custom_keyboards import CustomKeyboards

class AddCustomKeyboardTests(unittest.TestCase):
    def setUp(self):
        patcher1 = patch('keyman_config.custom_keyboards.GSettings')
        self.mockGSettingsClass = patcher1.start()
        self.addCleanup(patcher1.stop)

    def test_Add_NoParam(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = None
        # Execute
        CustomKeyboards().add(None)
        # Verify
        mockGSettingsInstance.set.assert_not_called()

    def test_Add_Unset(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = None
        keyboard = 'mul:fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().add(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with('additional-keyboards', ['mul:fooDir/foo1.kmx'], 'as')

    def test_Add_Invalid(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = 'foo'
        keyboard = 'mul:fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().add(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with('additional-keyboards', ['mul:fooDir/foo1.kmx'], 'as')

    def test_Add_InvalidArray(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = ['garbage', '', 'und:barDir/bar.kmx']
        keyboard = 'mul:fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().add(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with(
            'additional-keyboards', ['und:barDir/bar.kmx', 'mul:fooDir/foo1.kmx'], 'as')

    def test_Add_EmptyArray(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = []
        keyboard = 'mul:fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().add(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with('additional-keyboards', ['mul:fooDir/foo1.kmx'], 'as')

    def test_Add_EmptyString(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = ['', 'und:barDir/bar.kmx']
        keyboard = 'mul:fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().add(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with(
            'additional-keyboards', ['und:barDir/bar.kmx', 'mul:fooDir/foo1.kmx'], 'as')

    def test_Add_PrevVal(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = ['und:barDir/bar.kmx']
        keyboard = 'mul:fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().add(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with(
            'additional-keyboards', ['und:barDir/bar.kmx', 'mul:fooDir/foo1.kmx'], 'as')

    def test_Add_Duplicate(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = ['und:barDir/bar.kmx', 'mul:fooDir/foo1.kmx']
        keyboard = 'mul:fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().add(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_not_called()  # nothing changed, so no need to call


class RemoveCustomKeyboardTests(unittest.TestCase):
    def setUp(self):
        patcher1 = patch('keyman_config.custom_keyboards.GSettings')
        self.mockGSettingsClass = patcher1.start()
        self.addCleanup(patcher1.stop)

    def test_Remove_NoParam(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = None
        # Execute
        CustomKeyboards().remove(None)
        # Verify
        mockGSettingsInstance.set.assert_not_called()

    def test_Remove_Unset(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = None
        keyboard = 'fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().remove(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with('additional-keyboards', [], 'as')

    def test_Remove_Invalid(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = 'foo'
        keyboard = 'fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().remove(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with('additional-keyboards', [], 'as')

    def test_Remove_InvalidArray(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = ['garbage', '', 'und:barDir/bar.kmx']
        keyboard = 'fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().remove(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with(
            'additional-keyboards', ['und:barDir/bar.kmx'], 'as')

    def test_Remove_EmptyArray(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = []
        keyboard = 'fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().remove(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with('additional-keyboards', [], 'as')

    def test_Remove_EmptyString(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = ['', 'und:barDir/bar.kmx']
        keyboard = 'fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().remove(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with(
            'additional-keyboards', ['und:barDir/bar.kmx'], 'as')

    def test_Remove_MultipleValues(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = ['und:barDir/bar.kmx', 'mul:fooDir/foo1.kmx']
        keyboard = 'fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().remove(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with(
            'additional-keyboards', ['und:barDir/bar.kmx'], 'as')

    def test_Remove_MultipleValuesWithSameKeyboard(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = ['und:barDir/bar.kmx', 'mul:fooDir/foo1.kmx', 'und:fooDir/foo1.kmx']
        keyboard = 'fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().remove(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with(
            'additional-keyboards', ['und:barDir/bar.kmx'], 'as')

    def test_Remove_SingleValue(self):
        # Setup
        mockGSettingsInstance = self.mockGSettingsClass.return_value
        mockGSettingsInstance.get.return_value = ['mul:fooDir/foo1.kmx']
        keyboard = 'fooDir/foo1.kmx'
        # Execute
        CustomKeyboards().remove(keyboard)
        # Verify
        mockGSettingsInstance.set.assert_called_once_with(
            'additional-keyboards', [], 'as')
