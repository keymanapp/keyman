#!/usr/bin/python3
import unittest
from unittest.mock import patch, ANY

from keyman_config.install_kmp import install_keyboards_to_ibus, install_keyboards_to_gnome


class InstallKmpTests(unittest.TestCase):

    def setUp(self):
        patcher1 = patch('keyman_config.install_kmp.install_to_ibus')
        self.mockInstallToIbus = patcher1.start()
        self.addCleanup(patcher1.stop)
        patcher2 = patch('keyman_config.install_kmp.restart_ibus')
        self.mockRestartIbus = patcher2.start()
        self.addCleanup(patcher2.stop)
        patcher3 = patch('keyman_config.install_kmp.get_ibus_bus')
        self.mockGetIbusBus = patcher3.start()
        self.addCleanup(patcher3.stop)
        patcher4 = patch('keyman_config.install_kmp.GnomeKeyboardsUtil')
        self.mockGnomeKeyboardsUtilClass = patcher4.start()
        self.addCleanup(patcher4.stop)

    def test_InstallKeyboardsToIbus_NoIbus(self):
        # Setup
        self.mockGetIbusBus.return_value = None
        # Execute
        install_keyboards_to_ibus([], None)
        # Verify
        self.mockRestartIbus.assert_not_called()

    def test_InstallKeyboardsToIbus_SingleKbNoLanguages(self):
        # Setup
        bus = self.mockGetIbusBus.return_value
        keyboards = [{'id': 'foo1'}]
        # Execute
        install_keyboards_to_ibus(keyboards, 'fooDir')
        # Verify
        self.mockInstallToIbus.assert_called_once_with(ANY, 'fooDir/foo1.kmx')
        self.mockRestartIbus.assert_called_once()
        bus.destroy.assert_called_once()

    def test_InstallKeyboardsToIbus_MultipleKbsNoLanguages(self):
        # Setup
        bus = self.mockGetIbusBus.return_value
        keyboards = [{'id': 'foo1'}, {'id': 'foo2'}]
        # Execute
        install_keyboards_to_ibus(keyboards, 'fooDir')
        # Verify
        self.mockInstallToIbus.assert_any_call(ANY, 'fooDir/foo1.kmx')
        self.mockInstallToIbus.assert_any_call(ANY, 'fooDir/foo2.kmx')
        self.mockRestartIbus.assert_called_once()
        bus.destroy.assert_called_once()

    def test_InstallKeyboardsToIbus_SingleKbSingleLanguage(self):
        # Setup
        bus = self.mockGetIbusBus.return_value
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}]}]
        # Execute
        install_keyboards_to_ibus(keyboards, 'fooDir')
        # Verify
        self.mockInstallToIbus.assert_called_once_with(ANY, 'en:fooDir/foo1.kmx')
        self.mockRestartIbus.assert_called_once()
        bus.destroy.assert_called_once()

    def test_InstallKeyboardsToIbus_SingleKbMultipleLanguages(self):
        # Setup
        bus = self.mockGetIbusBus.return_value
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}]
        # Execute
        install_keyboards_to_ibus(keyboards, 'fooDir')
        # Verify
        self.mockInstallToIbus.assert_called_once()
        self.mockInstallToIbus.assert_called_with(ANY, 'en:fooDir/foo1.kmx')
        # self.mockInstallToIbus.assert_not_called_with(ANY, 'fr:fooDir/foo1.kmx')
        self.mockRestartIbus.assert_called_once()
        bus.destroy.assert_called_once()

    def test_InstallKeyboardsToIbus_SingleKbMultipleLanguages_GivenLanguage(self):
        # Setup
        bus = self.mockGetIbusBus.return_value
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}]
        # Execute
        install_keyboards_to_ibus(keyboards, 'fooDir', 'fr')
        # Verify
        self.mockInstallToIbus.assert_called_once()
        self.mockInstallToIbus.assert_called_with(ANY, 'fr:fooDir/foo1.kmx')
        # self.mockInstallToIbus.assert_not_called_with(ANY, 'en:fooDir/foo1.kmx')
        self.mockRestartIbus.assert_called_once()
        bus.destroy.assert_called_once()

    def test_InstallKeyboardsToIbus_SingleKbMultipleLanguages_OtherLanguage(self):
        # Setup
        bus = self.mockGetIbusBus.return_value
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}]
        # Execute
        install_keyboards_to_ibus(keyboards, 'fooDir', 'de')
        # Verify
        self.mockInstallToIbus.assert_called_once()
        self.mockInstallToIbus.assert_called_with(ANY, 'de:fooDir/foo1.kmx')
        # self.mockInstallToIbus.assert_not_called_with(ANY, 'en:fooDir/foo1.kmx')
        self.mockRestartIbus.assert_called_once()
        bus.destroy.assert_called_once()

    def test_InstallKeyboardsToGnome_SingleKbNoLanguages(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [('xkb', 'en')]
        keyboards = [{'id': 'foo1'}]
        # Execute
        install_keyboards_to_gnome(keyboards, 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
            [('xkb', 'en'), ('ibus', 'fooDir/foo1.kmx')])
        self.mockRestartIbus.assert_not_called()

    def test_InstallKeyboardsToGnome_MultipleKbsNoLanguages(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [('xkb', 'en')]
        keyboards = [{'id': 'foo1'}, {'id': 'foo2'}]
        # Execute
        install_keyboards_to_gnome(keyboards, 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
            [('xkb', 'en'), ('ibus', 'fooDir/foo1.kmx'), ('ibus', 'fooDir/foo2.kmx')])
        self.mockRestartIbus.assert_not_called()

    def test_InstallKeyboardsToGnome_SingleKbSingleLanguage(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [('xkb', 'en')]
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}]}]
        # Execute
        install_keyboards_to_gnome(keyboards, 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
            [('xkb', 'en'), ('ibus', 'en:fooDir/foo1.kmx')])
        self.mockRestartIbus.assert_not_called()

    def test_InstallKeyboardsToGnome_SingleKbMultipleLanguages(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [('xkb', 'en')]
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}]
        # Execute
        install_keyboards_to_gnome(keyboards, 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
            [('xkb', 'en'), ('ibus', 'en:fooDir/foo1.kmx')])
        self.mockRestartIbus.assert_not_called()

    def test_InstallKeyboardsToGnome_SingleKbMultipleLanguages_GivenLanguage(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [('xkb', 'en')]
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}]
        # Execute
        install_keyboards_to_gnome(keyboards, 'fooDir', 'fr')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
            [('xkb', 'en'), ('ibus', 'fr:fooDir/foo1.kmx')])
        self.mockRestartIbus.assert_not_called()

    def test_InstallKeyboardsToGnome_SingleKbMultipleLanguages_OtherLanguage(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [('xkb', 'en')]
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}]
        # Execute
        install_keyboards_to_gnome(keyboards, 'fooDir', 'de')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
            [('xkb', 'en'), ('ibus', 'de:fooDir/foo1.kmx')])
        self.mockRestartIbus.assert_not_called()


if __name__ == '__main__':
    unittest.main()
