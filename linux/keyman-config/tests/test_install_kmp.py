import unittest
from unittest.mock import create_autospec, patch, ANY

from keyman_config.install_kmp import install_keyboards_to_ibus

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
        self.mockInstallToIbus.assert_called_once_with(
            ANY, 'en:fooDir/foo1.kmx')
        self.mockRestartIbus.assert_called_once()
        bus.destroy.assert_called_once()

if __name__ == '__main__':
    unittest.main()
