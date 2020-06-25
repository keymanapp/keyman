#!/usr/bin/python3
import unittest
from unittest.mock import patch

from keyman_config.ibus_util import get_ibus_bus, install_to_ibus, uninstall_from_ibus


@patch('keyman_config.ibus_util.IBus.Bus')
class IbusUtilTests(unittest.TestCase):

    def test_getIbusBus_NotConnected_ReturnsNone(self, MockIbusBusClass):
        # Setup
        mock_ibusBusInstance = MockIbusBusClass.return_value
        mock_ibusBusInstance.is_connected.return_value = False
        # Execute/Verify
        self.assertIsNone(get_ibus_bus())
        self.assertTrue(mock_ibusBusInstance.is_connected.called)
        self.assertTrue(MockIbusBusClass.called, "IBus.Bus called")
        self.assertTrue(mock_ibusBusInstance.destroy.called)

    def test_getIbusBus_GlobalEngineNotEnabled_ReturnsNone(self, MockIbusBusClass):
        # Setup
        mock_ibusBusInstance = MockIbusBusClass.return_value
        mock_ibusBusInstance.is_connected.return_value = True
        mock_ibusBusInstance.is_global_engine_enabled.return_value = False
        # Execute/Verify
        self.assertIsNone(get_ibus_bus())
        self.assertTrue(mock_ibusBusInstance.is_connected.called)
        self.assertTrue(mock_ibusBusInstance.is_global_engine_enabled.called)
        self.assertTrue(MockIbusBusClass.called, "IBus.Bus called")
        self.assertTrue(mock_ibusBusInstance.destroy.called)

    def test_getIbusBus_ReturnsBus(self, MockIbusBusClass):
        # Setup
        mock_ibusBusInstance = MockIbusBusClass.return_value
        # Execute/Verify
        self.assertIsNotNone(get_ibus_bus())
        self.assertTrue(MockIbusBusClass.called, "IBus.Bus called")
        self.assertFalse(mock_ibusBusInstance.destroy.called)

    @patch('keyman_config.ibus_util.Gio.Settings.new')
    def test_installToIbus_AlreadyInstalled(self, MockSettingsClass, MockIbusBusClass):
        # Setup
        mock_settingsInstance = MockSettingsClass.return_value
        mock_settingsInstance.get_strv.return_value = ['k1', 'k2', 'k3']
        mock_ibusBusInstance = MockIbusBusClass.return_value
        # Execute
        install_to_ibus(mock_ibusBusInstance, 'k3')
        # Verify
        mock_settingsInstance.set_strv.assert_called_once_with(
            "preload-engines", ['k1', 'k2', 'k3'])
        mock_ibusBusInstance.preload_engines.assert_called_once_with(
            ['k1', 'k2', 'k3'])

    @patch('keyman_config.ibus_util.Gio.Settings.new')
    def test_installToIbus_InstallsNewKb(self, MockSettingsClass, MockIbusBusClass):
        # Setup
        mock_settingsInstance = MockSettingsClass.return_value
        mock_settingsInstance.get_strv.return_value = ['k1', 'k2', 'k3']
        mock_ibusBusInstance = MockIbusBusClass.return_value
        # Execute
        install_to_ibus(mock_ibusBusInstance, 'k4')
        # Verify
        mock_settingsInstance.set_strv.assert_called_once_with(
            "preload-engines", ['k1', 'k2', 'k3', 'k4'])
        mock_ibusBusInstance.preload_engines.assert_called_once_with(
            ['k1', 'k2', 'k3', 'k4'])

    @patch('keyman_config.ibus_util.Gio.Settings.new')
    def test_uninstallFromIbus_KbInstalled(self, MockSettingsClass, MockIbusBusClass):
        # Setup
        mock_settingsInstance = MockSettingsClass.return_value
        mock_settingsInstance.get_strv.return_value = ['k1', 'k2', 'k3']
        mock_ibusBusInstance = MockIbusBusClass.return_value
        # Execute
        uninstall_from_ibus(mock_ibusBusInstance, 'k2')
        # Verify
        mock_settingsInstance.set_strv.assert_called_once_with(
            "preload-engines", ['k1', 'k3'])
        mock_ibusBusInstance.preload_engines.assert_called_once_with(
            ['k1', 'k3'])

    @patch('keyman_config.ibus_util.Gio.Settings.new')
    def test_uninstallFromIbus_KbNotInstalled(self, MockSettingsClass, MockIbusBusClass):
        # Setup
        mock_settingsInstance = MockSettingsClass.return_value
        mock_settingsInstance.get_strv.return_value = ['k1', 'k2', 'k3']
        mock_ibusBusInstance = MockIbusBusClass.return_value
        # Execute
        uninstall_from_ibus(mock_ibusBusInstance, 'k4')
        # Verify
        mock_settingsInstance.set_strv.assert_called_once_with(
            "preload-engines", ['k1', 'k2', 'k3'])
        mock_ibusBusInstance.preload_engines.assert_called_once_with(
            ['k1', 'k2', 'k3'])


if __name__ == '__main__':
    unittest.main()
