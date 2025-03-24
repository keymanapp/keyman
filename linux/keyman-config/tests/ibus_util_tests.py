#!/usr/bin/python3
import unittest
from unittest.mock import Mock, patch

from keyman_config.ibus_util import _get_ibus_version, get_ibus_bus, install_to_ibus, uninstall_from_ibus


@patch('keyman_config.ibus_util.IBus.Bus')
@patch('time.sleep', return_value=None)
class IbusUtilTests(unittest.TestCase):

    def test_getIbusBus_NotConnected_ReturnsNone(self, patched_time_sleep, MockIbusBusClass):
        # Setup
        mock_ibusBusInstance = MockIbusBusClass.return_value
        mock_ibusBusInstance.is_connected.return_value = False
        # Execute/Verify
        self.assertIsNone(get_ibus_bus())
        self.assertTrue(mock_ibusBusInstance.is_connected.called)
        self.assertTrue(MockIbusBusClass.called, "IBus.Bus called")
        self.assertTrue(mock_ibusBusInstance.destroy.called)

    def test_getIbusBus_GlobalEngineNotEnabled_ReturnsNone(self, patched_time_sleep, MockIbusBusClass):
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

    def test_getIbusBus_ReturnsBus(self, patched_time_sleep, MockIbusBusClass):
        # Setup
        mock_ibusBusInstance = MockIbusBusClass.return_value
        # Execute/Verify
        self.assertIsNotNone(get_ibus_bus())
        self.assertTrue(MockIbusBusClass.called, "IBus.Bus called")
        self.assertFalse(mock_ibusBusInstance.destroy.called)

    @patch('keyman_config.gsettings.GSettings.get', return_value=['k1', 'k2', 'k3'])
    @patch('keyman_config.gsettings.GSettings.set')
    def test_installToIbus_AlreadyInstalled(self, patched_settings_set, patched_settings_get, patched_time_sleep, MockIbusBusClass):
        # Setup
        mock_ibusBusInstance = MockIbusBusClass.return_value
        # Execute
        install_to_ibus(mock_ibusBusInstance, 'k3')
        # Verify
        patched_settings_set.assert_called_once_with(
            "preload-engines", ['k1', 'k2', 'k3'], 'as')
        mock_ibusBusInstance.preload_engines.assert_called_once_with(
            ['k1', 'k2', 'k3'])

    @patch('keyman_config.gsettings.GSettings.get', return_value=['k1', 'k2', 'k3'])
    @patch('keyman_config.gsettings.GSettings.set')
    def test_installToIbus_InstallsNewKb(self, patched_settings_set, patched_settings_get, patched_time_sleep, MockIbusBusClass):
        # Setup
        mock_ibusBusInstance = MockIbusBusClass.return_value
        # Execute
        install_to_ibus(mock_ibusBusInstance, 'k4')
        # Verify
        patched_settings_set.assert_called_once_with(
            "preload-engines", ['k1', 'k2', 'k3', 'k4'], 'as')
        mock_ibusBusInstance.preload_engines.assert_called_once_with(
            ['k1', 'k2', 'k3', 'k4'])

    @patch('keyman_config.gsettings.GSettings.get', return_value=['k1', 'k2', 'k3'])
    @patch('keyman_config.gsettings.GSettings.set')
    def test_uninstallFromIbus_KbInstalled(self, patched_settings_set, patched_settings_get, patched_time_sleep, MockIbusBusClass):
        # Setup
        mock_ibusBusInstance = MockIbusBusClass.return_value
        # Execute
        uninstall_from_ibus(mock_ibusBusInstance, 'k2')
        # Verify
        patched_settings_set.assert_called_once_with(
            "preload-engines", ['k1', 'k3'], 'as')
        mock_ibusBusInstance.preload_engines.assert_called_once_with(
            ['k1', 'k3'])

    @patch('keyman_config.gsettings.GSettings.get', return_value=['k1', 'k2', 'k3'])
    @patch('keyman_config.gsettings.GSettings.set')
    def test_uninstallFromIbus_KbNotInstalled(self, patched_settings_set, patched_settings_get, patched_time_sleep, MockIbusBusClass):
        # Setup
        mock_ibusBusInstance = MockIbusBusClass.return_value
        # Execute
        uninstall_from_ibus(mock_ibusBusInstance, 'k4')
        # Verify
        patched_settings_set.assert_called_once_with(
            "preload-engines", ['k1', 'k2', 'k3'], 'as')
        mock_ibusBusInstance.preload_engines.assert_called_once_with(
            ['k1', 'k2', 'k3'])

    @patch('subprocess.run')
    def test_get_ibus_version(self, patched_subprocess_run, patched_time_sleep, MockIbusBusClass):
        # Setup
        patched_subprocess_run.return_value = Mock(
            stdout=b'IBus 1.5.28\n',  # Simulating the output of the subprocess command
            returncode=0  # Simulating a successful execution
        )

        # Execute
        ibus_version = _get_ibus_version()

        # Verify
        self.assertEqual(ibus_version, '1.5.28')

if __name__ == '__main__':
    unittest.main()
