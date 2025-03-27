#!/usr/bin/python3
import unittest
from unittest.mock import Mock, patch

from dbus import DBusException

from keyman_config.dbus_util import get_keyman_config_service


class KeymanConfigServiceManager(unittest.TestCase):
    def setUp(self):
        self.mockDbus = self._setupMock('keyman_config.dbus_util.dbus')

    def _setupMock(self, arg0):
        patcher = patch(arg0)
        result = patcher.start()
        self.addCleanup(patcher.stop)
        return result

    def test_KeyboardListChanged_HappyPath(self):
        # Setup
        serviceManager = get_keyman_config_service()
        serviceManager.__service = Mock()
        serviceManager.__service.keyboard_list_changed = Mock()

        # Execute
        serviceManager.keyboard_list_changed()

        # Verify
        self.assertEqual(serviceManager.__service.keyboard_list_changed.call_count, 1)

    def test_KeyboardListChanged_IgnoreDbusExceptionDisconnected(self):
        '''#12902: ignore DBus exceptions happening from call to keyboard_list_changed'''

        # Setup
        serviceManager = get_keyman_config_service()
        serviceManager.__service = Mock()
        serviceManager.__service.keyboard_list_changed = Mock(side_effect=DBusException(
            'org.freedesktop.DBus.Error.NoReply: Message recipient disconnected from message bus without replying'))

        # Execute
        serviceManager.keyboard_list_changed()

        # Verify
        self.assertEqual(serviceManager.__service.keyboard_list_changed.call_count, 1)

    def test_KeyboardListChanged_IgnoreDbusExceptionNoReply(self):
        '''#13283: ignore DBus exceptions happening from call to keyboard_list_changed'''

        # Setup
        serviceManager = get_keyman_config_service()
        serviceManager.__service = Mock()
        serviceManager.__service.keyboard_list_changed = Mock(side_effect=DBusException(
            'org.freedesktop.DBus.Error.NoReply: Did not receive a reply.'))

        # Execute
        serviceManager.keyboard_list_changed()

        # Verify
        self.assertEqual(serviceManager.__service.keyboard_list_changed.call_count, 1)

