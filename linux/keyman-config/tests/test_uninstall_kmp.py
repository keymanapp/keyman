#!/usr/bin/python3
import unittest
from unittest.mock import patch

from keyman_config.uninstall_kmp import _uninstall_keyboards_from_gnome, _uninstall_keyboards_from_ibus


class UninstallKmpGnomeTests(unittest.TestCase):

    def setUp(self):
        patcher = patch('keyman_config.uninstall_kmp.GnomeKeyboardsUtil')
        self.mockGnomeKeyboardsUtilClass = patcher.start()
        self.addCleanup(patcher.stop)

    def test_UninstallKeyboardsFromGnome_RemoveOneKeyboardFromEmpty(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = []
        # Execute
        _uninstall_keyboards_from_gnome([{'id': 'foo1'}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with([])

    def test_UninstallKeyboardsFromGnome_RemoveNonExistingKeyboard(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
          ('ibus', 'fooDir/foo2.kmx')]
        # Execute
        _uninstall_keyboards_from_gnome([{'id': 'foo1'}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with([
          ('ibus', 'fooDir/foo2.kmx')])

    def test_UninstallKeyboardsFromGnome_RemoveOneKeyboard(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
          ('xkb', 'en'), ('ibus', 'fooDir/foo1.kmx')]
        # Execute
        _uninstall_keyboards_from_gnome([{'id': 'foo1'}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
          [('xkb', 'en')])

    def test_UninstallKeyboardsFromGnome_RemoveMultipleKeyboards(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
          ('xkb', 'en'), ('ibus', 'fooDir/foo1.kmx'), ('ibus', 'fooDir/foo2.kmx')]
        # Execute
        _uninstall_keyboards_from_gnome([{'id': 'foo1'}, {'id': 'foo2'}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
          [('xkb', 'en')])

    def test_UninstallKeyboardsFromGnome_RemoveAllKeyboards(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
          ('ibus', 'fooDir/foo1.kmx'), ('ibus', 'fooDir/foo2.kmx')]
        # Execute
        _uninstall_keyboards_from_gnome([{'id': 'foo1'}, {'id': 'foo2'}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with([])

    def test_UninstallKeyboardsFromGnome_RemoveKeyboard_SingleLanguage(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
          ('xkb', 'en'), ('ibus', 'en:fooDir/foo1.kmx')]
        # Execute
        _uninstall_keyboards_from_gnome([{'id': 'foo1', 'languages': [{'id': 'en'}]}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
          [('xkb', 'en')])

    def test_UninstallKeyboardsFromGnome_RemoveKeyboard_MultipleLanguages(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
          ('xkb', 'en'), ('ibus', 'fr:fooDir/foo1.kmx')]
        # Execute
        _uninstall_keyboards_from_gnome(
          [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
          [('xkb', 'en')])

    def test_UninstallKeyboardsFromGnome_RemoveKeyboard_OneNotMatchingLanguage(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
          ('xkb', 'en'), ('ibus', 'en:fooDir/foo1.kmx')]
        # Execute
        _uninstall_keyboards_from_gnome(
          [{'id': 'foo1', 'languages': [{'id': 'fr'}]}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
          [('xkb', 'en')])

    def test_UninstallKeyboardsFromGnome_RemoveKeyboard_RemovesAllMatching(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
          ('xkb', 'en'), ('ibus', 'en:fooDir/foo1.kmx'), ('ibus', 'fr:fooDir/foo1.kmx')]
        # Execute
        _uninstall_keyboards_from_gnome(
          [{'id': 'foo1', 'languages': [{'id': 'fr'}]}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
          [('xkb', 'en')])


class UninstallKmpIbusTests(unittest.TestCase):

    def setUp(self):
        self.mockIbusUtilClass = self._setupMock('keyman_config.uninstall_kmp.IbusUtil')
        self.mockRestartIbus = self._setupMock('keyman_config.uninstall_kmp.restart_ibus')
        self.mockGetIbusBus = self._setupMock('keyman_config.uninstall_kmp.get_ibus_bus')
        self.mockGetIbusBus.return_value = None

    def _setupMock(self, arg0):
        patcher = patch(arg0)
        result = patcher.start()
        self.addCleanup(patcher.stop)
        return result

    def test_UninstallKeyboardsFromIbus_RemoveOneKeyboardFromEmpty(self):
        # Setup
        mockIbusUtilInstance = self.mockIbusUtilClass.return_value
        mockIbusUtilInstance.read_preload_engines.return_value = []
        # Execute
        _uninstall_keyboards_from_ibus([{'id': 'foo1'}], 'fooDir')
        # Verify
        mockIbusUtilInstance.write_preload_engines.assert_not_called()

    def test_UninstallKeyboardsFromIbus_RemoveNonExistingKeyboard(self):
        # Setup
        mockIbusUtilInstance = self.mockIbusUtilClass.return_value
        mockIbusUtilInstance.read_preload_engines.return_value = [
          'fr:fooDir/foo2.kmx']
        # Execute
        _uninstall_keyboards_from_ibus([{'id': 'foo1'}], 'fooDir')
        # Verify
        mockIbusUtilInstance.write_preload_engines.assert_called_once_with(None, [
          'fr:fooDir/foo2.kmx'])

    def test_UninstallKeyboardsFromIbus_RemoveOneKeyboard(self):
        # Setup
        mockIbusUtilInstance = self.mockIbusUtilClass.return_value
        mockIbusUtilInstance.read_preload_engines.return_value = [
          'xkb:us::eng', 'km:fooDir/foo1.kmx']
        # Execute
        _uninstall_keyboards_from_ibus([{'id': 'foo1'}], 'fooDir')
        # Verify
        mockIbusUtilInstance.write_preload_engines.assert_called_once_with(
          None,
          ['xkb:us::eng'])

    def test_UninstallKeyboardsFromIbus_RemoveMultipleKeyboards(self):
        # Setup
        mockIbusUtilInstance = self.mockIbusUtilClass.return_value
        mockIbusUtilInstance.read_preload_engines.return_value = [
          'xkb:us::eng', 'km:fooDir/foo1.kmx', 'fr:fooDir/foo2.kmx']
        # Execute
        _uninstall_keyboards_from_ibus([{'id': 'foo1'}, {'id': 'foo2'}], 'fooDir')
        # Verify
        mockIbusUtilInstance.write_preload_engines.assert_called_once_with(
          None,
          ['xkb:us::eng'])

    def test_UninstallKeyboardsFromIbus_RemoveAllKeyboards(self):
        # Setup
        mockIbusUtilInstance = self.mockIbusUtilClass.return_value
        mockIbusUtilInstance.read_preload_engines.return_value = [
          'km:fooDir/foo1.kmx', 'fr:fooDir/foo2.kmx']
        # Execute
        _uninstall_keyboards_from_ibus([{'id': 'foo1'}, {'id': 'foo2'}], 'fooDir')
        # Verify
        mockIbusUtilInstance.write_preload_engines.assert_called_once_with(None, [])

    def test_UninstallKeyboardsFromIbus_RemoveKeyboard_SingleLanguage(self):
        # Setup
        mockIbusUtilInstance = self.mockIbusUtilClass.return_value
        mockIbusUtilInstance.read_preload_engines.return_value = [
          'xkb:us::eng', 'en:fooDir/foo1.kmx']
        # Execute
        _uninstall_keyboards_from_ibus([{'id': 'foo1', 'languages': [{'id': 'en'}]}], 'fooDir')
        # Verify
        mockIbusUtilInstance.write_preload_engines.assert_called_once_with(
          None,
          ['xkb:us::eng'])

    def test_UninstallKeyboardsFromIbus_RemoveKeyboard_MultipleLanguages(self):
        # Setup
        mockIbusUtilInstance = self.mockIbusUtilClass.return_value
        mockIbusUtilInstance.read_preload_engines.return_value = [
          'xkb:us::eng', 'fr:fooDir/foo1.kmx']
        # Execute
        _uninstall_keyboards_from_ibus(
          [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}], 'fooDir')
        # Verify
        mockIbusUtilInstance.write_preload_engines.assert_called_once_with(
          None,
          ['xkb:us::eng'])

    def test_UninstallKeyboardsFromIbus_RemoveKeyboard_OneNotMatchingLanguage(self):
        # Setup
        mockIbusUtilInstance = self.mockIbusUtilClass.return_value
        mockIbusUtilInstance.read_preload_engines.return_value = [
          'xkb:us::eng', 'en:fooDir/foo1.kmx']
        # Execute
        _uninstall_keyboards_from_ibus(
          [{'id': 'foo1', 'languages': [{'id': 'fr'}]}], 'fooDir')
        # Verify
        mockIbusUtilInstance.write_preload_engines.assert_called_once_with(
          None,
          ['xkb:us::eng'])

    def test_UninstallKeyboardsFromIbus_RemoveKeyboard_RemovesAllMatching(self):
        # Setup
        mockIbusUtilInstance = self.mockIbusUtilClass.return_value
        mockIbusUtilInstance.read_preload_engines.return_value = [
          'xkb:us::eng', 'en:fooDir/foo1.kmx', 'fr:fooDir/foo1.kmx', 'km:fooDir/foo2.kmx']
        # Execute
        _uninstall_keyboards_from_ibus(
          [{'id': 'foo1', 'languages': [{'id': 'fr'}]}], 'fooDir')
        # Verify
        mockIbusUtilInstance.write_preload_engines.assert_called_once_with(
          None,
          ['xkb:us::eng', 'km:fooDir/foo2.kmx'])
