#!/usr/bin/python3
import os
import tempfile
import unittest
from unittest.mock import patch
from keyman_config.get_kmp import InstallLocation

from keyman_config.uninstall_kmp import _uninstall_keyboards_from_gnome, _uninstall_keyboards_from_ibus, _uninstall_kmp_common


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


class UninstallKmpCommonTests(unittest.TestCase):
    def setUp(self):
        patcher1 = patch('keyman_config.uninstall_kmp.get_keyboard_dir')
        self.mockGetKeyboardDir = patcher1.start()
        self.addCleanup(patcher1.stop)
        patcher2 = patch('keyman_config.uninstall_kmp.get_keyman_doc_dir')
        self.mockGetKeymanDocDir = patcher2.start()
        self.addCleanup(patcher2.stop)
        patcher3 = patch('keyman_config.uninstall_kmp.get_keyman_font_dir')
        self.mockGetKeymanFontDir = patcher3.start()
        self.addCleanup(patcher3.stop)
        patcher4 = patch('keyman_config.uninstall_kmp.is_gnome_desktop')
        self.mockIsGnomeShell = patcher4.start()
        self.addCleanup(patcher4.stop)
        self.mockIsGnomeShell.return_value = False
        patcher5 = patch('keyman_config.uninstall_kmp.is_fcitx_running')
        self.mockIsFcitxRunning = patcher5.start()
        self.addCleanup(patcher5.stop)
        self.mockIsFcitxRunning.return_value = False
        patcher6 = patch('keyman_config.uninstall_kmp._uninstall_keyboards_from_ibus')
        self.mockUninstallKeyboardsFromIbus = patcher6.start()
        self.addCleanup(patcher6.stop)
        patcher7 = patch('keyman_config.uninstall_kmp._uninstall_dir')
        self.mockUninstallDir = patcher7.start()
        self.addCleanup(patcher7.stop)
        patcher8 = patch('keyman_config.uninstall_kmp.get_metadata')
        self.mockGetMetaData = patcher8.start()
        self.addCleanup(patcher8.stop)
        patcher9 = patch('keyman_config.uninstall_kmp.CustomKeyboards')
        self.mockCustomKeyboardsClass = patcher9.start()
        self.addCleanup(patcher9.stop)

    def test_UninstallKmpCommon_NoRemoveLanguage(self):
        # Setup
        mockCustomKeyboardsInstance = self.mockCustomKeyboardsClass.return_value
        self.mockGetKeyboardDir.return_value = '/tmp/foo'
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}]}]
        self.mockGetMetaData.return_value = (None, None, None, keyboards, None)
        # Execute
        _uninstall_kmp_common(InstallLocation.User, 'foo1', False)
        # Verify
        self.mockUninstallKeyboardsFromIbus.assert_not_called()
        self.mockUninstallDir.assert_called()
        mockCustomKeyboardsInstance.remove.assert_not_called()

    def test_UninstallKmpCommon_RemoveLanguage(self):
        # Setup
        mockCustomKeyboardsInstance = self.mockCustomKeyboardsClass.return_value
        self.mockGetKeyboardDir.return_value = '/tmp/foo'
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}]}]
        self.mockGetMetaData.return_value = (None, None, None, keyboards, None)
        # Execute
        _uninstall_kmp_common(InstallLocation.User, 'foo1', True)
        # Verify
        self.mockUninstallKeyboardsFromIbus.assert_called_once_with(keyboards, '/tmp/foo')
        self.mockUninstallDir.assert_called()
        mockCustomKeyboardsInstance.remove.assert_called_once_with('/tmp/foo/foo1.kmx')

    def test_UninstallKmpCommon_RemoveLanguage_NoKeyboards(self):
        # Setup
        mockCustomKeyboardsInstance = self.mockCustomKeyboardsClass.return_value
        self.mockGetKeyboardDir.return_value = '/tmp/foo'
        keyboards = []
        self.mockGetMetaData.return_value = (None, None, None, keyboards, None)
        # Execute
        _uninstall_kmp_common(InstallLocation.User, 'foo1', True)
        # Verify
        self.mockUninstallKeyboardsFromIbus.assert_not_called()
        self.mockUninstallDir.assert_called()
        mockCustomKeyboardsInstance.remove.assert_not_called()

    def test_UninstallKmpCommon_RemoveLanguage_MultipleKeyboards(self):
        # Setup
        mockCustomKeyboardsInstance = self.mockCustomKeyboardsClass.return_value
        self.mockGetKeyboardDir.return_value = '/tmp/foo'
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}]}, {'id': 'bar2', 'languages': [{'id': 'fr'}]}]
        self.mockGetMetaData.return_value = (None, None, None, keyboards, None)
        # Execute
        _uninstall_kmp_common(InstallLocation.User, 'foo1', True)
        # Verify
        self.mockUninstallKeyboardsFromIbus.assert_called_once_with(keyboards, '/tmp/foo')
        self.mockUninstallDir.assert_called()
        self.assertEqual(mockCustomKeyboardsInstance.remove.call_count, 2)
