#!/usr/bin/python3
import unittest
from unittest.mock import patch

from keyman_config.uninstall_kmp import uninstall_keyboards_from_gnome


class UninstallKmpTests(unittest.TestCase):

    def setUp(self):
        patcher = patch('keyman_config.uninstall_kmp.GnomeKeyboardsUtil')
        self.mockGnomeKeyboardsUtilClass = patcher.start()
        self.addCleanup(patcher.stop)

    def test_UninstallKeyboardsFromGnome_RemoveOneKeyboardFromEmpty(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = []
        # Execute
        uninstall_keyboards_from_gnome([{'id': 'foo1'}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with([])

    def test_UninstallKeyboardsFromGnome_RemoveNonExistingKeyboard(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
            ('ibus', 'fooDir/foo2.kmx')]
        # Execute
        uninstall_keyboards_from_gnome([{'id': 'foo1'}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with([
            ('ibus', 'fooDir/foo2.kmx')])

    def test_UninstallKeyboardsFromGnome_RemoveOneKeyboard(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
            ('xkb', 'en'), ('ibus', 'fooDir/foo1.kmx')]
        # Execute
        uninstall_keyboards_from_gnome([{'id': 'foo1'}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
            [('xkb', 'en')])

    def test_UninstallKeyboardsFromGnome_RemoveMultipleKeyboards(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
            ('xkb', 'en'), ('ibus', 'fooDir/foo1.kmx'), ('ibus', 'fooDir/foo2.kmx')]
        # Execute
        uninstall_keyboards_from_gnome([{'id': 'foo1'}, {'id': 'foo2'}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
            [('xkb', 'en')])

    def test_UninstallKeyboardsFromGnome_RemoveAllKeyboards(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
            ('ibus', 'fooDir/foo1.kmx'), ('ibus', 'fooDir/foo2.kmx')]
        # Execute
        uninstall_keyboards_from_gnome([{'id': 'foo1'}, {'id': 'foo2'}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with([])

    def test_UninstallKeyboardsFromGnome_RemoveKeyboard_SingleLanguage(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
            ('xkb', 'en'), ('ibus', 'en:fooDir/foo1.kmx')]
        # Execute
        uninstall_keyboards_from_gnome([{'id': 'foo1', 'languages': [{'id': 'en'}]}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
            [('xkb', 'en')])

    def test_UninstallKeyboardsFromGnome_RemoveKeyboard_MultipleLanguages(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [
            ('xkb', 'en'), ('ibus', 'fr:fooDir/foo1.kmx')]
        # Execute
        uninstall_keyboards_from_gnome(
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
        uninstall_keyboards_from_gnome(
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
        uninstall_keyboards_from_gnome(
            [{'id': 'foo1', 'languages': [{'id': 'fr'}]}], 'fooDir')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
            [('xkb', 'en')])
