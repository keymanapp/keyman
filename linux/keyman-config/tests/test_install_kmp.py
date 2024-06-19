#!/usr/bin/python3
import os
import tempfile
import unittest
from unittest.mock import patch, ANY
from keyman_config import __version__
from keyman_config.get_kmp import InstallLocation
from keyman_config.install_kmp import InstallError, InstallKmp


class InstallKmpBase(unittest.TestCase):

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
        patcher5 = patch('keyman_config.install_kmp.get_keyboard_dir')
        self.mockGetKeyboardDir = patcher5.start()
        self.addCleanup(patcher5.stop)
        patcher6 = patch('keyman_config.install_kmp.extract_kmp')
        self.mockExtractKmp = patcher6.start()
        self.addCleanup(patcher6.stop)
        patcher7 = patch('keyman_config.install_kmp.extractico')
        self.mockExtractIco = patcher7.start()
        self.addCleanup(patcher7.stop)
        patcher8 = patch('keyman_config.install_kmp.is_gnome_desktop')
        self.mockIsGnomeShell = patcher8.start()
        self.addCleanup(patcher8.stop)
        self.mockIsGnomeShell.return_value = False
        patcher9 = patch('keyman_config.install_kmp.is_fcitx_running')
        self.mockIsFcitxRunning = patcher9.start()
        self.addCleanup(patcher9.stop)
        self.mockIsFcitxRunning.return_value = False
        patcher10 = patch('keyman_config.install_kmp.CustomKeyboards')
        self.mockCustomKeyboardsClass = patcher10.start()
        self.addCleanup(patcher10.stop)


class InstallKeyboardsToIbusTests(InstallKmpBase):
    def test_InstallKeyboardsToIbus_NoIbus(self):
        # Setup
        self.mockGetIbusBus.return_value = None
        # Execute
        InstallKmp()._install_keyboards_to_ibus([], None)
        # Verify
        self.mockRestartIbus.assert_not_called()

    def test_InstallKeyboardsToIbus_SingleKbNoLanguages(self):
        # Setup
        bus = self.mockGetIbusBus.return_value
        keyboards = [{'id': 'foo1'}]
        # Execute
        InstallKmp()._install_keyboards_to_ibus(keyboards, 'fooDir')
        # Verify
        self.mockInstallToIbus.assert_called_once_with(ANY, 'fooDir/foo1.kmx')
        self.mockRestartIbus.assert_called_once()
        bus.destroy.assert_called_once()

    def test_InstallKeyboardsToIbus_MultipleKbsNoLanguages(self):
        # Setup
        bus = self.mockGetIbusBus.return_value
        keyboards = [{'id': 'foo1'}, {'id': 'foo2'}]
        # Execute
        InstallKmp()._install_keyboards_to_ibus(keyboards, 'fooDir')
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
        InstallKmp()._install_keyboards_to_ibus(keyboards, 'fooDir')
        # Verify
        self.mockInstallToIbus.assert_called_once_with(ANY, 'en:fooDir/foo1.kmx')
        self.mockRestartIbus.assert_called_once()
        bus.destroy.assert_called_once()

    def test_InstallKeyboardsToIbus_SingleKbMultipleLanguages(self):
        # Setup
        bus = self.mockGetIbusBus.return_value
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}]
        # Execute
        InstallKmp()._install_keyboards_to_ibus(keyboards, 'fooDir')
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
        InstallKmp()._install_keyboards_to_ibus(keyboards, 'fooDir', 'fr')
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
        InstallKmp()._install_keyboards_to_ibus(keyboards, 'fooDir', 'de')
        # Verify
        self.mockInstallToIbus.assert_called_once()
        self.mockInstallToIbus.assert_called_with(ANY, 'de:fooDir/foo1.kmx')
        # self.mockInstallToIbus.assert_not_called_with(ANY, 'en:fooDir/foo1.kmx')
        self.mockRestartIbus.assert_called_once()
        bus.destroy.assert_called_once()


class InstallKeyboardsToGnomeTests(InstallKmpBase):
    def test_InstallKeyboardsToGnome_SingleKbNoLanguages(self):
        # Setup
        mockGnomeKeyboardsUtilInstance = self.mockGnomeKeyboardsUtilClass.return_value
        mockGnomeKeyboardsUtilInstance.read_input_sources.return_value = [('xkb', 'en')]
        keyboards = [{'id': 'foo1'}]
        # Execute
        InstallKmp()._install_keyboards_to_gnome(keyboards, 'fooDir')
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
        InstallKmp()._install_keyboards_to_gnome(keyboards, 'fooDir')
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
        InstallKmp()._install_keyboards_to_gnome(keyboards, 'fooDir')
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
        InstallKmp()._install_keyboards_to_gnome(keyboards, 'fooDir')
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
        InstallKmp()._install_keyboards_to_gnome(keyboards, 'fooDir', 'fr')
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
        InstallKmp()._install_keyboards_to_gnome(keyboards, 'fooDir', 'de')
        # Verify
        mockGnomeKeyboardsUtilInstance.write_input_sources.assert_called_once_with(
            [('xkb', 'en'), ('ibus', 'de:fooDir/foo1.kmx')])
        self.mockRestartIbus.assert_not_called()


class NormalizeLanguageTests(InstallKmpBase):
    def test_normalizeLanguage(self):
        # Setup
        languages = [
            {'id': 'de'},
            {'id': 'esi-Latn'},
            {'id': 'dyo'},
            {'id': 'fuh-Arab'}
        ]

        for testcase in [
            {'given': 'de', 'expected': 'de'},
            {'given': 'esi', 'expected': 'esi'},
            {'given': 'esi-Latn', 'expected': 'esi'},
            {'given': 'es', 'expected': None},
            {'given': 'en', 'expected': None},
            {'given': None, 'expected': None},
            # #3399
            {'given': 'dyo-latn', 'expected': 'dyo'},
            {'given': 'dyo', 'expected': 'dyo'},
            {'given': 'fuh-Arab', 'expected': 'fuh-Arab'},
            {'given': 'fuh', 'expected': None},
        ]:
            with self.subTest(data=testcase):
                # Execute
                result = InstallKmp()._normalize_language(languages, testcase['given'])

                # Verify
                self.assertEqual(result, testcase['expected'])

    def test_normalizeLanguage_noLanguages(self):
        # Setup
        languages = []

        # Execute
        result = InstallKmp()._normalize_language(languages, 'en')

        # Verify
        self.assertEqual(result, '')


class InstallKmpTests(InstallKmpBase):
    def _createEmptyKmp(self, workdir):
        kmpfile = os.path.join(workdir, 'foo.kmp')
        with open(kmpfile, 'w') as file:
            file.write('')
        return kmpfile

    def _createKmpJson(self, packagedir, fileVersionString):
        with open(os.path.join(packagedir, 'kmp.json'), 'w') as file:
            file.write('''{
                "system": {
                    "keymanDeveloperVersion": "15.0"
                    ''' + fileVersionString + '''
                },
                "files": [ {
                    "name": "khmer_angkor.kmx",
                    "description": "Keyboard Khmer Angkor"
                    }, {
                    "name": "kmp.json",
                    "description": "Package information (JSON)"
                    } ],
                "keyboards": [ {
                    "name": "Khmer Angkor",
                    "id": "khmer_angkor",
                    "version": "1.1",
                    "languages": [ {
                        "name": "Central Khmer (Khmer, Cambodia)",
                        "id": "km"
                        } ]}
                ]}''')

    def test_InstallKmp_FutureKeymanVersion(self):
        # Setup
        workdir = tempfile.TemporaryDirectory()
        packagedir = tempfile.TemporaryDirectory()
        self.mockGetKeyboardDir.return_value = packagedir.name
        kmpfile = self._createEmptyKmp(workdir.name)
        self._createKmpJson(packagedir.name, ', "fileVersion": "99.0"')

        # Execute
        with self.assertRaises(InstallError) as context:
            InstallKmp()._install_kmp(kmpfile, 'km', InstallLocation.User)

        # Verify
        self.assertTrue('foo.kmp requires Keyman 99.0 or higher' in context.exception.message)
        self.mockInstallToIbus.assert_not_called()

        # Teardown
        workdir.cleanup()
        packagedir.cleanup()

    def test_InstallKmp(self):
        for testcase in [
            {'name': 'PreviousKeymanVersion', 'fileVersion': ', "fileVersion": "7.0"'},
            {'name': 'SameKeymanVersion', 'fileVersion': f', "fileVersion": "{__version__}"'},
            {'name': 'NoFileVersion', 'fileVersion': ''}
        ]:
            with self.subTest(msg=testcase['name'], data=testcase['fileVersion']):
                # Setup
                self.mockInstallToIbus.reset_mock()
                workdir = tempfile.TemporaryDirectory()
                packagedir = tempfile.TemporaryDirectory()
                self.mockGetKeyboardDir.return_value = packagedir.name
                kmpfile = self._createEmptyKmp(workdir.name)
                self._createKmpJson(packagedir.name, testcase['fileVersion'])

                with patch('keyman_config.install_kmp.process_keyboard_data') as mock:
                    method = mock.return_value
                    method.return_value = None

                    # Execute
                    InstallKmp()._install_kmp(kmpfile, 'km', InstallLocation.User)

                # Verify
                self.mockInstallToIbus.assert_called_once()

                # Teardown
                workdir.cleanup()
                packagedir.cleanup()


class InstallKeyboardsTests(InstallKmpBase):
    def test_InstallKeyboards_NoLang(self):
        # Setup
        mockCustomKeyboardsInstance = self.mockCustomKeyboardsClass.return_value
        mockCustomKeyboardsInstance.get.return_value = None
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}]
        # Execute
        InstallKmp()._install_keyboards(keyboards, 'fooDir', None)
        # Verify
        self.mockInstallToIbus.assert_called_once_with(ANY, 'en:fooDir/foo1.kmx')
        mockCustomKeyboardsInstance.add.assert_not_called()

    def test_InstallKeyboards_LangInKeyboard(self):
        # Setup
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}]
        mockCustomKeyboardsInstance = self.mockCustomKeyboardsClass.return_value
        mockCustomKeyboardsInstance.get.return_value = None
        # Execute
        InstallKmp()._install_keyboards(keyboards, 'fooDir', 'fr')
        # Verify
        self.mockInstallToIbus.assert_called_once_with(ANY, 'fr:fooDir/foo1.kmx')
        mockCustomKeyboardsInstance.add.assert_not_called()

    def test_InstallKeyboards_CustomLang(self):
        # Setup
        mockCustomKeyboardsInstance = self.mockCustomKeyboardsClass.return_value
        mockCustomKeyboardsInstance.get.return_value = None
        keyboards = [{'id': 'foo1', 'languages': [{'id': 'en'}, {'id': 'fr'}]}]
        # Execute
        InstallKmp()._install_keyboards(keyboards, 'fooDir', 'mul')
        # Verify
        self.mockInstallToIbus.assert_called_once_with(ANY, 'mul:fooDir/foo1.kmx')
        mockCustomKeyboardsInstance.add.assert_called_once_with('mul:fooDir/foo1.kmx')


class AddCustomKeyboardTests(InstallKmpBase):
    def test_AddCustomKeyboard_NoLang(self):
        # Setup
        mockCustomKeyboardsInstance = self.mockCustomKeyboardsClass.return_value
        keyboard = {'id': 'foo1'}
        # Execute
        language = InstallKmp()._add_custom_keyboard(keyboard, 'fooDir', None)
        # Verify
        self.assertEqual(language, None)
        mockCustomKeyboardsInstance.add.assert_not_called()

    def test_AddCustomKeyboard_Add(self):
        # Setup
        mockCustomKeyboardsInstance = self.mockCustomKeyboardsClass.return_value
        keyboard = {'id': 'foo1'}
        # Execute
        language = InstallKmp()._add_custom_keyboard(keyboard, 'fooDir', 'mul')
        # Verify
        self.assertEqual(language, 'mul')
        mockCustomKeyboardsInstance.add.assert_called_once_with('mul:fooDir/foo1.kmx')
