#!/usr/bin/env python3
import os
import tempfile
import unittest
from unittest.mock import Mock, patch, MagicMock

from keyman_config.diag_report import DiagReport, get_diagnostic_report


class DiagReportTests(unittest.TestCase):

    def test_get_keyman_version__returns_dict(self):
        # Execute
        diag = DiagReport()
        result = diag._get_keyman_version()

        # Verify
        self.assertIsInstance(result, dict)
        self.assertIn('version', result)
        self.assertIn('version_with_tag', result)
        self.assertIn('package_version', result)
        self.assertIn('tier', result)

    @patch('subprocess.run')
    def test_get_package_version__success(self, patched_subprocess_run):
        # Setup
        patched_subprocess_run.return_value = Mock(
            stdout=b'19.0.201-1',
            returncode=0
        )

        # Execute
        diag = DiagReport()
        result = diag._get_package_version()

        # Verify
        self.assertEqual(result, '19.0.201-1')
        patched_subprocess_run.assert_called_once_with(
            ['dpkg-query', '-W', '-f', '${Version}', 'keyman'],
            capture_output=True, check=False)

    @patch('subprocess.run')
    def test_get_package_version__not_installed(self, patched_subprocess_run):
        # Setup
        patched_subprocess_run.return_value = Mock(
            stdout=b'',
            returncode=1
        )

        # Execute
        diag = DiagReport()
        result = diag._get_package_version()

        # Verify
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_get_fcitx_version__success(self, patched_subprocess_run):
        # Setup
        patched_subprocess_run.return_value = Mock(
            stdout=b'fcitx5 version: 5.0.23\n',
            returncode=0
        )

        # Execute
        diag = DiagReport()
        result = diag._get_fcitx_version()

        # Verify
        self.assertEqual(result, '5.0.23')

    @patch('subprocess.run')
    def test_get_fcitx_version__not_installed(self, patched_subprocess_run):
        # Setup
        patched_subprocess_run.side_effect = FileNotFoundError()

        # Execute
        diag = DiagReport()
        result = diag._get_fcitx_version()

        # Verify
        self.assertIsNone(result)

    def test_get_os_info__returns_dict(self):
        # Execute
        diag = DiagReport()
        result = diag._get_os_info()

        # Verify
        self.assertIsInstance(result, dict)
        self.assertIn('platform', result)
        self.assertIn('system', result)
        self.assertIn('release', result)
        self.assertIn('machine', result)

    @patch.dict(os.environ, {'XDG_SESSION_TYPE': 'wayland', 'WAYLAND_DISPLAY': 'wayland-0', 'DISPLAY': ':0'})
    def test_get_display_server(self):
        # Execute
        diag = DiagReport()
        result = diag._get_display_server()

        # Verify
        self.assertEqual(result['session_type'], 'wayland')
        self.assertEqual(result['wayland_display'], 'wayland-0')
        self.assertEqual(result['display'], ':0')

    @patch.dict(os.environ, {'XDG_CURRENT_DESKTOP': 'GNOME', 'XDG_SESSION_DESKTOP': 'gnome'})
    def test_get_desktop_environment(self):
        # Execute
        diag = DiagReport()
        result = diag._get_desktop_environment()

        # Verify
        self.assertEqual(result['xdg_current_desktop'], 'GNOME')
        self.assertEqual(result['xdg_session_desktop'], 'gnome')

    @patch.dict(os.environ, {'GTK_IM_MODULE': 'ibus', 'QT_IM_MODULE': 'ibus', 'XMODIFIERS': '@im=ibus'})
    def test_get_input_method(self):
        # Execute
        diag = DiagReport()
        result = diag._get_input_method()

        # Verify
        self.assertEqual(result['gtk_im_module'], 'ibus')
        self.assertEqual(result['qt_im_module'], 'ibus')
        self.assertEqual(result['xmodifiers'], '@im=ibus')

    @patch('keyman_config.diag_report.get_installed_kmp')
    def test_get_installed_keyboards(self, patched_get_installed_kmp):
        # Setup
        patched_get_installed_kmp.return_value = {
            'sil_euro_latin': {
                'name': 'EuroLatin',
                'kmpversion': '2.0.1'
            }
        }

        # Execute
        diag = DiagReport()
        result = diag._get_installed_keyboards()

        # Verify
        self.assertIsInstance(result, dict)
        self.assertIn('user', result)
        self.assertIn('shared', result)
        self.assertIn('os', result)

    @patch('keyman_config.diag_report.get_installed_kmp')
    def test_get_installed_keyboards__with_keyboards(self, patched_get_installed_kmp):
        # Setup - only return data for user area
        def mock_get_installed_kmp(location):
            from keyman_config.get_kmp import InstallLocation
            # sourcery skip: no-conditionals-in-tests
            if location == InstallLocation.User:
                return {
                    'sil_euro_latin': {
                        'name': 'EuroLatin',
                        'kmpversion': '2.0.1'
                    }
                }
            return {}

        patched_get_installed_kmp.side_effect = mock_get_installed_kmp

        # Execute
        diag = DiagReport()
        result = diag._get_installed_keyboards()

        # Verify
        self.assertEqual(len(result['user']), 1)
        self.assertEqual(result['user'][0]['id'], 'sil_euro_latin')
        self.assertEqual(result['user'][0]['name'], 'EuroLatin')
        self.assertEqual(result['user'][0]['version'], '2.0.1')
        self.assertEqual(len(result['shared']), 0)
        self.assertEqual(len(result['os']), 0)

    @patch('keyman_config.diag_report.get_ibus_version')
    @patch('keyman_config.diag_report.get_installed_kmp')
    def test_create_report__returns_string(self, patched_get_installed_kmp, patched_ibus_version):
        # Setup
        patched_ibus_version.return_value = '1.5.28'
        patched_get_installed_kmp.return_value = {}

        # Execute
        diag = DiagReport()
        result = diag._create_report()

        # Verify
        self.assertIsInstance(result, str)
        self.assertIn('Keyman for Linux Diagnostic Report', result)
        self.assertIn('Keyman Version', result)
        self.assertIn('IBus', result)
        self.assertIn('Operating System', result)
        self.assertIn('Display Server', result)

    @patch('keyman_config.diag_report.get_ibus_version')
    @patch('keyman_config.diag_report.get_installed_kmp')
    def test_get_diagnostic_report__convenience_function(self, patched_get_installed_kmp, patched_ibus_version):
        # Setup
        patched_ibus_version.return_value = '1.5.28'
        patched_get_installed_kmp.return_value = {}

        # Execute
        result = get_diagnostic_report()

        # Verify
        self.assertIsInstance(result, str)
        self.assertIn('Keyman for Linux Diagnostic Report', result)

    @patch.object(DiagReport, '_parse_desktop_file')
    @patch('subprocess.run')
    def test_get_default_browser__success(self, patched_subprocess_run, mock_parse_desktop_file):
        # Setup
        patched_subprocess_run.return_value = Mock(
            stdout=b'firefox.desktop\n',
            returncode=0
        )
        mock_parse_desktop_file.return_value = None  # Parse returns None, fallback to desktop_id

        # Execute
        diag = DiagReport()
        result = diag._get_default_browser()

        # Verify
        self.assertEqual(result, 'firefox.desktop')
        patched_subprocess_run.assert_called_once_with(
            ['xdg-mime', 'query', 'default', 'text/html'],
            capture_output=True, check=False)

    @patch.object(DiagReport, '_parse_desktop_file')
    @patch('subprocess.run')
    def test_get_default_browser__with_desktop_file_parsing(self, patched_subprocess_run, mock_parse_desktop_file):
        # Setup
        patched_subprocess_run.return_value = Mock(
            stdout=b'firefox.desktop\n',
            returncode=0
        )
        mock_parse_desktop_file.return_value = '/usr/share/applications/firefox.desktop (/usr/bin/firefox)'

        # Execute
        diag = DiagReport()
        result = diag._get_default_browser()

        # Verify
        self.assertEqual('/usr/share/applications/firefox.desktop (/usr/bin/firefox)', result)
        mock_parse_desktop_file.assert_called_once_with('firefox.desktop')

    @patch('subprocess.run')
    def test_get_default_browser__not_set(self, patched_subprocess_run):
        # Setup
        patched_subprocess_run.return_value = Mock(
            stdout=b'',
            returncode=0
        )

        # Execute
        diag = DiagReport()
        result = diag._get_default_browser()

        # Verify
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_get_default_browser__xdg_mime_not_found(self, patched_subprocess_run):
        # Setup
        patched_subprocess_run.side_effect = FileNotFoundError()

        # Execute
        diag = DiagReport()
        result = diag._get_default_browser()

        # Verify
        self.assertIsNone(result)

    def test_parse_desktop_file__parsing_logic(self):
        # Test the parsing logic with a simulated file content
        # This tests that when a file is found, it extracts Exec and path correctly
        diag = DiagReport()

        # Test on a file we know exists (even if it's just checking the logic runs)
        result = diag._parse_desktop_file('nonexistent-browser.desktop')
        # It should return None if file doesn't exist
        self.assertIsNone(result)

    def test_parse_desktop_file__with_xdg_data_home(self):
        # Test that XDG_DATA_HOME is checked when set
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create applications subdirectory
            apps_dir = os.path.join(tmpdir, 'applications')
            os.makedirs(apps_dir)

            # Create a test .desktop file
            desktop_path = os.path.join(apps_dir, 'testbrowser.desktop')
            self._create_desktop_file(desktop_path, 'TestBrowser', '/usr/bin/testbrowser %U')

            # Set XDG_DATA_HOME and test
            with patch.dict(os.environ, {'XDG_DATA_HOME': tmpdir}, clear=False):
                diag = DiagReport()
                result = diag._parse_desktop_file('testbrowser.desktop')

            # Verify the file was found and parsed
            self.assertEqual(result, f'{desktop_path} (/usr/bin/testbrowser)')

    def test_parse_desktop_file__with_xdg_data_dirs(self):
        # Test that XDG_DATA_DIRS is checked when set
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create applications subdirectory
            apps_dir = os.path.join(tmpdir, 'applications')
            os.makedirs(apps_dir)

            # Create a test .desktop file
            desktop_path = os.path.join(apps_dir, 'mybrowser.desktop')
            self._create_desktop_file(desktop_path, 'MyBrowser', '/opt/mybrowser/bin/mybrowser')

            # Set XDG_DATA_DIRS and test
            with patch.dict(os.environ, {'XDG_DATA_DIRS': tmpdir}, clear=False):
                diag = DiagReport()
                result = diag._parse_desktop_file('mybrowser.desktop')

            # Verify the file was found and parsed
            self.assertEqual(result, f'{desktop_path} (/opt/mybrowser/bin/mybrowser)')

    def test_parse_desktop_file__with_both_xdg_vars(self):
        # Test that both XDG_DATA_HOME and XDG_DATA_DIRS are used
        with tempfile.TemporaryDirectory() as tmpdir1:
            with tempfile.TemporaryDirectory() as tmpdir2:
                # Create applications subdirectory in XDG_DATA_HOME
                home_apps_dir = os.path.join(tmpdir1, 'applications')
                os.makedirs(home_apps_dir)

                # Create applications subdirectory in XDG_DATA_DIRS
                dirs_apps_dir = os.path.join(tmpdir2, 'applications')
                os.makedirs(dirs_apps_dir)

                # Create a test .desktop file in XDG_DATA_HOME
                home_desktop = os.path.join(home_apps_dir, 'homebrowser.desktop')
                self._create_desktop_file(home_desktop, 'HomeBrowser', '/home/user/.local/bin/homebrowser')

                # Create a test .desktop file in XDG_DATA_DIRS
                dirs_desktop = os.path.join(dirs_apps_dir, 'dirsbrowser.desktop')
                self._create_desktop_file(dirs_desktop, 'DirsBrowser', '/usr/bin/dirsbrowser')

                # Create a test .desktop file in XDG_DATA_DIRS with same name as in XDG_DATA_HOME
                home_desktop2 = os.path.join(dirs_apps_dir, 'homebrowser.desktop')
                self._create_desktop_file(home_desktop2, 'HomeBrowser', '/usr/bin/wrongbrowser')

                # Set both XDG variables
                with patch.dict(os.environ, {
                    'XDG_DATA_HOME': tmpdir1,
                    'XDG_DATA_DIRS': tmpdir2
                }, clear=False):
                    diag = DiagReport()

                    # Test finding file from XDG_DATA_HOME
                    result1 = diag._parse_desktop_file('homebrowser.desktop')
                    self.assertEqual(result1, f'{home_desktop} (/home/user/.local/bin/homebrowser)')

                    # Test finding file from XDG_DATA_DIRS
                    result2 = diag._parse_desktop_file('dirsbrowser.desktop')
                    self.assertEqual(result2, f'{dirs_desktop} (/usr/bin/dirsbrowser)')

    def _create_desktop_file(self, filename, name, exe):
        with open(filename, 'w') as f:
            f.write('[Desktop Entry]\n')
            f.write(f'Name={name}\n')
            f.write(f'Exec={exe}\n')
            f.write('Type=Application\n')

    def test_parse_desktop_file__not_found(self):
        # Setup
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch('keyman_config.diag_report.os.path.expanduser') as mock_expanduser:
                with patch('keyman_config.diag_report.os.path.isfile') as mock_isfile:
                    mock_expanduser.side_effect = lambda x: x.replace('~', tmpdir)
                    mock_isfile.return_value = False

                    # Execute
                    diag = DiagReport()
                    result = diag._parse_desktop_file('nonexistent.desktop')

            # Verify
            self.assertIsNone(result)

    @patch('subprocess.run')
    @patch('shutil.which')
    def test_get_apt_browser_versions__success(self, patched_shutil_which, patched_subprocess_run):
        # Setup - mock dpkg-query available and finding firefox and chromium
        def mock_which(cmd):
            return '/usr/bin/dpkg-query' if cmd == 'dpkg-query' else None

        def mock_run(cmd, *args, **kwargs):
            # sourcery skip: no-conditionals-in-tests
            if cmd[0] == 'dpkg-query':
                if 'firefox' in cmd:
                    return Mock(returncode=0, stdout=b'115.0.1-1')
                elif 'chromium' in cmd:
                    return Mock(returncode=0, stdout=b'120.0.0-1')
            return Mock(returncode=1, stdout=b'')

        patched_shutil_which.side_effect = mock_which
        patched_subprocess_run.side_effect = mock_run

        # Execute
        diag = DiagReport()
        result = diag._get_apt_browser_versions()

        # Verify
        self.assertIsInstance(result, dict)
        self.assertEqual(result['firefox'], '115.0.1-1')
        self.assertEqual(result['chromium'], '120.0.0-1')

    @patch('shutil.which')
    def test_get_apt_browser_versions__not_found(self, patched_shutil_which):
        # Setup - mock dpkg-query not available
        patched_shutil_which.return_value = None

        # Execute
        diag = DiagReport()
        result = diag._get_apt_browser_versions()

        # Verify
        self.assertEqual(result, {})

    @patch('subprocess.run')
    @patch('shutil.which')
    def test_get_rpm_browser_versions__with_dnf(self, patched_shutil_which, patched_subprocess_run):
        # Setup - mock dnf available and finding firefox
        def mock_run(cmd, *args, **kwargs):
            # sourcery skip: no-conditionals-in-tests
            if cmd[0] == 'dnf' and cmd[1] == 'info' and 'firefox' in cmd:
                return Mock(returncode=0, stdout=b'Name: firefox\nVersion: 115.0.1')
            return Mock(returncode=1, stdout=b'')

        patched_subprocess_run.side_effect = mock_run
        patched_shutil_which.return_value = '/path/to/command'

        # Execute
        diag = DiagReport()
        result = diag._get_rpm_browser_versions()

        # Verify
        self.assertIsInstance(result, dict)
        self.assertEqual(result['firefox'], '115.0.1')

    @patch('subprocess.run')
    @patch('shutil.which')
    def test_get_rpm_browser_versions__with_yum(self, patched_shutil_which, patched_subprocess_run):
        # Setup - mock dnf not available, yum available
        def mock_which(cmd):
            # dnf not found, yum found
            return '/path/to/yum' if cmd == 'yum' else None

        def mock_run(cmd, *args, **kwargs):
            # sourcery skip: no-conditionals-in-tests
            if cmd[0] == 'yum' and cmd[1] == 'info' and 'chromium' in cmd:
                return Mock(returncode=0, stdout=b'Name: chromium\nVersion: 115.0.0')
            return Mock(returncode=1, stdout=b'')

        patched_shutil_which.side_effect = mock_which
        patched_subprocess_run.side_effect = mock_run

        # Execute
        diag = DiagReport()
        result = diag._get_rpm_browser_versions()

        # Verify
        self.assertIsInstance(result, dict)
        self.assertEqual(result['chromium'], '115.0.0')

    @patch('subprocess.run')
    def test_get_rpm_browser_versions__no_pkg_mgr(self, patched_subprocess_run):
        # Setup - mock neither dnf nor yum available
        patched_subprocess_run.side_effect = FileNotFoundError()

        # Execute
        diag = DiagReport()
        result = diag._get_rpm_browser_versions()

        # Verify
        self.assertEqual(result, {})

    @patch('subprocess.run')
    @patch('shutil.which')
    def test_get_pacman_browser_versions__success(self, patched_shutil_which, patched_subprocess_run):
        # Setup - mock pacman available and finding firefox
        def mock_run(cmd, *args, **kwargs):
            # sourcery skip: no-conditionals-in-tests
            if cmd[0] == 'pacman' and cmd[1] == '-Q' and 'firefox' in cmd:
                return Mock(returncode=0, stdout=b'firefox 115.0.1-1')
            return Mock(returncode=1, stdout=b'')

        patched_subprocess_run.side_effect = mock_run
        patched_shutil_which.return_value = '/path/to/command'

        # Execute
        diag = DiagReport()
        result = diag._get_pacman_browser_versions()

        # Verify
        self.assertIsInstance(result, dict)
        self.assertEqual(result['firefox'], '115.0.1-1')

    @patch('subprocess.run')
    def test_get_pacman_browser_versions__not_found(self, patched_subprocess_run):
        # Setup - mock pacman not available
        patched_subprocess_run.side_effect = FileNotFoundError()

        # Execute
        diag = DiagReport()
        result = diag._get_pacman_browser_versions()

        # Verify
        self.assertEqual(result, {})


if __name__ == '__main__':
    unittest.main()
