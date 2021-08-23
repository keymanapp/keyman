#!/usr/bin/python3
import unittest
from unittest.mock import patch

from keyman_config import KeymanComUrl, __tier__
from keyman_config.handle_install import download_and_install_package


class HandleInstallTests(unittest.TestCase):

    @patch('keyman_config.handle_install.is_zipfile')
    @patch('keyman_config.handle_install._install_package')
    def test_downloadAndInstallPackage_file(self, installPackageMethod, mockIsZipfile):
        # Setup
        mockIsZipfile.return_value = True

        # Execute
        download_and_install_package('/tmp/keyboard/sil_euro_latin.kmp')

        # Verify
        installPackageMethod.assert_called_with('/tmp/keyboard/sil_euro_latin.kmp', '')

    @patch('keyman_config.handle_install.is_zipfile')
    @patch('keyman_config.handle_install._install_package')
    def test_downloadAndInstallPackage_fileUrl(self, installPackageMethod, mockIsZipfile):
        # Setup
        mockIsZipfile.return_value = True

        # Execute
        download_and_install_package('file:///tmp/keyboard/sil_euro_latin.kmp')

        # Verify
        installPackageMethod.assert_called_with('/tmp/keyboard/sil_euro_latin.kmp', '')

    @patch('keyman_config.handle_install.is_zipfile')
    @patch('keyman_config.handle_install._install_package')
    def test_downloadAndInstallPackage_fileUrlWithBcp47(self, installPackageMethod,
                                                        mockIsZipfile):
        # Setup
        mockIsZipfile.return_value = True

        # Execute
        download_and_install_package('file:///tmp/keyboard/sil_euro_latin.kmp?bcp47=dyo')

        # Verify
        installPackageMethod.assert_called_with('/tmp/keyboard/sil_euro_latin.kmp', 'dyo')

    @patch('keyman_config.handle_install.is_zipfile')
    @patch('keyman_config.handle_install._install_package')
    def test_downloadAndInstallPackage_fileUrlWithBcp47AndVersion(self, installPackageMethod,
                                                                  mockIsZipfile):
        # Setup
        mockIsZipfile.return_value = True

        # Execute
        download_and_install_package('file:///tmp/keyboard/sil_euro_latin.kmp?bcp47=dyo&version=1')

        # Verify
        installPackageMethod.assert_called_with('/tmp/keyboard/sil_euro_latin.kmp', 'dyo')

    @patch('keyman_config.handle_install.is_zipfile')
    @patch('keyman_config.handle_install._install_package')
    def test_downloadAndInstallPackage_InvalidUrl(self, installPackageMethod, mockIsZipfile):
        # Setup
        mockIsZipfile.return_value = True

        # Execute
        download_and_install_package('http://localhost/keyboard/sil_euro_latin.kmp')

        # Verify
        installPackageMethod.assert_not_called()

    @patch('keyman_config.handle_install.is_zipfile')
    @patch('keyman_config.handle_install._install_package')
    @patch('keyman_config.get_kmp.download_kmp_file')
    def test_downloadAndInstallPackage_invalidUrl(self, downloadKmpFileMethod,
                                                  installPackageMethod, mockIsZipfile):
        for url in ['foo://download/keyboard/sil_euro_latin', 'keyman://keyboard/sil_euro_latin',
                    'keyman://download/sil_euro_latin', 'keyman://download/keyboard/']:
            with self.subTest(url=url):
                # Setup
                mockIsZipfile.return_value = True

                # Execute
                download_and_install_package(url)

                # Verify
                downloadKmpFileMethod.assert_not_called()
                installPackageMethod.assert_not_called()

    @patch('keyman_config.handle_install.is_zipfile')
    @patch('keyman_config.handle_install._install_package')
    @patch('keyman_config.get_kmp.keyman_cache_dir')
    @patch('keyman_config.handle_install.download_kmp_file')
    def test_downloadAndInstallPackage_keymanUrl(self, downloadKmpFileMethod,
                                                 keymanCacheDirMethod, installPackageMethod,
                                                 mockIsZipfile):
        # Setup
        mockPackagePath = '/tmp/sil_euro_latin'
        keymanCacheDirMethod.return_value = '/tmp'
        downloadKmpFileMethod.return_value = mockPackagePath
        mockIsZipfile.return_value = True

        # Execute
        download_and_install_package('keyman://download/keyboard/sil_euro_latin')

        # Verify
        downloadKmpFileMethod.assert_called_with(
            KeymanComUrl + '/go/package/download/sil_euro_latin?platform=linux&tier=' + __tier__,
            mockPackagePath)
        installPackageMethod.assert_called_with(mockPackagePath, '')

    @patch('keyman_config.handle_install.is_zipfile')
    @patch('keyman_config.handle_install._install_package')
    @patch('keyman_config.get_kmp.keyman_cache_dir')
    @patch('keyman_config.handle_install.download_kmp_file')
    def test_downloadAndInstallPackage_keymanUrlWithBcp47(self, downloadKmpFileMethod,
                                                          keymanCacheDirMethod,
                                                          installPackageMethod, mockIsZipfile):
        # Setup
        mockPackagePath = '/tmp/sil_euro_latin'
        keymanCacheDirMethod.return_value = '/tmp'
        downloadKmpFileMethod.return_value = mockPackagePath
        mockIsZipfile.return_value = True

        # Execute
        download_and_install_package('keyman://download/keyboard/sil_euro_latin?bcp47=de')

        # Verify
        downloadKmpFileMethod.assert_called_with(
            KeymanComUrl + '/go/package/download/sil_euro_latin?platform=linux&tier=' + __tier__,
            mockPackagePath)
        installPackageMethod.assert_called_with(mockPackagePath, 'de')


if __name__ == '__main__':
    unittest.main()
