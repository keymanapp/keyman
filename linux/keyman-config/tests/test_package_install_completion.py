#!/usr/bin/python3
import os
import tempfile
import unittest
from unittest.mock import patch
from importlib.machinery import SourceFileLoader
from importlib.util import module_from_spec, spec_from_loader


class PackageInstallCompletionTests(unittest.TestCase):

    def setUp(self):
        self.mockExtractKmp = self._setupMock('keyman_config.install_kmp.extract_kmp')
        self.mockGetMetadata = self._setupMock('keyman_config.kmpmetadata.get_metadata')
        loader = SourceFileLoader('km_package_install', os.path.join(os.path.dirname(
          os.path.abspath(__file__)), '../km-package-install'))
        if spec := spec_from_loader(loader.name, loader):
            self.mod = module_from_spec(spec)
            loader.exec_module(self.mod)

        self.tempDir = tempfile.TemporaryDirectory()
        os.environ["XDG_CACHE_HOME"] = self.tempDir.name
        self.cacheDir = os.path.join(self.tempDir.name, 'keyman')
        os.makedirs(self.cacheDir)

    def tearDown(self):
        self.tempDir.cleanup()

    def _setupMock(self, arg0):
        patcher = patch(arg0)
        result = patcher.start()
        self.addCleanup(patcher.stop)
        return result

    def _list_languages_for_keyboard_impl(self, packageId):
        return self.mod._list_languages_for_keyboard_impl(packageId, 'someDir')

    def test_PackageCompletionNoLanguage(self):
        with open(os.path.join(self.cacheDir, 'foo'), 'w'):
            self.mockGetMetadata.return_value = (None, None, None, [{}], None)
            result = self._list_languages_for_keyboard_impl('foo')
            self.assertEqual(result, "")

    def test_PackageCompletionOneLanguage(self):
        with open(os.path.join(self.cacheDir, 'khmer_angkor'), 'w'):
            self.mockGetMetadata.return_value = (
              None, None, None,
              [{'languages': [{'name': 'Central Khmer (Khmer, Cambodia)', 'id': 'km'}]}],
              None
            )
            result = self._list_languages_for_keyboard_impl('khmer_angkor')
            self.assertEqual(result, "km")

    def test_PackageCompletionMultipleLanguages(self):
        with open(os.path.join(self.cacheDir, 'sil_euro_latin'), 'w'):
            self.mockGetMetadata.return_value = (
              None, None, None,
              [{'languages': [
                {'name': 'English', 'id': 'en'},
                {'name': 'French', 'id': 'fr'},
                {'name': 'German', 'id': 'de'}]}],
              None
            )
            result = self._list_languages_for_keyboard_impl('sil_euro_latin')
            self.assertEqual(result, 'en\nfr\nde')
