#!/usr/bin/python3
import os
import tempfile
import unittest
from unittest.mock import patch

from keyman_config.get_kmp import keyman_cache_dir


class TestGetKmp(unittest.TestCase):
    def _setup_mock(self, arg0):
        patcher = patch(arg0)
        result = patcher.start()
        self.addCleanup(patcher.stop)
        return result

    def setUp(self):
        super().setUp()
        self.temp_dir = tempfile.TemporaryDirectory()
        self.mock_expanduser = self._setup_mock('os.path.expanduser')
        self.mock_expanduser.return_value = self.temp_dir.name

    def tearDown(self) -> None:
        self.temp_dir.cleanup()
        return super().tearDown()

    def test_keyman_cache_dir__all_exists(self):
        cache_dir = os.path.join(self.temp_dir.name, '.cache')
        os.mkdir(cache_dir)
        cache_dir = os.path.join(cache_dir, 'keyman')
        os.mkdir(cache_dir)

        keyman_cache = keyman_cache_dir()

        self.assertEqual(keyman_cache, cache_dir)
        self.assertTrue(os.path.exists(cache_dir))

    def test_keyman_cache_dir__cachedir_exists(self):
        cache_dir = os.path.join(self.temp_dir.name, '.cache')
        os.mkdir(cache_dir)

        keyman_cache = keyman_cache_dir()

        self.assertEqual(keyman_cache, os.path.join(cache_dir, 'keyman'))
        self.assertTrue(os.path.exists(os.path.join(cache_dir, 'keyman')))

    def test_keyman_cache_dir__xdg_cache_exists(self):
        cache_dir = os.path.join(self.temp_dir.name, '.cache')
        os.mkdir(cache_dir)
        os.environ['XDG_CACHE_HOME'] = cache_dir
        self.mock_expanduser.return_value = '/foo'

        keyman_cache = keyman_cache_dir()

        self.assertEqual(keyman_cache, os.path.join(cache_dir, 'keyman'))
        self.assertTrue(os.path.exists(os.path.join(cache_dir, 'keyman')))

    def test_keyman_cache_dir__not_exist(self):
        cache_dir = os.path.join(self.temp_dir.name, '.cache')

        keyman_cache = keyman_cache_dir()

        self.assertEqual(keyman_cache, os.path.join(cache_dir, 'keyman'))
        self.assertTrue(os.path.exists(os.path.join(cache_dir, 'keyman')))

    def test_keyman_cache_dir__cache_is_file(self):
        cache_dir = os.path.join(self.temp_dir.name, '.cache')
        with open(cache_dir, 'w', encoding='utf-8') as file:
            file.write('Test')

        keyman_cache = keyman_cache_dir()

        self.assertFalse(keyman_cache.startswith(self.temp_dir.name))
        self.assertTrue(os.path.exists(keyman_cache))

    def test_keyman_cache_dir__keyman_is_file(self):
        cache_dir = os.path.join(self.temp_dir.name, '.cache')
        os.mkdir(cache_dir)
        keyman_dir = os.path.join(cache_dir, 'keyman')
        with open(keyman_dir, 'w', encoding='utf-8') as file:
            file.write('Test')

        keyman_cache = keyman_cache_dir()

        self.assertFalse(keyman_cache.startswith(self.temp_dir.name))
        self.assertTrue(os.path.exists(keyman_cache))
