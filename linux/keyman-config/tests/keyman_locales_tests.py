#!/usr/bin/env python3
#
# Keyman is copyright (C) SIL Global. MIT License.
# 
# Created by Marc Durdin on 2026-08-19
# 
# Unit tests for find_locales
# 
import os
from unittest import TestCase, mock
from keyman_config.keyman_locales import find_locales

class FindLocalesTests(TestCase):
    @mock.patch.dict(os.environ, {"LANGUAGE": "en_US.UTF-8:de_DE.UTF-8"}, True)
    def test_find_locales_LANGUAGE(self):
        loc = find_locales()
        self.assertEqual(loc, ['en_US', 'de_DE', 'C'])

    @mock.patch.dict(os.environ, {"LANGUAGE": "en_US.UTF-8:de_DE.UTF-8", "LANG": "fr_FR"}, True)
    def test_find_locales_LANGUAGE_and_LANG(self):
        loc = find_locales()
        self.assertEqual(loc, ['en_US', 'de_DE', 'C'])

    @mock.patch.dict(os.environ, {"LANG": "de_DE.UTF-8@euro"}, True)
    def test_find_locales_LANG(self):
        loc = find_locales()
        self.assertEqual(loc, ['de_DE', 'C'])

    @mock.patch.dict(os.environ, {"LC_MESSAGES": "km_Khmr_KH.UTF-8"}, True)
    def test_find_locales_LC_MESSAGES(self):
        loc = find_locales()
        self.assertEqual(loc, ['km_Khmr_KH', 'C'])

    @mock.patch.dict(os.environ, {"LC_ALL": "km_Khmr_KH.UTF-8"}, True)
    def test_find_locales_LC_ALL(self):
        loc = find_locales()
        self.assertEqual(loc, ['km_Khmr_KH', 'C'])
