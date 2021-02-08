#!/usr/bin/python3
import unittest

from keyman_config.standards.lang_tags_map import LangTagsMap


class LangTagsMapTests(unittest.TestCase):
    def test_LookupLangTags_ValidTag(self):
        # Execute
        tag = LangTagsMap.lookupLangTags('aa')
        # Verify
        self.assertEqual('aa-Latn-ET', tag['full'])

    def test_LookupLangTags_CustomTag(self):
        self.assertIsNone(LangTagsMap.lookupLangTags('qaa'))

    def test_LookupLangTags_AddsMissingFields(self):
        # Execute
        tag = LangTagsMap.lookupLangTags('sga')
        # Verify
        self.assertEqual('sga-Ogam-IE', tag['full'])
        self.assertFalse(tag['suppress'])

    def test_LookupLangTags_ScriptSuppress(self):
        tag = LangTagsMap.lookupLangTags('ab')
        self.assertTrue(tag['suppress'])

    def test_LookupLangTags_NoScriptSuppress(self):
        tag = LangTagsMap.lookupLangTags('ab-Geor')
        self.assertFalse(tag['suppress'])

    def test_LookupAllTags_FromTag(self):
        self.assertEqual('se-Cyrl', LangTagsMap.lookupAllTags('se-Cyrl'))

    def test_LookupAllTags_FromFull(self):
        self.assertEqual('se-Cyrl', LangTagsMap.lookupAllTags('se-Cyrl-NO'))

    def test_TranslateISO6393ToBCP47_HasTwoLetterCode(self):
        self.assertEqual('ar', LangTagsMap.translateISO6393ToBCP47('ara'))

    def test_TranslateISO6393ToBCP47_HasThreeLetterCode(self):
        self.assertEqual('zkt', LangTagsMap.translateISO6393ToBCP47('zkt'))
