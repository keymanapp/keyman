#!/usr/bin/python3
import unittest

from keyman_config.canonical_language_code_utils import CanonicalLanguageCodeUtils


class CanonicalLanguageCodeUtilsTests(unittest.TestCase):
    def test_FindBestTag_EmptyTag(self):
        self.assertIsNone(CanonicalLanguageCodeUtils.findBestTag('', True))

    def test_FindBestTag_InvalidTag(self):
        self.assertIsNone(CanonicalLanguageCodeUtils.findBestTag('x', True))

    def test_FindBestTag_ValidCases(self):
        for testCase in [
            # #3485 - Gilaki (Latin) script
            { 'expected': 'glk-Arab-IR', 'tags': ['glk', 'glk-Arab', 'glk-Arab-IR', 'glk-IR'] },
            { 'expected': 'glk-Latn-IR', 'tags': ['glk-Latn', 'glk-Latn-IR'] },

            # #1719
            { 'expected': 'sqt-Arab-YE', 'tags': ['sqt', 'sqt-YE', 'sqt-Arab'] },
            { 'expected': 'sqt-Latn-YE', 'tags': ['sqt-Latn', 'sqt-Latn-YE'] },

            { 'expected': 'sa-Latn-IN', 'tags': ['sa-Latn'] },
            { 'expected': 'hi-Latn-IN', 'tags': ['hi-Latn'] },

            # #1282
            { 'expected': 'raw-Latn-MM', 'tags': ['raw', 'raw-MM', 'raw-Latn'] },

            # Various extended tags and tests
            { 'expected': 'km-KH', 'tags': ['km', 'km-kh', 'km-khmr', 'km-khmr-kh'] },
            { 'expected': 'th-TH', 'tags': ['th', 'th-th', 'th-thai-th'] },

            # A BCP 47 tag that is not in our canonicalization tables
            { 'expected': 'th-Latn-DE', 'tags': ['th-latn-de'] },

            { 'expected': 'fr-FR', 'tags': ['fr', 'fr-FR', 'fr-Latn-fr'] },
            { 'expected': 'arn-Latn-CL', 'tags': ['arn', 'arn-cl'] },
            { 'expected': 'se-Latn-NO', 'tags': ['se', 'se-NO'] },
            { 'expected': 'kma-Latn-GH', 'tags': ['kma', 'kma-latn', 'kma-latn-gh'] },
            { 'expected': 'tpi-PG', 'tags': ['tpi', 'tpi-PG', 'tpi-Latn-PG'] },
            { 'expected': 'sv-SE', 'tags': ['sv'] },
            { 'expected': 'en-US', 'tags': ['en'] },

            # fonipa
            { 'expected': 'en-US-fonipa', 'tags': ['en-fonipa'] },
            { 'expected': 'tpi-PG-fonipa', 'tags': ['tpi-Latn-fonipa'] },
            { 'expected': 'se-Latn-NO-fonipa', 'tags': ['se-fonipa', 'se-no-fonipa'] },
            { 'expected': 'fr-FR-fonipa', 'tags': ['fr-fonipa'] }
        ]:
            with self.subTest(testCase=testCase):
                expectedTag = testCase['expected']
                for tag in testCase['tags']:
                    with self.subTest(tag = tag):
                        # Execute
                        bestTag = CanonicalLanguageCodeUtils.findBestTag(tag, True)

                        # Verify
                        msg = "\nExpected: %s\ngot:      %s " % (expectedTag, bestTag)
                        self.assertEqual(expectedTag, bestTag, msg)

    def test_FindBestTag_NoAddingRegion(self):
        for testCase in [
            {'expected': 'fr', 'tags': ['fr']},
            {'expected': 'fr-FR', 'tags': ['fr-FR', 'fr-Latn-fr']},
            {'expected': 'en-fonipa', 'tags': ['en-fonipa']},
            {'expected': 'tpi-fonipa', 'tags': ['tpi-Latn-fonipa']},
            {'expected': 'se-Latn-fonipa', 'tags': ['se-fonipa']},
            {'expected': 'se-Latn-NO-fonipa', 'tags': ['se-no-fonipa']},
            {'expected': 'fr-fonipa', 'tags': ['fr-fonipa']}
        ]:
            with self.subTest(testCase=testCase):
                expectedTag = testCase['expected']
                for tag in testCase['tags']:
                    with self.subTest(tag=tag):
                        # Execute
                        bestTag = CanonicalLanguageCodeUtils.findBestTag(tag, False)

                        # Verify
                        msg = "\nExpected: %s\ngot:      %s " % (expectedTag, bestTag)
                        self.assertEqual(expectedTag, bestTag, msg)
