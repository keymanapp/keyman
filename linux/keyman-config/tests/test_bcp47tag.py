#!/usr/bin/python3
import unittest

from keyman_config.bcp47tag import Bcp47Tag


class Bcp47TagTests(unittest.TestCase):
    def test_Create_InvalidTags(self):
        for tag in [
            'x',
            # test cases from https://tools.ietf.org/html/bcp47#appendix-A
            # Some Invalid Tags:
            'de-419-DE', # two region tags
            'a-DE', # use of a single-character subtag in primary position; note that there are a few grandfathered tags that start with "i-" that are valid
            'ar-a-aaa-b-bbb-a-ccc',  # two extensions with same single-letter prefix
        ]:
            with self.subTest(tag=tag):
                bcp47 = Bcp47Tag.create(tag)
                self.assertIsNone(bcp47)

    def test_Create_ValidTags(self):
        for testTuple in [
            # test cases from https://tools.ietf.org/html/bcp47#appendix-A
            # Simple language subtag:
            ('de', Bcp47Tag('de')), # German
            ('fr', Bcp47Tag('fr')), # French
            ('ja', Bcp47Tag('ja')), # Japanese
            ('i-enochian', Bcp47Tag(None, None, None, [], [], [], 'i-enochian')),  # example of a grandfathered tag
            # Language subtag plus Script subtag:
            ('zh-Hant', Bcp47Tag('zh', 'Hant')), # Chinese written using the Traditional Chinese script
            ('zh-Hans', Bcp47Tag('zh', 'Hans')), # Chinese written using the Simplified Chinese script
            ('sr-Cyrl', Bcp47Tag('sr', 'Cyrl')), # Serbian written using the Cyrillic script
            ('sr-Latn', Bcp47Tag('sr', 'Latn')), # Serbian written using the Latin script
            # Language-Script-Region:
            ('zh-Hans-CN', Bcp47Tag('zh', 'Hans', 'CN')), # Chinese written using the Simplified script as used in mainland China
            ('sr-Latn-RS', Bcp47Tag('sr', 'Latn', 'RS')), # Serbian written using the Latin script as used in Serbia
            # Language-Variant:
            ('sl-rozaj', Bcp47Tag('sl', None, None, [], ['rozaj'])), # Resian dialect of Slovenian
            ('sl-rozaj-biske', Bcp47Tag('sl', None, None, [], ['rozaj', 'biske'])),  # San Giorgio dialect of Resian dialect of Slovenian
            # Language-Region-Variant:
            ('de-CH-1901', Bcp47Tag('de', None, 'CH', [], ['1901'])), # German as used in Switzerland using the 1901 variant [orthography]
            ('sl-IT-nedis', Bcp47Tag('sl', None, 'IT', [], ['nedis'])), # Slovenian as used in Italy, Nadiza dialect
            # Language-Script-Region-Variant:
            ('hy-Latn-IT-arevela', Bcp47Tag('hy', 'Latn', 'IT', [], ['arevela'])), # Eastern Armenian written in Latin script, as used in Italy
            # Language-Region:
            ('de-DE', Bcp47Tag('de', None, 'DE')), # German for Germany
            ('en-US', Bcp47Tag('en', None, 'US')), # English as used in the United States
            ('es-419', Bcp47Tag('es', None, '419')), # Spanish appropriate for the Latin America and Caribbean region using the UN region code
            # Extended language subtags and their primary language subtag counterparts:
            ('cmn-Hans-CN', Bcp47Tag('cmn', 'Hans', 'CN')),  # Mandarin Chinese, Simplified script, as used in China
            ('yue-HK', Bcp47Tag('yue', None, 'HK')),  # Cantonese Chinese, as used in Hong Kong SAR
            ('zh-cmn-Hans-CN', Bcp47Tag('zh', 'Hans', 'CN', [], [], [], None, ['cmn'])), # Chinese, Mandarin, Simplified script, as used in China
            ('zh-yue-HK', Bcp47Tag('zh', None, 'HK', [], [], [], None, ['yue'])), # Chinese, Cantonese, as used in Hong Kong SAR
            # Private use subtags:
            ('de-CH-x-phonebk', Bcp47Tag('de', None, 'CH', [], [], [], None, [], ['x', 'phonebk'])),
            ('az-Arab-x-AZE-derbend', Bcp47Tag('az', 'Arab', None, [], [], [], None, [], ['x', 'AZE', 'derbend'])),
            # Private use registry values:
            ('x-whatever', Bcp47Tag(None, None, None, ['whatever'])), # private use using the singleton 'x'
            ('qaa-Qaaa-QM-x-southern', Bcp47Tag('qaa', 'Qaaa', 'QM', [], [], [], None, [], ['x', 'southern'])),  # all private tags
            ('de-Qaaa', Bcp47Tag('de', 'Qaaa')), # German, with a private script
            ('sr-Latn-QM', Bcp47Tag('sr', 'Latn', 'QM')), # Serbian, Latin script, private region
            ('sr-Qaaa-RS', Bcp47Tag('sr', 'Qaaa', 'RS')), # Serbian, private script, for Serbia
            # Tags that use extensions:
            ('en-US-u-islamcal', Bcp47Tag('en', None, 'US', [], [], ['u-islamcal'])),
            ('zh-CN-a-myext-x-private', Bcp47Tag('zh', None, 'CN', [], [], ['a-myext'], None, [], ['x', 'private'])),
            ('en-a-myext-b-another', Bcp47Tag('en', None, None, [], [], ['a-myext', 'b-another'])),
      ]:
            with self.subTest(testTuple=testTuple):
                tag = testTuple[0]
                expected = testTuple[1]

                bcp47 = Bcp47Tag.create(tag)

                msg = "\nExpected: %s (%s)\ngot:      %s (%s)" % (expected, expected.tag, bcp47, bcp47.tag)
                self.assertEqual(expected, bcp47, msg)

    def test_SetTag(self):
        # Setup
        sut = Bcp47Tag.create('en')
        # Execute
        sut.tag = 'sr-Cyrl'
        # Verify
        self.assertEqual(Bcp47Tag('sr', 'Cyrl'), sut)

    def test_SetTag_Update(self):
        # Setup
        sut = Bcp47Tag.create('glr-Latn-LR')
        # Execute
        sut.tag = 'en'
        # Verify
        self.assertEqual(Bcp47Tag('en'), sut)

    def test_SetTag_AdjustCasing(self):
        for testTuple in [
            ('cmn-hans-cn', Bcp47Tag('cmn', 'Hans', 'CN')),
            ('CMN-HANS-CN', Bcp47Tag('cmn', 'Hans', 'CN'))
        ]:
            with self.subTest(testTuple=testTuple):
                tag = testTuple[0]
                expected = testTuple[1]
                sut = Bcp47Tag(tag)

                # Execute
                sut.tag = tag

                # Verify
                msg = "\nExpected: %s (%s)\ngot:      %s (%s)" % (expected, expected.tag, sut, sut.tag)
                self.assertEqual(expected, sut, msg)
