import unittest

from keyman_config.kvk2ldml import KVKData, NFont, convert_ldml

class Kvk2LdmlTests(unittest.TestCase):
    def test_convert_ldml__adds_keymanFacename(self):
        kvkData = KVKData()
        kvkData.AssociatedKeyboard = 'khmer_angkor'
        kvkData.UnicodeFont = NFont()
        kvkData.UnicodeFont.name='KbdKhmr'

        ldml = convert_ldml(kvkData)
        self.assertEqual(ldml.get('locale'), "zzz-keyman")
        self.assertEqual(ldml.get('keymanFacename'), 'KbdKhmr')
