import os
import shutil
import sys
import tempfile
import unittest

from keyman_config.kvk2ldml import KVKData, NFont, convert_ldml

class Kvk2LdmlTests(unittest.TestCase):
    def _createKmpJson(self, packagedir):
        kmpJsonFilename = os.path.join(packagedir, 'kmp.json')
        with open(kmpJsonFilename, 'w') as file:
            file.write('''{
                "system": {
                    "keymanDeveloperVersion": "18.0",
                    "fileVersion": "7.0"
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
                    "version": "1.5",
                    "oskFont": "KbdKhmrTest.ttf",
                    "languages": [ {
                        "name": "Central Khmer (Khmer, Cambodia)",
                        "id": "km"
                        } ]}
                ]}''')
        return kmpJsonFilename

    def test_convert_ldml__adds_keymanFacename(self):
        # Setup
        keyboardName = 'khmer_angkor'
        kvkData = KVKData()
        kvkData.AssociatedKeyboard = keyboardName
        kvkData.UnicodeFont = NFont()
        kvkData.UnicodeFont.name='DontUseThis!'

        workdir = tempfile.TemporaryDirectory()
        kmpJsonFilename = self._createKmpJson(workdir.name)
        shutil.copyfile(os.path.join(sys.path[0], '../../../common/resources/fonts/KbdKhmr.ttf'),
                        os.path.join(workdir.name, 'KbdKhmrTest.ttf'))

        # Execute
        ldml = convert_ldml(keyboardName, kvkData, kmpJsonFilename)

        # Verify
        self.assertEqual(ldml.get('locale'), "zzz-keyman")
        self.assertEqual(ldml.get('keymanFacename'), 'KbdKhmr')

        workdir.cleanup()
