#!/usr/bin/env python3
import os
import shutil
import sys
import tempfile
import unittest

from keyman_config.kvk2ldml import KVKData, NFont, convert_ldml

class Kvk2LdmlTests(unittest.TestCase):
    def _createKmpJson(self, packagedir, add_keyboards=True):
        kmpJsonFilename = os.path.join(packagedir, 'kmp.json')
        system = '''"system": {
            "keymanDeveloperVersion": "18.0",
            "fileVersion": "7.0"
        }'''
        files = '''"files": [ {
            "name": "khmer_angkor.kmx",
            "description": "Keyboard Khmer Angkor"
            }, {
            "name": "kmp.json",
            "description": "Package information (JSON)"
        } ]'''
        if add_keyboards:
            keyboards = '''"keyboards": [ {
                "name": "Khmer Angkor",
                "id": "khmer_angkor",
                "version": "1.5",
                "oskFont": "keymanweb-osk.ttf",
                "languages": [ {
                    "name": "Central Khmer (Khmer, Cambodia)",
                    "id": "km"
                } ]}
            ]'''
        else:
            keyboards = ''
        with open(kmpJsonFilename, 'w') as file:
            file.write(f'{{ {system}, {files}, {keyboards} }}')
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
        shutil.copy2(os.path.join(sys.path[0], '../../../common/resources/fonts/keymanweb-osk.ttf'),
                    workdir.name)

        # Execute
        ldml = convert_ldml(keyboardName, kvkData, kmpJsonFilename)

        # Verify
        self.assertEqual(ldml.get('locale'), "zzz-keyman")
        self.assertEqual(ldml.get('keymanFacename'), 'SymChar')

        workdir.cleanup()

    def test_convert_ldml__no_crash_without_keyboards(self):
        # Setup
        keyboardName = 'khmer_angkor'
        kvkData = KVKData()
        kvkData.AssociatedKeyboard = keyboardName

        workdir = tempfile.TemporaryDirectory()
        kmpJsonFilename = self._createKmpJson(workdir.name, add_keyboards=False)

        # Execute
        ldml = convert_ldml(keyboardName, kvkData, kmpJsonFilename)

        # Verify
        self.assertIsNone(ldml)

        workdir.cleanup()
