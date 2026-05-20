/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Tests for KeylayoutFileReader
 *
 */

import 'mocha';
import { assert } from 'chai';
import { Keylayout } from "@keymanapp/developer-utils";
import { compilerTestCallbacks, makePathToFixture } from './helpers/index.js';
import { KeylayoutFileReader } from '../src/keylayout-to-kmn/keylayout-file-reader.js';

describe('KeylayoutFileReader', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe("validate() ", function () {
    it('validate() should return true on correct inputfile', async function () {
      const sutR = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      const result: Keylayout.KeylayoutXMLSourceFile | null = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
      const validated = sutR.validate(result as Keylayout.KeylayoutXMLSourceFile, inputFilename);
      assert.isTrue(validated);
    });
  });

  describe('validate() should return false on inputfiles with errors ', function () {
    const sut = new KeylayoutFileReader(compilerTestCallbacks);
    [
      ['../data/Test_moreKeymapSelectThanKeymapERROR.keylayout'],
      ['../data/Test_moreKeyMapThanKeyMapselectERROR.keylayout'],
      ['../data/Test_moreKeyMapThanKeyMapselectAndJisERROR.keylayout'],
      ['../data/Test_MissingkeyERROR.keylayout'],
      ['../data/Test_MissingkeyMapERROR.keylayout'],
      ['../data/Test_MissingLayoutsERROR.keylayout'],
      ['../data/Test_MissingmodifierMapERROR.keylayout'],
      ['../data/Test_MissingkeyMapSetERROR.keylayout'],
      ['../data/Test_MissingActionsERROR.keylayout'],
      ['../data/Test_MissingTerminatorsERROR.keylayout'],
      ['../data/Test_MissingAllERROR.keylayout'],
      ['../data/Test_unknownTags.keylayout'],
      ['../data/Test_additionalTags.keylayout'],
      ['../data/Test_missingTags.keylayout'],
    ].forEach(function (files) {
      it(files + " should not be valid ", async function () {
        const result: Keylayout.KeylayoutXMLSourceFile | null = sut.read(compilerTestCallbacks.loadFile(makePathToFixture(files[0])));
        const validated = sut.validate(result as Keylayout.KeylayoutXMLSourceFile, makePathToFixture(files[0]));
        assert.isFalse(validated);
      });
    });
  });

  describe("read() ", function () {
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);

    it('read() should return filled array on correct input', async function () {
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      const result = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
      assert.isNotEmpty(result);
    });

    it('read() should return null on empty input', async function () {
      const result = sutR.read(compilerTestCallbacks.loadFile(""));
      assert.isNull(result);
    });

    it('read() should return null on space as input', async function () {
      const result = sutR.read(compilerTestCallbacks.loadFile(" "));
      assert.isNull(result);
    });

    it('read() should return null on unavailable file name', async function () {
      const inputFilenameUnavailable = makePathToFixture('../data/X.keylayout');
      const result = sutR.read(compilerTestCallbacks.loadFile(inputFilenameUnavailable));
      assert.isNull(result);
    });

    it('read() should return null on typo in path', async function () {
      const result = sutR.read(compilerTestCallbacks.loadFile(makePathToFixture('../data|Test.keylayout')));
      assert.isNull(result);
    });
  });

  describe("read() check structure of returned JSON", function () {

    it('read() should have the correct JSON structure', async function () {

      const inputFilename = makePathToFixture('../data/Test.keylayout');
      const sutR = new KeylayoutFileReader(compilerTestCallbacks);
      const binaryData = compilerTestCallbacks.loadFile(inputFilename);
      const result: Keylayout.KeylayoutXMLSourceFile = sutR.read(binaryData);

      assert.isNotNull(result);
      assert.isTrue(result.keyboard !== null);

      for (let i = 0; i < result.keyboard.layouts.length; i++) {
        assert.isTrue(result.keyboard.layouts[i].layout.length > 0);
      }

      assert.isTrue(result.keyboard.keyMapSet.length >= 1);
      for (let i = 0; i < result.keyboard.keyMapSet.length; i++) {
        assert.isTrue(result.keyboard.keyMapSet[i].keyMap.length > 0);
        for (let j = 0; j < result.keyboard.keyMapSet[i].keyMap.length; j++) {
          assert.isTrue(result.keyboard.keyMapSet[i].keyMap[j].key.length > 0);
          for (let k = 0; k < result.keyboard.keyMapSet[i].keyMap[j].key.length; k++) {
            assert.isTrue(result.keyboard.keyMapSet[i].keyMap[j].key[k]['action'] !== null
              || result.keyboard.keyMapSet[i].keyMap[j].key[k]['output'] !== null);
          }
        }
      }

      assert.isTrue(result.keyboard.modifierMap.length >= 1);
      for (let i = 0; i < result.keyboard.modifierMap.length; i++) {
        assert.isTrue(result.keyboard.modifierMap[i].keyMapSelect.length > 0);
        for (let j = 0; j < result.keyboard.modifierMap[i].keyMapSelect.length; j++) {
          assert.isTrue(result.keyboard.modifierMap[i].keyMapSelect[j].modifier.length > 0);
        }
      }
      for (let i = 0; i < result.keyboard.modifierMap.length; i++) {
        assert.isTrue(result.keyboard.modifierMap[i].keyMapSelect.length > 0);
        for (let j = 0; j < result.keyboard.keyMapSet.length; j++) {
          assert.isTrue(result.keyboard.keyMapSet[j].keyMap.length
            === result.keyboard.modifierMap[i].keyMapSelect.length);
        }
      }
    });
  });

});
