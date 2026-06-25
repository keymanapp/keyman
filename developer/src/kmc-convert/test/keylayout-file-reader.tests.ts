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
import { KL_KeyMap, KL_KeyMapSelect } from "../../common/web/utils/src/types/keylayout/keylayout-xml.js";

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

    it('validate() should return false on inputfile with unknown tags', async function () {
      const sutR = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test_unknownTags.keylayout');
      const result: Keylayout.KeylayoutXMLSourceFile | null = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
      const validated = sutR.validate(result as Keylayout.KeylayoutXMLSourceFile, inputFilename);
      assert.isFalse(validated);
    });

    it('validate() should return false on inputfile with additional tags', async function () {
      const sutR = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test_additionalTags.keylayout');
      const result: Keylayout.KeylayoutXMLSourceFile | null = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
      const validated = sutR.validate(result as Keylayout.KeylayoutXMLSourceFile, inputFilename);
      assert.isFalse(validated);
    });
    it('validate() should return false on inputfile with missing tags', async function () {
      const sutR = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test_missingTags.keylayout');
      const result: Keylayout.KeylayoutXMLSourceFile | null = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
      const validated = sutR.validate(result as Keylayout.KeylayoutXMLSourceFile, inputFilename);
      assert.isFalse(validated);
    });
    it('validate() should return false on no entries in action-when', async function () {
      const sutR = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test_noActionWhen.keylayout');
      const result: Keylayout.KeylayoutXMLSourceFile | null = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
      const validated = sutR.validate(result as Keylayout.KeylayoutXMLSourceFile, inputFilename);
      assert.isFalse(validated);
    });
    it('validate() should return false on null as input', async function () {
      const sutR = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test_noActionWhen.keylayout');
      //const result: Keylayout.KeylayoutXMLSourceFile | null = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
      const validated = sutR.validate(null, inputFilename);
      assert.isFalse(validated);
    });
    it('validate() should return false on undefined as input', async function () {
      const sutR = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test_noActionWhen.keylayout');
      //const result: Keylayout.KeylayoutXMLSourceFile | null = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
      const validated = sutR.validate(undefined, inputFilename);
      assert.isFalse(validated);
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

  describe('findMapIndexinKeymap ', function () {
    const keyMapSelect: KL_KeyMapSelect = {
      mapIndex: '',
      modifier: []
    };

    keyMapSelect.modifier.push({ keys: 'caps' });
    keyMapSelect.modifier.push({ keys: 'rightOption' });
    keyMapSelect.modifier.push({ keys: 'rightShift caps' });

    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const jsonO: Keylayout.KeylayoutXMLSourceFile | null = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
    [
      ['0', true],
      ['7', true],
      ['999', false],
      ['A', false],
      [123, false],
      ['', false],
      [null, false],
      [undefined, false],
    ].forEach(function (values) {
      it(("findMapIndexinKeymap(keyMapSelect.mapIndex = '" + values[0] + "')").padEnd(40, " ") + "should return " + "'" + values[1] + "'", async function () {
        keyMapSelect.mapIndex = values[0] as string;
        const result = sutR.findMapIndexinKeymap(jsonO as Keylayout.KeylayoutXMLSourceFile, keyMapSelect);
        assert.isTrue(result === values[1]);
      });
    });
  });

  describe('findIndexinKeymapSelect ', function () {
    const keyMap: KL_KeyMap = {
      index: '',
      key: []
    };
    keyMap.key.push({ code: '0', output: 'A' });
    keyMap.key.push({ code: '1', action: 'S' });
    keyMap.key.push({ code: '2', output: 'D' });

    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const jsonO: Keylayout.KeylayoutXMLSourceFile | null = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
    [
      ['0', true],
      ['7', true],
      ['999', false],
      ['A', false],
      [123, false],
      ['', false],
      [null, false],
      [undefined, false],
    ].forEach(function (values) {
      it(("findIndexinKeymapSelect(keyMap.index = '" + values[0] + "')").padEnd(40, " ") + "should return " + "'" + values[1] + "'", async function () {
        keyMap.index = values[0] as string;
        const result = sutR.findIndexinKeymapSelect(jsonO as Keylayout.KeylayoutXMLSourceFile, keyMap);
        assert.isTrue(result === values[1]);
      });
    });
  });

  describe('checkForCorrespondingElements ', function () {
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    [
      ['../data/Test.keylayout', true],
      ['../data/Test_moreKeymapSelectThanKeymapERROR.keylayout', false],
      ['../data/Test_moreKeyMapThanKeyMapselectERROR.keylayout', false],
      ['../data/Test_moreKeyMapThanKeyMapselectAndJisERROR.keylayout', false],
    ].forEach(function (values) {
      it(("checkForCorrespondingElements in " + values[0]).padEnd(40, " ") + " should return " + "'" + values[1] + "'", async function () {
        const jsonO: Keylayout.KeylayoutXMLSourceFile | null = sutR.read(compilerTestCallbacks.loadFile(makePathToFixture(values[0] as string)));
        const result = sutR.checkForCorrespondingElements(jsonO as Keylayout.KeylayoutXMLSourceFile);
        assert.isTrue(result === values[1]);
      });
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
