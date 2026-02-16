/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Tests for KeylayoutToKmnConverter, KeylayoutFileReader, KmnFileWriter
 *
 */

import 'mocha';
import { assert } from 'chai';
import { KeylayoutXMLSourceFile } from "@keymanapp/developer-utils"
import { compilerTestCallbacks, makePathToFixture } from './helpers/index.js';
import { KeylayoutFileReader } from '../src/keylayout-to-kmn/keylayout-file-reader.js';

describe('KeylayoutFileReader', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe("validate() ", function () {

    it('validate() should return true on correct inputfile', async function () {
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      const result: KeylayoutXMLSourceFile = sut_r.read(inputFilename);
      const validated = sut_r.validate(result);
      assert.isTrue(validated);
    });

    it('validate() should return false on inputfile with unknown tags', async function () {
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test_unknownTags.keylayout');
      const result: KeylayoutXMLSourceFile = sut_r.read(inputFilename);
      const validated = sut_r.validate(result);
      assert.isFalse(validated);
    });

    it('validate() should return false on inputfile with additional tags', async function () {
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test_additionalTags.keylayout');
      const result: KeylayoutXMLSourceFile = sut_r.read(inputFilename);
      const validated = sut_r.validate(result);
      assert.isFalse(validated);
    });
    it('validate() should return false on inputfile with missing tags', async function () {
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test_missingTags.keylayout');
      const result: KeylayoutXMLSourceFile = sut_r.read(inputFilename);
      const validated = sut_r.validate(result);
      assert.isFalse(validated);
    });
    it('validate() should return false on no entries in action-when', async function () {
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
      const inputFilename = makePathToFixture('../data/Test_noActionWhen.keylayout');
      const result: KeylayoutXMLSourceFile = sut_r.read(inputFilename);
      const validated = sut_r.validate(result);
      assert.isFalse(validated);
    });
  });

  describe("read() ", function () {

    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);

    it('read() should return filled array on correct input', async function () {
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      const result = sut_r.read(inputFilename);
      assert.isNotEmpty(result);
    });

    it('read() should return empty array on null input', async function () {
      const result = sut_r.read(null);
      assert.isNull(result);
    });

    it('read() should return empty array on empty input', async function () {
      const result = sut_r.read("");
      assert.isNull(result);
    });

    it('read() should return empty array on space as input', async function () {
      const result = sut_r.read(" ");
      assert.isNull(result);
    });

    it('read() should return empty array on unavailable file name', async function () {
      const inputFilename_unavailable = makePathToFixture('../data/X.keylayout');
      const result = sut_r.read(inputFilename_unavailable);
      assert.isNull(result);
    });

    it('read() should return empty array on typo in path', async function () {
      const result = sut_r.read(makePathToFixture('../data|Test.keylayout'));
      assert.isNull(result);
    });
  });
});
