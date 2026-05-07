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
import { compilerTestCallbacks, makePathToFixture } from './helpers/index.js';
import { KeylayoutFileReader } from '../src//kmc-convert-read/keylayout-file-reader.js';

describe('KeylayoutFileReader', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe("read() ", function () {
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);

    it('read() should return filled array on correct input', async function () {
      const inputFilename = makePathToFixture('../data/tests-keylayout-kmn/Test.keylayout');
      const result = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
      assert.isNotEmpty(result);
    });

    it('read() should return empty array on empty input', async function () {
      const result = sutR.read(compilerTestCallbacks.loadFile(""));
      assert.isNull(result);
    });

    it('read() should return empty array on space as input', async function () {
      const result = sutR.read(compilerTestCallbacks.loadFile(" "));
      assert.isNull(result);
    });

    it('read() should return empty array on unavailable file name', async function () {
      const inputFilenameUnavailable = makePathToFixture('../data/tests-keylayout-kmn/X.keylayout');
      const result = sutR.read(compilerTestCallbacks.loadFile(inputFilenameUnavailable));
      assert.isNull(result);
    });

    it('read() should return empty array on typo in path', async function () {
      const result = sutR.read(compilerTestCallbacks.loadFile(makePathToFixture('../data|Test.keylayout')));
      assert.isNull(result);
    });
  });

});
