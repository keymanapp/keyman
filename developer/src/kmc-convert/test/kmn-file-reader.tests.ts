/*
 * Keyman is 2025 copyright (C) SIL International. MIT License.
 *
 * Tests for KeylayoutToKmnConverter, KeylayoutFileReader, KmnFileWriter
 *
 */

import 'mocha';
import { assert } from 'chai';
import { compilerTestCallbacks, makePathToFixture } from './helpers/index.js';
import { KeylayoutFileReader } from '../src/keylayout-to-kmn/keylayout-file-reader.js';

//-----------------------------------------------------------------------------------------------------------------------

describe('KeylayoutFileReader', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe("read() ", function () {
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename_unavailable = makePathToFixture('X.keylayout');

    it('read() should return filled array on correct input', async function () {
      const inputFilename = makePathToFixture('../data/Italian.keylayout');
      const result = sut_r.read(inputFilename);
      assert.isNotEmpty(result);
    });

    it('read() should return empty array  on null input', async function () {
      const result = sut_r.read(null);
      assert.isEmpty(result);
    });

    it('read() should return empty array  on empty input', async function () {
      const result = sut_r.read("");
      assert.isEmpty(result);
    });

    it('read() should return empty array  on space as input', async function () {
      const result = sut_r.read(" ");
      assert.isEmpty(result);
    });

    it('read() should return empty array  on unavailable file name', async function () {
      const result = sut_r.read(inputFilename_unavailable);
      assert.isEmpty(result);
    });

    it('read() should return empty array  on typo in path', async function () {
      const result = sut_r.read('../data|Italian.keylayout');
      assert.isEmpty(result);
    });
  });
});
