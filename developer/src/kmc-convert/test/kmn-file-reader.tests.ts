/*
 * Keyman is 2025 copyright (C) SIL International. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Tests for KeylayoutToKmnConverter, KeylayoutFileReader, KmnFileWriter
 *
 */

import 'mocha';
import { assert } from 'chai';
import { compilerTestCallbacks, makePathToFixture } from './helpers/index.js';
import { KeylayoutFileReader } from '../src/keylayout-to-kmn/keylayout-file-reader.js';

describe('KeylayoutFileReader', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe("read() ", function () {

    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);

    it('read() should return filled array on correct input', async function () {
      const inputFilename = makePathToFixture('../' + 'data' + '/Test.keylayout');
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
      const inputFilename_unavailable = makePathToFixture('../' + 'data' + '/X.keylayout');
      const result = sut_r.read(inputFilename_unavailable);
      assert.isNull(result);
    });

    it('read() should return empty array on typo in path', async function () {
      const result = sut_r.read(makePathToFixture('../' + 'data' + '|Test.keylayout'));
      assert.isNull(result);
    });
  });
});
