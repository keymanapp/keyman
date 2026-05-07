/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Tests for KeylayoutToKmnConverter, XkbFileReader, KmnFileWriter
 *
 */

import 'mocha';
import { assert } from 'chai';
import { compilerTestCallbacks, makePathToFixture } from './helpers/index.js';
import { XkbFileReader } from '../src/xkb-to-kmn/xkb-file-reader.js';

describe('XkbFileReader', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe("Xkb-kmn:: read() ", function () {
    const sutR = new XkbFileReader(compilerTestCallbacks);

    it('read() should return filled array on correct input', async function () {
      const inputFilename = makePathToFixture('../data/Test.xkb');
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
      const inputFilenameUnavailable = makePathToFixture('../data/X.xkb');
      const result = sutR.read(compilerTestCallbacks.loadFile(inputFilenameUnavailable));
      assert.isNull(result);
    });

    it('read() should return empty array on typo in path', async function () {
      const result = sutR.read(compilerTestCallbacks.loadFile(makePathToFixture('../data|Test.xkb')));
      assert.isNull(result);
    });
  });

});
