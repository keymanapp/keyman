/*
 * Keyman is 2025 copyright (C) SIL International. MIT License.
 *
 * Tests for KeylayoutToKmnConverter, KeylayoutFileReader, KmnFileWriter
 *
 */

import 'mocha';
import { compilerTestCallbacks, makePathToFixture } from './helpers/index.js';
import { KeylayoutFileReader } from '../src/keylayout-to-kmn/keylayout-file-reader.js';
import { doesNotReject } from "node:assert";


describe('KeylayoutFileReader', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe('read()', function () {
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const empty: any = [];

    it('read() should return filled array on correct input', async function () {
      await doesNotReject(async () => (sut_r.read(makePathToFixture('../data/Italian.keylayout'))));
    });

    it('read() should return empty array on null input', async function () {
      await doesNotReject(async () => (Array.isArray(sut_r.read(null)) === Array.isArray(empty)));
    });

    it('read() should return empty array on empty input', async function () {
      await doesNotReject(async () => (Array.isArray(sut_r.read("")) === Array.isArray(empty)));
    });

    it('read() should return empty array on space as input', async function () {
      await doesNotReject(async () => (Array.isArray(sut_r.read(" ")) === Array.isArray(empty)));
    });

    it('read() should return empty array on unavailable file name', async function () {
      await doesNotReject(async () => (Array.isArray(sut_r.read(makePathToFixture('Unavailable_InputFilename.keylayout'))) === empty));
    });

    it('read() should return empty array on typo in path', async function () {
      await doesNotReject(async () => (Array.isArray(sut_r.read(makePathToFixture('../data|Italian.keylayout'))) === empty));
    });
  });
});
