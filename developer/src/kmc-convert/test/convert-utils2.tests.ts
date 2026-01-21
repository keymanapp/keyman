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
import { compilerTestCallbacks } from './helpers/index.js';

import { convertUtil } from '@keymanapp/common-types'
//import { convertUtil } from '@keymanapp/developer-utils';

describe('convert-utils2', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe('convertToUnicodeCodePoint from convert-utils2', function () {
    [
      ["&#x10F601;", 'U+10F601'],
      ["&#x1F601;", 'U+1F601'],
      ["&#x9;", 'U+0009'],
      ["&#x99;", 'U+0099'],
      ["&#x999;", 'U+0999'],
      ["&#x9999;", 'U+9999'],
      ["&#x99999;", 'U+99999'],
      ["&#1111553;", 'U+10F601'],
      ["&#128513;", 'U+1F601'],
      ["&#9;", 'U+0009'],
      ["&#99;", 'U+0063'],
      ["&#999;", 'U+03E7'],
      ["&#9999;", 'U+270F'],
      ["&#99999;", 'U+1869F'],
      ['0000;', '0000;'],
      ['X;', 'X;'],
      ['123;', '123;'],
      [';', ';'],
      [' ;', ' ;']
    ].forEach(function (values) {
      it(('should convert "' + values[0] + '"').padEnd(25, " ") + 'to "' + values[1] + '"', async function () {
        const result = convertUtil.convertToUnicodeCodePoint(values[0] as string);
        assert.equal(result, values[1]);
      });
    });
  });

  describe('convertToUnicodeCharacter from convert-utils2', function () {
    [
      ["&#1000000;", undefined],
      ["a", 'a'],
      ["ሴ", 'ሴ'],
      ['😎', '😎'],
      ["ẘ", "ẘ"],
      ["U+1E98", "ẘ"],
      ["", ''],
      ["&#x61;", 'a'],
      ["&#x1234;", 'ሴ'],
      ["&#x1F60E;", '😎'],
      ["&#x1000000;", undefined],
      ["&#97;", 'a'],
      ["&#4660;", 'ሴ'],
      ["&#128518;", '😆'],
      ["U+0061", 'a'],
      ["U+1234", 'ሴ'],
      ["U+1F60E", '😎'],
      ["U+1000000;", undefined],
      ["&commat;", undefined],
      ["ab", undefined],
      [undefined, undefined],
      [null, undefined]
    ].forEach(function (values) {
      it(('from writer should convert "' + values[0] + '"').padEnd(25, " ") + 'to "' + values[1] + '"', async function () {
        const result = convertUtil.convertToUnicodeCharacter(values[0] as string);
        assert.equal(result, values[1]);
      });
    });
  });
});
