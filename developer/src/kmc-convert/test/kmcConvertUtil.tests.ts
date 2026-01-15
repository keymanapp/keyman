/*
 * Keyman is 2025 copyright (C) SIL International. MIT License.
 *
 * Created by S. Schmitt on 2025-08-21
 *
 * Tests for kmcConvertutil
 *
 */

import 'mocha';
import { assert } from 'chai';
import { compilerTestCallbacks } from './helpers/index.js';
import { kmcConvertutil } from '@keymanapp/common-types';

describe('kmcConvertutil', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe('convertToUnicodeCodePoint ', function () {
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
        const result = kmcConvertutil.convertToUnicodeCodePoint(values[0] as string);
        assert.equal(result, values[1]);
      });
    });
  });

  describe('convertToUnicodeCharacter ', function () {
    [
      ["&#x61;", 'a'],
      ["&#x1234;", 'ሴ'],
      ["&#x1F60E;", '😎'],
      ["&#x1000000;", undefined],
      ["&#97;", 'a'],
      ["&#4660;", 'ሴ'],
      ["&#128518;", '😆'],
      ["&#1000000;", undefined],
      ["U+0061", 'a'],
      ["U+1234", 'ሴ'],
      ["U+1F60E", '😎'],
      ["U+1000000", undefined],      
      ["a", 'a'],
      ["ሴ", 'ሴ'],
      ['😎', '😎'],
      ["W̊", "W̊"],
      ["&", "&"],
      ["&gt;", ">"],
      ["&lt;", "<"],
      ["&amp;", "&"],
      ["&apos;", "'"],
      ["&quot;", '"'],
      ["&commat;", undefined],
      ["ab", undefined],
      ["Uu", undefined],
      ["U+", undefined],
      ["U+1", undefined],
      ["", ''],
      [undefined, undefined],
      [null, undefined]
    ].forEach(function (values) {
      it(('from utils should convert "' + values[0] + '"').padEnd(25, " ") + 'to "' + values[1] + '"', async function () {
        const result = kmcConvertutil.convertToUnicodeCharacter(values[0] as string);
        assert.equal(result, values[1]);
      });
    });
  });

});
