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

import { convertUtil } from '@keymanapp/common-types';
//import { convertUtil } from '@keymanapp/developer-utils';

describe('convert-utils2', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe('convertControlCharacterToUnicodeCodePoint from convert-utils2', function () {
    [
      ["U+0061", 'U+0061'],
      ["U+1234", 'U+1234'],
      ["U+1F60E", 'U+1F60E'],
      ["U+1E98", "U+1E98"],
      ["U+", undefined],
      ['U+', undefined],
      ['U+U+', undefined],
      ['U+D799', 'U+D799'],
      ['U+D800', undefined],
      ['U+D83D',undefined],
      ['U+DFFF', undefined],
      ['U+10FFFF', 'U+10FFFF'],
      ['U+110000;',undefined],
      ['U+E000', 'U+E000'],
      ['U+1000000', undefined],

      ["&#x10F601;", 'U+10F601'],
      ["&#x1F601;", 'U+1F601'],
      ["&#x9;", 'U+0009'],
      ["&#x99;", 'U+0099'],
      ["&#x999;", 'U+0999'],
      ["&#x9999;", 'U+9999'],
      ["&#x99999;", 'U+99999'],
      ["&#x0017;", 'U+0017'],
      ["&#x007;", 'U+0007'],
      ['&#xD799;', 'U+D799'],
      ['&#xD800;', undefined],
      ['&#xD83D;', undefined],
      ['&#xDFFF;',undefined],
      ['&#x110000;',undefined],
      ["&#x", undefined],
      ["&#x;", undefined],

      ["&#1111553;", 'U+10F601'],
      ["&#128513;", 'U+1F601'],
      ["&#9;", 'U+0009'],
      ["&#99;", 'U+0063'],
      ["&#999;", 'U+03E7'],
      ["&#9999;", 'U+270F'],
      ["&#99999;", 'U+1869F'],
      ["&#100000000;", undefined],
      ["&#", undefined],
      ["&#;", undefined],
      ["&#1234;56", undefined],
      ["&#007;", 'U+0007'],
      ["&#0018;", 'U+0012'],
      ["&#100000000;", undefined],
      ["&#100000000;", undefined],
      ["&#128513;", 'U+1F601'],
      ["&#128513;", 'U+1F601'],

      ['0000;', '0000;'],
      ['X;', 'X;'],
      ['123;', '123;'],
      [';', ';'],
      ["&", "&"],
      ["a", "a"],
      ["ሴ", "ሴ"],
      ['😎', '😎'],
      ["ẘ", "ẘ"],
    ].forEach(function (values) {
      it(('should convert "' + values[0] + '"').padEnd(25, " ") + 'to "' + values[1] + '"', async function () {
        const result = convertUtil.convertControlCharacterToUnicodeCodePoint(values[0] as string);
        assert.equal(result, values[1]);
      });
    });
  });

  describe('convertToUnicodeCharacter from convert-utils2', function () {
    [
      ["a", 'a'],
      ["ሴ", 'ሴ'],
      ['😎', '😎'],
      ["ẘ", "ẘ"],
      ["&#x61;", 'a'],
      ["&#x1234;", 'ሴ'],
      ["&#x1F60E;", '😎'],
      ["&#x1E98;", "ẘ"],
      ["&#97;", 'a'],
      ["&#4660;", 'ሴ'],
      ["&#128518;", '😆'],
      ["&#7832;", "ẘ"],
      ["U+0061", 'a'],
      ["U+1234", 'ሴ'],
      ["U+1F60E", '😎'],
      ["U+1E98", "ẘ"],
      ["U+", undefined],
      ['U+', undefined],
      ['U+U+', undefined],
      ['U+D799', '힙'],
      ['U+D800', undefined],
      ['U+D83D', undefined],
      ['U+DFFF', undefined],
      ['U+10FFFF', '􏿿'],
      ['U+E000', ''],
      ['U+1000000', undefined],
      ["&gt;", '>'],
      ["&commat;", undefined],
      ["&commat", undefined],
      ["ab", "ab"],
      ["abcde", "abcde"],
      ["ሴሴ", 'ሴሴ'],
      ['😎😆', '😎😆'],
      ["ẘẘ", "ẘẘ"],
      ["", ''],
      ['&', '&'],
      ['&;', '&;'],
      ['&&', '&&'],
      ['&&;', '&&;'],
      ["&#&#", undefined],
      ["&#x&#x", undefined],
      ["&#", undefined],
      ["&#;", undefined],
      ["&#x", undefined],
      ["&#x;", undefined],
      ['&##', undefined],
      ['&##;', undefined],
      ["&#x10FFFF;", "􏿿"],
      ["&#1114111;", "􏿿"],
      ["&#x110000;", undefined],
      ["&#x1000000;", undefined],
      ['&#1234;56', undefined],
      [undefined, undefined],
      [null, undefined],

    ].forEach(function (values) {
      it(('from writer should convert "' + values[0] + '"').padEnd(25, " ") + 'to "' + values[1] + '"', async function () {
        const result = convertUtil.convertToUnicodeCharacter(values[0] as string);
        assert.equal(result, values[1]);
      });
    });
  });
});
