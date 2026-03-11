import 'mocha';
import { assert } from 'chai';
import { convertUtil } from '@keymanapp/common-types';

describe('convert-utils', function () {

  describe('convertToUnicodeCharacter from convert-utils', function () {
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
      it(('should convert "' + values[0] + '"').padEnd(25, " ") + 'to "' + values[1] + '"', async function () {
        const result = convertUtil.convertToUnicodeCharacter(values[0] as string);
        assert.equal(result, values[1]);
      });
    });
  });


});
