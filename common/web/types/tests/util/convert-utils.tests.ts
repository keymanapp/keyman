import 'mocha';
import { assert } from 'chai';
import { convertUtil } from '@keymanapp/common-types';

describe('convert-utils', function () {

  describe('convertToUnicodeCharacter', function () {
    [
      ["a", 'a'],
      ["ሴ", 'ሴ'],
      ["W̊", "W̊"],
      ['😎', '😎'],
      ["ab", 'ab'],
      ["ሴЖ", 'ሴЖ'],
      ["ẘẈ", "ẘẈ"],
      ["😎😆", '😎😆'],
      ["aሴ😆", 'aሴ😆'],
      ["U+0061", 'a'],
      ["U+1234", 'ሴ'],
      ["U+1E9A", "ẚ"],
      ["U+1F60A", '😊'],
      ["U+0001", '\u0001'],
      ["U+1000000;", undefined],
      ["&#x61;", 'a'],
      ["&#x1234;", 'ሴ'],
      ["&#x1E98;", "ẘ"],
      ["&#x1F60F;", '😏'],
      ["&#x0002;", '\u0002'],
      ["&#x1000000;", undefined],
      ["&#97;", 'a'],
      ["&#4660;", 'ሴ'],
      ["&#7835;", "ẛ"],
      ["&#128518;", '😆'],
      ["&#0003;", '\u0003'],
      ["&#1000000;", '󴉀'],
      ["&commat;", undefined],
      ["&gt;", '>'],
      ["&lt;", '<'],
      ["&quot;", '"'],
      ["&apos;", "'"],
      ["&gt", undefined],
      ["␤", '␤'],
      ["␕", '␕'],
      ["", ''],
      ["", ''],
      [undefined, undefined],
      [null, undefined],
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

    ].forEach(function (values) {
      it(('should convert "' + values[0] + '"').padEnd(30, " ") + 'to "' + values[1] + '"', async function () {
        const result = convertUtil.convertToUnicodeCharacter(values[0] as string);
        assert.equal(result, values[1]);
      });
    });
  });
});
