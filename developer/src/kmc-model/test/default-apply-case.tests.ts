
import 'mocha';
import { assert } from 'chai';
import { defaultApplyCasing } from '../src/model-defaults.js';

describe('The default applyCasing() function', function () {
  // // --------
  // // Definitions toward SMP testing.
  // // Ref: https://unicode.org/charts/nameslist/n_1D400.html
  // // Useful for tests related to strings with supplementary pairs.
  // let u = function(code: number): string {
  //   var H = Math.floor((code - 0x10000) / 0x400) + 0xD800;
  //   var L = (code - 0x10000) % 0x400 + 0xDC00;

  //   return String.fromCharCode(H, L);
  // }

  // Are there any known default-handled SMP cases?
  // If not... we could make the defaultApplyCase function simpler by not worrying about SMP.
  //
  // let smp_a = u(0x1d5ba);  // MATHEMATICAL SANS-SERIF SMALL A
  // let smp_p = u(0x1d5c9);
  // let smp_l = u(0x1d5c5);
  // let smp_e = u(0x1d5be);

  // let smp_A = u(0x1d5a0);  // MATHEMATICAL SANS_SERIF CAPITAL A
  // let smp_P = u(0x1d5af);
  // let smp_L = u(0x1d5ab);
  // let smp_E = u(0x1d5a4);

  // // Unfortunately... the default JS .toUpperCase() implementation doesn't actually
  // // map the 'SMALL' versions to the 'CAPITAL' versions.
  // // ---------

  describe('case:  \'lower\'', function() {
    const testCases: [string, string][] = [
      // Note:  not written the Turkish way.  Turns out 'İ'.toLowerCase() decomposes the result,
      // which would have made for a fairly yucky test.
      ['Istanbul', 'istanbul'],

      // The DEFAULT function is NOT responsible for understanding the Turkish
      // case regarding the lowercasing of:
      // 'I' U+0048 LATIN CAPITAL LETTER I to 'ı' U+0131 LATIN SMALL LETTER DOTLESS I
      // For Turkic languages, the recommendation is to make a
      // custom applyCasing function:
      ['DİYARBAKIR', 'di̇yarbakir'], // The 'i̇' is the decomposed result alluded to for the previous case.

      // full-width romaji has corresponding capitalized versions:
      ['ＡＥＳＴＨＥＴＩＣ', 'ａｅｓｔｈｅｔｉｃ'],

      // "skýlos" is Greek for dog 🇬🇷🐶
      // starts with an 's' and ends with an 's'
      // which are DIFFERENT CHARACTERS in lowercased Greek!
      ['ΣΚΥΛΟΣ', 'σκυλος'],

      // Uncased syntax and numbers should pass through unscathed:
      ['1234.?!', '1234.?!']
    ];

    for (let [input, expected] of testCases) {
      it(`should lowercase '${input}' as '${expected}'`, function() {
        assert.equal(defaultApplyCasing('lower', input), expected);
      });
    }
  });

  describe('case:  \'upper\'', function() {
    const testCases: [string, string][] = [
      // Inverse of the corresponding 'lower' test.
      ['istanbul', 'ISTANBUL'],

      // The DEFAULT function is NOT responsible for understanding the Turkish
      // case regarding the uppercasing of:
      // 'ı' U+0131 LATIN SMALL LETTER DOTLESS I to 'I' U+0048 LATIN CAPITAL LETTER I
      // For Turkic languages, the recommendation is to make a
      // custom applyCasing function:
      ['diyarbakır', 'DIYARBAKIR'], // The 'i̇' is the decomposed result alluded to for the previous case.

      // full-width romaji has corresponding capitalized versions:
      ['ａｅｓｔｈｅｔｉｃ', 'ＡＥＳＴＨＥＴＩＣ'],

      // "skýlos" is Greek for dog 🇬🇷🐶
      // starts with an 's' and ends with an 's'
      // which are DIFFERENT CHARACTERS in lowercased Greek!
      ['σκυλος', 'ΣΚΥΛΟΣ'],

      // Uncased syntax and numbers should pass through unscathed:
      ['1234.?!', '1234.?!']
    ];

    for (let [input, expected] of testCases) {
      it(`should uppercase '${input}' as '${expected}'`, function() {
        assert.equal(defaultApplyCasing('upper', input), expected);
      });
    }
  });

  describe('case:  \'initial\'', function() {
    const testCases: [string, string][] = [
      // Inverse of the corresponding 'lower' test.
      ['istanbul', 'Istanbul'],

      // The DEFAULT function is NOT responsible for understanding the Turkish
      // case regarding the uppercasing of:
      // 'ı' U+0131 LATIN SMALL LETTER DOTLESS I to 'I' U+0048 LATIN CAPITAL LETTER I
      // For Turkic languages, the recommendation is to make a
      // custom applyCasing function:
      ['diyarbakır', 'Diyarbakır'], // The 'i̇' is the decomposed result alluded to for the previous case.

      // full-width romaji has corresponding capitalized versions:
      ['ａｅｓｔｈｅｔｉｃ', 'Ａｅｓｔｈｅｔｉｃ'],

      // "skýlos" is Greek for dog 🇬🇷🐶
      // starts with an 's' and ends with an 's'
      // which are DIFFERENT CHARACTERS in lowercased Greek!
      ['σκυλος', 'Σκυλος'],

      // Uncased syntax and numbers should pass through unscathed:
      ['1234.?!', '1234.?!']
    ];

    for (let [input, expected] of testCases) {
      it(`should initial-case '${input}' as '${expected}'`, function() {
        assert.equal(defaultApplyCasing('initial', input), expected);
      });
    }
  });
});