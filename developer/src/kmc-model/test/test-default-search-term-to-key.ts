
import 'mocha';
import {assert} from 'chai';
import { defaultSearchTermToKey,
         defaultCasedSearchTermToKey,
         defaultApplyCasing } from '../src/model-defaults.js';
import { CasingForm, CasingFunction } from '@keymanapp/common-types';


describe('The default searchTermToKey() function', function () {
  describe('languageUsesCasing: false', function() {
    const testCases: [string, string][] = [
      // "İstanbul" has a U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE.
      // Without any casing operations, only the I should be altered.
      ['İstanbul', 'Istanbul'],

      // Similarly...
      ['DİYARBAKIR', 'DIYARBAKIR'],

      // "skýlos" is Greek for dog 🇬🇷🐶
      // starts with an 's' and ends with an 's'
      // which are DIFFERENT CHARACTERS in lowercased Greek!
      ['σκύλος', 'σκυλος'],
      ['ΣΚΥΛΟΣ', 'ΣΚΥΛΟΣ'],

      // full-width romaji is compatibility-canonical with ASCII characters:
      ['ａｅｓｔｈｅｔｉｃ', 'aesthetic'],

      // U+212B ANGSTROM SIGN (Å)
      // U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE (Å)
      // and should both normalize to 'A'
      ['\u212B', 'A'],
      ['\u00C5', 'A'],

      // We should not fall for U+037E GREEK QUESTION MARK's trolling:
      ['\u037e', ';'],

      // Test presentational forms of Arabic:
      // U+FE8D ARABIC LETTER ALEF ISOLATED FORM -> U+0627 ARABIC LETTER ALEF
      // U+FEDF ARABIC LETTER LAM INITIAL FORM -> U+0644 ARABIC LETTER LAM
      // U+FED8 ARABIC LETTER QAF MEDIAL FORM -> U+0642 ARABIC LETTER QAF
      // U+FEEC ARABIC LETTER HEH MEDIAL FORM -> U+0647 ARABIC LETTER HEH
      // U+FEEE ARABIC LETTER WAW FINAL FORM -> U+0648 ARABIC LETTER WAW
      // U+FE93 ARABIC LETTER TEH MARBUTA ISOLATED FORM -> U+0629 ARABIC LETTER TEH MARBUTA
      ['\uFE8D\uFEDF\uFED8\uFEEC\uFEEE\uFE93', '\u0627\u0644\u0642\u0647\u0648\u0629'],

      // Combine both NFKD **AND** knocking off diacritics:
      // U+01C4 LATIN CAPITAL LETTER DZ WITH CARON (Ǆ) -> <U+0064, U+007A> (dz)
      ['Ǆ', 'DZ'],
    ];

    for (let [input, expected] of testCases) {
      it(`should normalize '${input}' to '${expected}'`, function() {
        assert.equal(defaultSearchTermToKey(input), expected);
      });
    }
  });

  describe('languageUsesCasing:  true (custom applyCasing, inverts lower- & upper- casing)', function() {
    const testCases: [string, string][] = [
      // The DEFAULT function is NOT responsible for understanding the Turkish
      // distinction between U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE and
      // 'I' U+0048 LATIN CAPITAL LETTER I.
      // For Turkic languages, the recommendation is thus to make a
      // custom searchTermToKey function.
      ['İstanbul', 'ISTANBUL'],
      ['DİYARBAKIR', 'DIYARBAKIR'],

      // "skýlos" is Greek for dog 🇬🇷🐶
      // starts with an 's' and ends with an 's'
      // which are DIFFERENT CHARACTERS in lowercased Greek!
      ['σκύλος', 'ΣΚΥΛΟΣ'],
      ['σκυλοσ', 'ΣΚΥΛΟΣ'],

      // full-width romaji is compatibility-canonical with ASCII characters:
      ['ａｅｓｔｈｅｔｉｃ', 'AESTHETIC'],

      // U+212B ANGSTROM SIGN (Å)
      // U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE (Å)
      // and should both normalize to 'a'
      ['\u212B', 'A'],
      ['\u00C5', 'A'],

      // We should not fall for U+037E GREEK QUESTION MARK's trolling:
      ['\u037e', ';'],

      // Test presentational forms of Arabic:
      // U+FE8D ARABIC LETTER ALEF ISOLATED FORM -> U+0627 ARABIC LETTER ALEF
      // U+FEDF ARABIC LETTER LAM INITIAL FORM -> U+0644 ARABIC LETTER LAM
      // U+FED8 ARABIC LETTER QAF MEDIAL FORM -> U+0642 ARABIC LETTER QAF
      // U+FEEC ARABIC LETTER HEH MEDIAL FORM -> U+0647 ARABIC LETTER HEH
      // U+FEEE ARABIC LETTER WAW FINAL FORM -> U+0648 ARABIC LETTER WAW
      // U+FE93 ARABIC LETTER TEH MARBUTA ISOLATED FORM -> U+0629 ARABIC LETTER TEH MARBUTA
      ['\uFE8D\uFEDF\uFED8\uFEEC\uFEEE\uFE93', '\u0627\u0644\u0642\u0647\u0648\u0629'],

      // Combine both NFKD **AND** knocking off diacritics:
      // U+01C4 LATIN CAPITAL LETTER DZ WITH CARON (Ǆ) -> <U+0064, U+007A> (dz)
      ['Ǆ', 'DZ'],
    ];

    // While a Turkish-based test would be nice, Turkish needs custom keying,
    // as U+0130's default handling is... not ideal in Turkish.
    //
    // Instead, we can get a simple-enough test with inverted casing.
    let customCasing = function(caseToApply: CasingForm,
                                text: string,
                                defaultApplyCasing: CasingFunction): string {
      switch(caseToApply) {
        case 'lower':
            return text.toUpperCase();
        case 'upper':
            return text.toLowerCase();
        case 'initial':
            return customCasing('upper', text.charAt(0), defaultApplyCasing) + text.substr(1);
        default:
            return text;
        }
    }

    let customCasingClosure = function(caseToApply: CasingForm, text: string): string {
      return customCasing(caseToApply, text, defaultApplyCasing);
    }

    for (let [input, expected] of testCases) {
      it(`should normalize '${input}' to '${expected}'`, function() {
        assert.equal(defaultCasedSearchTermToKey(input, customCasingClosure as CasingFunction), expected);
      });
    }
  });

  describe('languageUsesCasing:  undefined (legacy 12.0 / 13.0 mode, uses `defaultApplyCasing`)', function() {
    const testCases: [string, string][] = [
      // "İstanbul" has a U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE.
      // This should be lowercased.
      ['İstanbul', 'istanbul'],

      // The DEFAULT function is NOT responsible for understanding the Turkish
      // case regarding the lowercasing of:
      // 'I' U+0048 LATIN CAPITAL LETTER I to 'ı' U+0131 LATIN SMALL LETTER DOTLESS I
      // For Turkic languages, the recommendation is to make a
      // custom searchTermToKey function:
      ['DİYARBAKIR', 'diyarbakir'],

      // "skýlos" is Greek for dog 🇬🇷🐶
      // starts with an 's' and ends with an 's'
      // which are DIFFERENT CHARACTERS in lowercased Greek!
      ['σκύλος', 'σκυλος'],
      ['ΣΚΥΛΟΣ', 'σκυλοσ'],

      // full-width romaji is compatibility-canonical with ASCII characters:
      ['ａｅｓｔｈｅｔｉｃ', 'aesthetic'],

      // U+212B ANGSTROM SIGN (Å)
      // U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE (Å)
      // and should both normalize to 'a'
      ['\u212B', 'a'],
      ['\u00C5', 'a'],

      // We should not fall for U+037E GREEK QUESTION MARK's trolling:
      ['\u037e', ';'],

      // Test presentational forms of Arabic:
      // U+FE8D ARABIC LETTER ALEF ISOLATED FORM -> U+0627 ARABIC LETTER ALEF
      // U+FEDF ARABIC LETTER LAM INITIAL FORM -> U+0644 ARABIC LETTER LAM
      // U+FED8 ARABIC LETTER QAF MEDIAL FORM -> U+0642 ARABIC LETTER QAF
      // U+FEEC ARABIC LETTER HEH MEDIAL FORM -> U+0647 ARABIC LETTER HEH
      // U+FEEE ARABIC LETTER WAW FINAL FORM -> U+0648 ARABIC LETTER WAW
      // U+FE93 ARABIC LETTER TEH MARBUTA ISOLATED FORM -> U+0629 ARABIC LETTER TEH MARBUTA
      ['\uFE8D\uFEDF\uFED8\uFEEC\uFEEE\uFE93', '\u0627\u0644\u0642\u0647\u0648\u0629'],

      // Combine both NFKD **AND** knocking off diacritics:
      // U+01C4 LATIN CAPITAL LETTER DZ WITH CARON (Ǆ) -> <U+0064, U+007A> (dz)
      ['Ǆ', 'dz'],
    ];

    for (let [input, expected] of testCases) {
      it(`should normalize '${input}' to '${expected}'`, function() {
        assert.equal(defaultCasedSearchTermToKey(input, defaultApplyCasing), expected);
      });
    }
  });
});
