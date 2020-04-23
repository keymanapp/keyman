
import 'mocha';
import {assert} from 'chai';
import { defaultSearchTermToKey } from '../dist/lexical-model-compiler/build-trie';


describe('The default searchTermToKey() function', function () {
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
    ['ΣΚΥΛΟΣ', 'σκυλος'],

    // full-width romaji is compatibility-canonical with ASCII characters:
    ['ａｅｓｔｈｅｔｉｃ', 'aesthetic']

    // TODO: test angstrom, greek semicolon, and other dumb stuff
  ];

  for (let [input, expected] of testCases) {
    it(`should normalize '${input}' to '${expected}'`, function() {
      assert.equal(defaultSearchTermToKey(input), expected);
    });
  }
});
