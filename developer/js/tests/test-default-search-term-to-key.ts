
import 'mocha';
import {assert} from 'chai';
import { defaultSearchTermToKey } from '../dist/lexical-model-compiler/build-trie';


describe('The default searchTermToKey() function', function () {
  it('should lowercase and THEN normalize', function() {
    // "İstanbul" has a U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE.
    // This should be lowercased.
    assert.equal(defaultSearchTermToKey('İstanbul'), 'istanbul');
    // The DEFAULT function is NOT responsible for understanding the Turkish
    // case regarding the lowercasing of:
    // 'I' U+0048 LATIN CAPITAL LETTER I to 'ı' U+0131 LATIN SMALL LETTER DOTLESS I
    // For Turkic languages, the recommendation is to make a
    // custom searchTermToKey function:
    assert.equal(defaultSearchTermToKey('DİYARBAKIR'), 'diyarbakir');

    // "skýlos" is Greek for dog 🇬🇷🐶
    // starts with an 's' and ends with an 's'
    // which are DIFFERENT CHARACTERS in lowercased Greek!
    assert.equal(defaultSearchTermToKey('σκύλος'), 'σκυλος');
    assert.equal(defaultSearchTermToKey('ΣΚΥΛΟΣ'), 'σκυλος');
  });
});