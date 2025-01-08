import 'mocha';
import { assert } from 'chai';
import { ModelDefinitions } from '../src/model-definitions.js';
import { LexicalModelSource } from '../src/lexical-model.js';
import { LexicalModelTypes } from '@keymanapp/common-types';

describe('Model definition pseudoclosures', function () {
  describe('14.0 defaults', function() {
    describe('languageUsesCasing == true', function() {
      // We don't need a complete spec for this, given the (currently) limited range of what
      // the ModelPseudoclosure covers.
      let modelSource: LexicalModelSource = {
        languageUsesCasing: true,
        sources: [],
        format: 'trie-1.0'
      };

      let pseudoclosure = new ModelDefinitions(modelSource);

      const testCases: [string, string, string][] = [
        // Note:  not written the Turkish way.  Turns out 'İ'.toLowerCase() decomposes the result,
        // which would have made for a fairly yucky test.
        ['Istanbul', 'istanbul', 'istanbul'],

        // The DEFAULT function is NOT responsible for understanding the Turkish
        // case regarding the lowercasing of:
        // 'I' U+0048 LATIN CAPITAL LETTER I to 'ı' U+0131 LATIN SMALL LETTER DOTLESS I
        // For Turkic languages, the recommendation is to make a
        // custom applyCasing function:
        ['DİYARBAKIR', 'di̇yarbakir', 'diyarbakir'], // The 'i̇' is the decomposed result alluded to for the previous case.

        // full-width romaji has corresponding lowercased versions:
        ['ＡＥＳＴＨＥＴＩＣ', 'ａｅｓｔｈｅｔｉｃ', 'aesthetic'],

        // "skýlos" is Greek for dog 🇬🇷🐶
        // starts with an 's' and ends with an 's'
        // which are DIFFERENT CHARACTERS in lowercased Greek!
        ['σκύλος', 'σκύλος', 'σκυλος'],
        ['ΣΚΥΛΟΣ', 'σκυλος', 'σκυλοσ'],  // the keyed version after lowercasing doesn't know how
                                         // to make the distinction.  Both 'Σ's have the same char-code.

        // Uncased syntax and numbers should pass through unscathed:
        ['1234.?!', '1234.?!', '1234.?!'],
        ['”', '”', '"'],
        ["‘", "‘", "'"]
      ];

      for (let [input, cased, keyed] of testCases) {
        it(`should case '${input}' as '${cased}'`, function() {
          assert.equal(pseudoclosure.applyCasing('lower', input), cased);
        });

        it(`should key  '${input}' as '${keyed}'`, function() {
          assert.equal(pseudoclosure.searchTermToKey(input), keyed);
        });
      }
    });

    describe('languageUsesCasing == false', function() {
      // We don't need a complete spec for this, given the (currently) limited range of what
      // the ModelPseudoclosure covers.
      let modelSource: LexicalModelSource = {
        languageUsesCasing: false,
        sources: [],
        format: 'trie-1.0'
      };

      let pseudoclosure = new ModelDefinitions(modelSource);

      const testCases: [string, string][] = [
        // Note:  not written the Turkish way.  Turns out 'İ'.toLowerCase() decomposes the result,
        // which would have made for a fairly yucky test.
        ['Istanbul', 'Istanbul'],

        ['DİYARBAKIR', 'DIYARBAKIR'],

        // full-width romaji has corresponding capitalized versions:
        ['ＡＥＳＴＨＥＴＩＣ', 'AESTHETIC'],

        // "skýlos" is Greek for dog 🇬🇷🐶
        // starts with an 's' and ends with an 's'
        // which are DIFFERENT CHARACTERS in lowercased Greek!
        ['σκύλος', 'σκυλος'],
        ['ΣΚΥΛΟΣ', 'ΣΚΥΛΟΣ'],  // the keyed version after lowercasing doesn't know how
                               // to make the distinction.  Both 'Σ's have the same char-code.

        // Uncased syntax and numbers should pass through unscathed:
        ['1234.?!', '1234.?!'],

        ['”', '"'],
        ["‘", "'"]
      ];

      for (let [input, keyed] of testCases) {
        it(`should key  '${input}' as '${keyed}'`, function() {
          assert.equal(pseudoclosure.searchTermToKey(input), keyed);
        });
      }
    });
  });
  describe('Pre 14.0 defaults (languageUsesCasing == undefined)', function() {
    // We don't need a complete spec for this, given the (currently) limited range of what
    // the ModelPseudoclosure covers.
    let modelSource: LexicalModelSource = {
      sources: [],
      format: 'trie-1.0'
    };

    let pseudoclosure = new ModelDefinitions(modelSource);

    const testCases: [string, string][] = [
      // Note:  not written the Turkish way.  Turns out 'İ'.toLowerCase() decomposes the result,
      // which would have made for a fairly yucky test.
      ['Istanbul', 'istanbul'],

      ['DİYARBAKIR', 'diyarbakir'],

      // full-width romaji has corresponding capitalized versions:
      ['ＡＥＳＴＨＥＴＩＣ', 'aesthetic'],

      // "skýlos" is Greek for dog 🇬🇷🐶
      // starts with an 's' and ends with an 's'
      // which are DIFFERENT CHARACTERS in lowercased Greek!
      ['σκύλος', 'σκυλος'],
      ['ΣΚΥΛΟΣ', 'σκυλοσ'],  // the keyed version after lowercasing doesn't know how
                             // to make the distinction.  Both 'Σ's have the same char-code.

      // Uncased syntax and numbers should pass through unscathed:
      ['1234.?!', '1234.?!']
    ];

    for (let [input, keyed] of testCases) {
      it(`should key  '${input}' as '${keyed}'`, function() {
        assert.equal(pseudoclosure.searchTermToKey(input), keyed);
      });
    }
  });

  describe('Model-defined applyCasing + (dependent) searchTermToKey', function() {
    // Note:  this test only implements enough Turkish-related stuff to facilitate
    // a functional test.  Not guaranteed to be sufficient for actual Turkish use.
    let turkishCasing = function(form: LexicalModelTypes.CasingForm, text: string, defaultApplyCasing: (form: LexicalModelTypes.CasingForm, text: string) => string): string {
      switch(form) {
      case 'lower':
        return defaultApplyCasing(form, text
            .replace(/I/g, 'ı')
            .replace(/İ/g, 'i'));
      case 'upper':
        return defaultApplyCasing(form, text
            .replace(/ı/g, 'I')
            .replace(/i/g, 'İ'));
      case 'initial':
        return turkishCasing('upper', text.charAt(0), defaultApplyCasing) + text.substr(1);
      default:
        return text;
      }
    }

    let modelSource: LexicalModelSource = {
      languageUsesCasing: true,
      applyCasing: turkishCasing,
      searchTermToKey: function(wordform: string, applyCasing: LexicalModelTypes.CasingFunction): string {
        return Array.from(wordform
          .normalize('NFC')  // Mostly to avoid decomposing 'İ'
        ) // end of `Array.from`
        .map(function(c) { return applyCasing('lower', c)})  // Will use custom `applyCasing` definition!
        .join('');
      },
      sources: [],
      format: 'trie-1.0'
    };

    let pseudoclosure = new ModelDefinitions(modelSource);

    const testCases: [string, string, string][] = [
      ['İstanbul', 'istanbul', 'istanbul'],

      // The DEFAULT function is NOT responsible for understanding the Turkish
      // case regarding the lowercasing of:
      // 'I' U+0048 LATIN CAPITAL LETTER I to 'ı' U+0131 LATIN SMALL LETTER DOTLESS I
      // For Turkic languages, the recommendation is to make a
      // custom applyCasing function:
      ['DİYARBAKIR', 'diyarbakır', 'diyarbakır'],

      // Uncased syntax and numbers should pass through unscathed:
      ['1234.?!', '1234.?!', '1234.?!']
    ];

    for (let [input, cased, keyed] of testCases) {
      it(`should case '${input}' as '${cased}'`, function() {
        assert.equal(pseudoclosure.applyCasing('lower', input), cased);
      });

      it(`should key  '${input}' as '${keyed}'`, function() {
        assert.equal(pseudoclosure.searchTermToKey(input), keyed);
      });
    }
  });
});
