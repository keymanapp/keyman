/*
 * Unit tests for common utility functions/methods.
 */

import { assert } from 'chai';
import * as models from "@keymanapp/models-templates";
import * as wordBreakers from "@keymanapp/models-wordbreakers";

describe('Tokenization functions', function() {
  describe('tokenize', function() {
    it('tokenizes English using defaults, pre-whitespace caret', function() {
      let context = {
        left: "The quick brown fox",
        right: " jumped over the lazy dog",
        startOfBuffer: true,
        endOfBuffer: true
      };

      let tokenization = models.tokenize(wordBreakers.default, context);

      let expectedResult = {
        left: ['The', 'quick', 'brown', 'fox'],
        right: ['jumped', 'over', 'the', 'lazy', 'dog'],
        caretSplitsToken: false
      };

      assert.deepEqual(tokenization, expectedResult);
    });

    it('tokenizes English using defaults, pre-whitespace caret, partial context', function() {
      let context = {
        left: "quick brown fox",        // No "The"
        right: " jumped over the lazy", // No "dog"
        startOfBuffer: false,
        endOfBuffer: false
      };

      let tokenization = models.tokenize(wordBreakers.default, context);

      let expectedResult = {
        left: ['quick', 'brown', 'fox'],
        right: ['jumped', 'over', 'the', 'lazy'],
        caretSplitsToken: false
      };

      assert.deepEqual(tokenization, expectedResult);
    });

    it('tokenizes English using defaults, post-whitespace caret', function() {
      let context = {
        left: "The quick brown fox ",
        right: "jumped over the lazy dog",
        startOfBuffer: true,
        endOfBuffer: true
      };

      let tokenization = models.tokenize(wordBreakers.default, context);

      // Technically, we're editing the start of the first token on the right
      // when in this context.
      let expectedResult = {
        left: ['The', 'quick', 'brown', 'fox', ''],
        right: ['jumped', 'over', 'the', 'lazy', 'dog'],
        caretSplitsToken: true
      };

      assert.deepEqual(tokenization, expectedResult);
    });

    it('tokenizes English using defaults, post-whitespace caret, partial context', function() {
      let context = {
        left: "quick brown fox ",
        right: "jumped over the lazy",
        startOfBuffer: false,
        endOfBuffer: false
      };

      let tokenization = models.tokenize(wordBreakers.default, context);

      // Technically, we're editing the start of the first token on the right
      // when in this context.
      let expectedResult = {
        left: ['quick', 'brown', 'fox', ''],
        right: ['jumped', 'over', 'the', 'lazy'],
        caretSplitsToken: true
      };

      assert.deepEqual(tokenization, expectedResult);
    });

    it('tokenizes English using defaults, splitting caret, complete context (start, end == true)', function() {
      let context = {
        left: "The quick brown fox jum",
        right: "ped over the lazy dog",
        startOfBuffer: true,
        endOfBuffer: true
      };

      let tokenization = models.tokenize(wordBreakers.default, context);

      let expectedResult = {
        left: ['The', 'quick', 'brown', 'fox', 'jum'],
        right: ['ped', 'over', 'the', 'lazy', 'dog'],
        caretSplitsToken: true
      };

      assert.deepEqual(tokenization, expectedResult);
    });

    it('tokenizes English using defaults, splitting caret, incomplete context (start, end == false)', function() {
      let context = {
        left: "The quick brown fox jum",
        right: "ped over the lazy dog",
        startOfBuffer: false,
        endOfBuffer: false
      };

      let tokenization = models.tokenize(wordBreakers.default, context);

      let expectedResult = {
        left: ['The', 'quick', 'brown', 'fox', 'jum'],
        right: ['ped', 'over', 'the', 'lazy', 'dog'],
        caretSplitsToken: true
      };

      assert.deepEqual(tokenization, expectedResult);
    });

    it('empty context case', function() {
      // Wordbreaking on a empty space => no word.
      let context = {
        left: '', startOfBuffer: true,
        right: '', endOfBuffer: true
      };

      let tokenization = models.tokenize(wordBreakers.default, context);

      let expectedResult = {
        left: [],
        right: [],
        caretSplitsToken: false
      };

      assert.deepEqual(tokenization, expectedResult);
    });

    it('nil context case', function() {
      // Wordbreaking on a empty space => no word.
      let tokenization = models.tokenize(wordBreakers.default, null);

      let expectedResult = {
        left: [],
        right: [],
        caretSplitsToken: false
      };

      assert.deepEqual(tokenization, expectedResult);
    });

    it('near-empty context:  one space before caret', function() {
      // Wordbreaking on a empty space => no word.
      let context = {
        left: ' ', startOfBuffer: true,
        right: '', endOfBuffer: true
      };

      let tokenization = models.tokenize(wordBreakers.default, context);

      let expectedResult = {
        left: [''],
        right: [],
        caretSplitsToken: false
      };

      assert.deepEqual(tokenization, expectedResult);
    });

    // For the next few tests:  a mocked wordbreaker for Khmer, a language
    // without whitespace between words.
    let mockedKhmerBreaker = function(text) {
      // Step 1:  Build constants for spans that a real wordbreaker would return.
      let srok = { // Khmer romanization of 'ស្រុក'
        text: 'ស្រុក',
        start: 0,
        end: 5, // ្រ = ្ + រ
        length: 5
      };

      let sro = { // Khmer romanization of 'ស្រុ'
        text: 'ស្រុ',
        start: 0,
        end: 4,
        length: 4
      };

      let k = { // Not the proper Khmer romanization; 'k' used here for easier readability.
        text: 'ក',
        start: 0,
        end: 1,
        length: 1
      };

      let khmer = { // Khmer romanization of 'ខ្មែរ'
        text: 'ខ្មែរ',
        start: 0,
        end: 5, //  ្ម = ្ + ម
        length: 5
      }

      // Step 2: Allow shifting a defined 'constant' span without mutating the definition.
      let shiftSpan = function(span, delta) {
        // Avoid mutating the parameter!
        let shiftedSpan = {
          text: span.text,
          start: span.start + delta,
          end: span.end + delta,
          length: span.length
        };

        return shiftedSpan;
      }

      // Step 3: Define return values for the cases we expect to need mocking.
      switch(text) {
        case 'ស្រុ':
          return [sro];
        case 'ក':
          return [k];
        case 'ស្រុក':
          return [srok];
        case 'ខ្មែរ':
          return [khmer];
        case 'ស្រុកខ្មែរ':
          return [srok, shiftSpan(khmer, srok.length)]; // array of the two.
        case 'កខ្មែរ':
          // I'd admittedly be at least somewhat surprised if a real wordbreaker got this
          // and similar situations perfectly right... but at least it gives us what
          // we need for a test.
          return [k, shiftSpan(khmer, k.length)];
        default:
          throw "Dummying error - no return value specified for \"" + text + "\"!";
      }
    }

    it('tokenizes Khmer using mocked wordbreaker, caret between words', function() {
      // The two words:
      // - ស្រុក - 'land'
      // - ខ្មែរ - 'Khmer'
      // Translation:  Cambodia (informal), lit:  "Khmer land" / "land of [the] Khmer"

      let context = {
        left: "ស្រុក",
        right: "ខ្មែរ",
        startOfBuffer: true,
        endOfBuffer: true
      };

      let tokenization = models.tokenize(mockedKhmerBreaker, context);

      let expectedResult = {
        left: ['ស្រុក'],
        right: ['ខ្មែរ'],
        caretSplitsToken: false
      };

      assert.deepEqual(tokenization, expectedResult);
    });

    it('tokenizes Khmer using mocked wordbreaker, caret within word', function() {
      // The two words:
      // - ស្រុក - 'land'
      // - ខ្មែរ - 'Khmer'
      // Translation:  Cambodia (informal), lit:  "Khmer land" / "land of [the] Khmer"

      let context = {
        left: "ស្រុ",
        right: "កខ្មែរ",
        startOfBuffer: true,
        endOfBuffer: true
      };

      let tokenization = models.tokenize(mockedKhmerBreaker, context);

      let expectedResult = {
        left: ['ស្រុ'],
        right: ['ក', 'ខ្មែរ'],
        caretSplitsToken: true
      };

      assert.deepEqual(tokenization, expectedResult);
    });

    let midLetterNonbreaker = (text) => {
      let customization = {
        rules: [{
          match: (context) => {
            if(context.propertyMatch(null, ["ALetter"], ["MidLetter"], ["eot"])) {
              return true;
            } else {
              return false;
            }
          },
          breakIfMatch: false
        }],
        propertyMapping: (char) => {
          let hyphens = ['\u002d', '\u2010', '\u058a', '\u30a0'];
          if(hyphens.includes(char)) {
              return "MidLetter";
          } else {
            return null;
          }
        }
      };

      return wordBreakers.default(text, customization);
    }

    it('treats caret as `eot` for pre-caret text', function() {
      let context = {
        left: "don-",  // We use a hyphen here b/c single-quote is hardcoded.
        right: " worry",
        endOfBuffer: true,
        startOfBuffer: true
      };

      let tokenization = models.tokenize(wordBreakers.default, context);

      assert.deepEqual(tokenization, {
        left: ["don", "-"],
        right: ["worry"],
        caretSplitsToken: false
      });

      tokenization = models.tokenize(midLetterNonbreaker, context);

      assert.deepEqual(tokenization, {
        left: ["don-"],
        right: ["worry"],
        caretSplitsToken: false
      });
    });

    it('handles mid-contraction tokenization', function() {
      let context = {
        left: "don:",
        right: "t worry",
        endOfBuffer: true,
        startOfBuffer: true
      };

      let tokenization = models.tokenize(wordBreakers.default, context);

      assert.deepEqual(tokenization, {
        left: ["don", ":"],    // This particular case feels like a possible issue.
        right: ["t", "worry"], // It'd be a three-way split token, as "don:t" would
                               // be a single token were it not for the caret in the middle.
        caretSplitsToken: false
      })

      tokenization = models.tokenize(midLetterNonbreaker, context);

      assert.deepEqual(tokenization, {
        left: ["don:"],
        right: ["t", "worry"],
        caretSplitsToken: true
      });
    });
  });

  describe('getLastPreCaretToken', function() {
    it('with pre-whitespace caret', function() {
      let context = {
        left: "The quick brown fox",
        right: " jumped over the lazy dog",
        startOfBuffer: true,
        endOfBuffer: true
      };

      let tokenization = models.getLastPreCaretToken(wordBreakers.default, context);

      assert.equal(tokenization, 'fox');
    });

    it('with post-whitespace caret', function() {
      let context = {
        left: "The quick brown fox ",
        right: "jumped over the lazy dog",
        startOfBuffer: true,
        endOfBuffer: true
      };

      let tokenization = models.getLastPreCaretToken(wordBreakers.default, context);

      assert.equal(tokenization, '');
    });

    it('within a token', function() {
      let context = {
        left: "The quick brown fox jum",
        right: "ped over the lazy dog",
        startOfBuffer: true,
        endOfBuffer: true
      };

      let tokenization = models.getLastPreCaretToken(wordBreakers.default, context);

      assert.equal(tokenization, 'jum');
    });
  });

  describe('wordbreak', function() {
    it('with pre-whitespace caret', function() {
      let context = {
        left: "The quick brown fox",
        right: " jumped over the lazy dog",
        startOfBuffer: true,
        endOfBuffer: true
      };

      let tokenization = models.wordbreak(wordBreakers.default, context);

      assert.equal(tokenization, 'fox');
    });

    it('with post-whitespace caret', function() {
      let context = {
        left: "The quick brown fox ",
        right: "jumped over the lazy dog",
        startOfBuffer: true,
        endOfBuffer: true
      };

      let tokenization = models.wordbreak(wordBreakers.default, context);

      assert.equal(tokenization, '');
    });

    // This version is subject to change.  In the future, we may wish the wordbreak
    // operation to include "the rest of the word" - the post-caret part.
    it('within a token', function() {
      let context = {
        left: "The quick brown fox jum",
        right: "ped over the lazy dog",
        startOfBuffer: true,
        endOfBuffer: true
      };

      let tokenization = models.wordbreak(wordBreakers.default, context);

      assert.equal(tokenization, 'jum');
    });
  });
});