/**
 * Smoke-test basic components of the dictionary-based wordbreaker.
 *
 * As full testing requires either an implementation or a mock of the
 * `LexiconTraversal` type, which is fairly complex; this motivates
 * a unit-testing dependency on @keymanapp/models-templates as a result.
 */

import { assert } from 'chai';
import { default as dict, splitOnWhitespace } from '@keymanapp/models-wordbreakers/obj/dict/index.js';
import { fixture as fixture1 } from './fixtures/mocked-traversals/apples-and-ale.js';

import path from 'path';

import { createRequire } from "module";
import { fileURLToPath } from 'url';

import { TrieModel } from '@keymanapp/models-templates';

/**
 * Load JSON fixtures from a well-known place.
 */
function jsonFixture(name) {
  // The most straight-forward way... is to use CommonJS-style require to load JSON.
  // Fortunately, Node provides the tools needed to recreate it.
  const require = createRequire(import.meta.url);

  // ES-module mode also leaves out `__dirname`, so we rebuild that too.
  const __filename = fileURLToPath(import.meta.url);
  const __dirname = path.dirname(__filename);
  return require(path.join(__dirname, 'fixtures', `${name}.json`));
}

function assertSpanSplit(text, actualSplitSpans, expectedSplitStr) {
  assert.deepEqual(actualSplitSpans.map((entry) => entry.text), expectedSplitStr);

  let index = 0;
  for(let i=0; i < expectedSplitStr.length; i++) {
    const start = text.indexOf(expectedSplitStr[i], index);

    assert.equal(actualSplitSpans[i].start, start);
    assert.equal(actualSplitSpans[i].end, start + expectedSplitStr[i].length);
    assert.equal(actualSplitSpans[i].length, expectedSplitStr[i].length);

    index = start + actualSplitSpans[i].length;
  }
}

describe('dictionary-based wordbreaker', () => {
  describe('helpers', () => {
    describe('splits on existing whitespace and ZWNJ chars', () => {
      it('handles no-split cases', () => {
        const text = "supercalifragilisticexpialidocious";
        const expectedSplit = [text];

        const actualSplit = splitOnWhitespace(text);
        assertSpanSplit(text, actualSplit, expectedSplit);
      });

      it('handles simple-space cases', () => {
        const text = "this is a test";
        const expectedSplit = ['this', 'is', 'a', 'test'];

        const actualSplit = splitOnWhitespace(text);
        assertSpanSplit(text, actualSplit, expectedSplit);
      });

      it('properly indexes text with non-BMP chars', () => {
        let smpA = String.fromCodePoint(0x1d5ba);
        let smpP = String.fromCodePoint(0x1d5c9);
        let smpL = String.fromCodePoint(0x1d5c5);
        let smpE = String.fromCodePoint(0x1d5be);

        const apple = [smpA, 'p', smpP, 'l', smpE].join('');
        const ale = ['a', smpL, smpE].join('');

        const text = `${apple} and ${ale}`;
        const expectedSplit = [apple, 'and', ale];

        const actualSplit = splitOnWhitespace(text);
        assert.deepEqual(actualSplit.map((entry) => entry.text), expectedSplit);

        let index = 0;
        for(let i=0; i < expectedSplit.length; i++) {
          const start = text.indexOf(expectedSplit[i], index);

          assert.equal(actualSplit[i].start, start);
          // Note:  we're using code unit measurements here.  This is to
          // uphold Span's specification.
          assert.equal(actualSplit[i].end, start + expectedSplit[i].length);
          assert.equal(actualSplit[i].length, expectedSplit[i].length);

          index = start + actualSplit[i].length;
        }
      });

      it('handles text with ZWNJs and mixed-whitespace sequences', () => {
        const text = "this\tis   \t\n \t  a\n more\u200cintense\u200ctest.";
        const expectedSplit = ['this', 'is', 'a', 'more', 'intense', 'test.'];

        const actualSplit = splitOnWhitespace(text);
        assertSpanSplit(text, actualSplit, expectedSplit);
      });
    });
  });

  describe('main breaker', () => {
    describe('simple cases with a toy, mocked Traversal setup', () => {
      it('handles empty context', () => {
        const text = "";
        const expectedSplit = [];

        const actualSplit = dict(text, fixture1);
        assertSpanSplit(text, actualSplit, expectedSplit);
      });

      it('single section, all words in dict', () => {
        const text = "appleandanale"
        const expectedSplit = ['apple', 'and', 'an', 'ale'];

        const actualSplit = dict(text, fixture1);
        assertSpanSplit(text, actualSplit, expectedSplit);
      });

      it('three sections, all words in dict', () => {
        const text = "applyanapple appandale anyaleapp";
        const expectedSplit = ['apply', 'an', 'apple', 'app', 'and', 'ale', 'any', 'ale', 'app'];

        const actualSplit = dict(text, fixture1);
        assertSpanSplit(text, actualSplit, expectedSplit);
      });

      it('two sections, unexpected words', () => {
        const text = "bobsapple applesforale";
        const expectedSplit = [
          // As all words in this dict start with 'a', any other words get split into individual letters.
          'b', 'o', 'b', 's',
          'apple',
          'apple',
          // 'apples' does not exist while 'apple' does - the 's' gets split off because of this.
          's',
          // As all words in this dict start with 'a', any other words get split into individual letters.
          'f',
          'o',
          'r',
          'ale'
        ];

        const actualSplit = dict(text, fixture1);
        assertSpanSplit(text, actualSplit, expectedSplit);
      });
    });

    describe('breaking with sil.km.gcc 1.0', () => {
      const fixture = jsonFixture('tries/sil.km.gcc - 1.0');

      // Uses the original lexical-model's backing data, but swaps in the
      // dictionary-based wordbreaker.
      const model = new TrieModel(fixture);
      // Note:  to initialize it as needed for the model at this time... we
      // need to be able to build this object _before_ the `Trie` is constructed!
      //
      // A "hurdle to cross" / we'll "have to cross that bridge when we come to it."
      const root = model.traverseFromRoot();

      it('text with out-of-dict name and number', () => {
        const text = 'ខ្ញុំឃ្មោះយុសវេអាយុ៣៨ឆ្នាំហើយ';
        const expectedResults = [
          'ខ្ញុំ',
          'ឃ្មោះ',
          // So, my name (as transliterated into Khmer) doesn't have anything even CLOSE.
          // Each letter ended up spun off as its own thing.
          "យ",
          "ុ",
          "ស",
          "វ",
          "េ",
          // And back to normal (for a bit)
          'អាយុ',
          // Numbers aren't words in the dictionary.
          '៣',
          '៨',
          // And back to normal.
          'ឆ្នាំ',
          'ហើយ'
        ];

        const actualResults = dict(text, root);
        assertSpanSplit(text, actualResults, expectedResults);
      });
    });
  });
});