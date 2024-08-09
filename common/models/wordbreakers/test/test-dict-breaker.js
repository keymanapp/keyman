/**
 * Smoke-test basic components of the dictionary-based wordbreaker.
 *
 * As full testing requires either an implementation or a mock of the
 * `LexiconTraversal` type, which is fairly complex, more rigorous tests
 * [TODO: will] lie within @models-templates testing.
 */

import { assert } from 'chai';
import { default as dict, splitOnWhitespace } from '@keymanapp/models-wordbreakers/obj/dict/index.js';
import { fixture as fixture1 } from './fixtures/mocked-traversals/apples-and-ale.js';

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
      })

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
  });
});