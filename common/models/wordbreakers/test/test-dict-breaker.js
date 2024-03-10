/**
 * Smoke-test basic components of the dictionary-based wordbreaker.
 *
 * As full testing requires either an implementation or a mock of the
 * `LexiconTraversal` type, which is fairly complex, more rigorous tests
 * [TODO: will] lie within @models-templates testing.
 */

import { assert } from 'chai';
import { splitOnCodepoints, splitOnWhitespace } from '@keymanapp/models-wordbreakers/obj/dict/index.js';

describe('dictionary-based wordbreaker', () => {
  describe('helpers', () => {
    describe('splitOnCodepoints', () => {
      it('handles BMP text properly', () => {
        const text = 'apple';
        const expectedSplit = text.split('');

        const actualSplit = splitOnCodepoints(text);
        assert.deepEqual(actualSplit, expectedSplit);
      });

      it('handles non-BMP text properly', () => {
        let smpA = String.fromCodePoint(0x1d5ba);
        let smpP = String.fromCodePoint(0x1d5c9);
        let smpE = String.fromCodePoint(0x1d5be);

        const expectedSplit = [smpA, 'p', smpP, 'l', smpE];
        const text = expectedSplit.join('');

        const actualSplit = splitOnCodepoints(text);
        assert.deepEqual(actualSplit, expectedSplit);
      });
    });

    describe('splits on existing whitespace and ZWNJ chars', () => {
      it('handles no-split cases', () => {
        const text = "supercalifragilisticexpialidocious";
        const expectedSplit = [text];

        const actualSplit = splitOnWhitespace(text);
        assert.deepEqual(actualSplit.map((entry) => entry.text), expectedSplit);

        let index = 0;
        for(let i=0; i < expectedSplit.length; i++) {
          const start = text.indexOf(expectedSplit[i], index);

          assert.equal(actualSplit[i].start, start);
          assert.equal(actualSplit[i].end, start + expectedSplit[i].length);
          assert.equal(actualSplit[i].length, expectedSplit[i].length);

          index = start + actualSplit[i].length;
        }
      })

      it('handles simple-space cases', () => {
        const text = "this is a test";
        const expectedSplit = ['this', 'is', 'a', 'test'];

        const actualSplit = splitOnWhitespace(text);
        assert.deepEqual(actualSplit.map((entry) => entry.text), expectedSplit);

        let index = 0;
        for(let i=0; i < expectedSplit.length; i++) {
          const start = text.indexOf(expectedSplit[i], index);

          assert.equal(actualSplit[i].start, start);
          assert.equal(actualSplit[i].end, start + expectedSplit[i].length);
          assert.equal(actualSplit[i].length, expectedSplit[i].length);

          index = start + actualSplit[i].length;
        }
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
        assert.deepEqual(actualSplit.map((entry) => entry.text), expectedSplit);

        let index = 0;
        for(let i=0; i < expectedSplit.length; i++) {
          const start = text.indexOf(expectedSplit[i], index);

          assert.equal(actualSplit[i].start, start);
          assert.equal(actualSplit[i].end, start + expectedSplit[i].length);
          assert.equal(actualSplit[i].length, expectedSplit[i].length);

          index = start + actualSplit[i].length;
        }
      });
    });
  });
});