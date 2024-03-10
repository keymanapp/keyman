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

      it.skip('handles non-BMP text properly', () => {

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

      it.skip('properly indexes text with non-BMP chars', () => {

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