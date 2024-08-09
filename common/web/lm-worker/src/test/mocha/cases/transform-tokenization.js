import { assert } from 'chai';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';

import { tokenize } from '@keymanapp/models-templates';
import { tokenizeTransform } from '#./correction/transform-tokenization.js';

const defaultTokenize = (context) => tokenize(defaultBreaker, context);

describe('tokenizeTransform', () => {
  describe('with default wordbreaking', () => {
    it('properly handles simple token-edit transform', () => {
      const context = {
        left: 'an apple a date',
        right: ''
      };

      const editTransform = {
        insert: 'y',
        deleteLeft: 2
      };

      const result = tokenizeTransform(
        defaultTokenize,
        context,
        editTransform
      );

      assert.equal(result.length, 1);
      assert.deepEqual(result[0], editTransform);
    });

    it('properly handles simple token-replacing transform', () => {
      const context = {
        left: 'an apple a date',
        right: ''
      };

      const editTransform = {
        insert: 'week',
        deleteLeft: 4
      };

      const result = tokenizeTransform(
        defaultTokenize,
        context,
        editTransform
      );

      assert.equal(result.length, 1);
      assert.deepEqual(result[0], editTransform);
    });

    it('handles simple token-replacing transform with cross-token deleteLeft', () => {
      const context = {
        left: 'an apple a date',
        right: ''
      };

      // 'an apple any'
      const editTransform = {
        insert: 'ny',
        deleteLeft: 6
      };

      const result = tokenizeTransform(
        defaultTokenize,
        context,
        editTransform
      );

      assert.equal(result.length, 1);
      assert.deepEqual(result[0], editTransform);
    });

    it('properly handles a simple appended whitespace', () => {
      const context = {
        left: 'an apple a day',
        right: ''
      };

      const editTransform = {
        insert: ' ',
        deleteLeft: 0
      };

      const result = tokenizeTransform(
        defaultTokenize,
        context,
        editTransform
      );

      assert.equal(result.length, 2);
      assert.deepEqual(result, [
        // The whitespace belongs on the whitespace token that will be added.
        editTransform,
        // The default-breaker adds an empty token after whitespace, hence this
        // empty transform.
        { insert: '', deleteLeft: 0 }
      ]);
    });

    it('properly handles a simple appended period', () => {
      const context = {
        left: 'an apple a day',
        right: ''
      };

      const editTransform = {
        insert: '.',
        deleteLeft: 0
      };

      const result = tokenizeTransform(
        defaultTokenize,
        context,
        editTransform
      );

      // The default wordbreaker does not (currently) append a blank token
      // after standard English punctuation.
      assert.equal(result.length, 1);
      assert.deepEqual(result, [
        editTransform
      ]);
    });

    it('handles word-breakable transforms (case 1)', () => {
      const context = {
        left: 'an apple a dat',
        right: ''
      };

      const editTransform = {
        insert: 'y k',
        deleteLeft: 1
      };

      const result = tokenizeTransform(
        defaultTokenize,
        context,
        editTransform
      );

      assert.equal(result.length, 3);
      assert.deepEqual(result, [
        { insert: 'y', deleteLeft: 1 },
        { insert: ' ', deleteLeft: 0 },
        { insert: 'k', deleteLeft: 0 }
      ]);
    });

    it('handles word-breakable transforms (case 2)', () => {
      const context = {
        left: 'an apple a dat',
        right: ''
      };

      const editTransform = {
        insert: 'y. ',
        deleteLeft: 1
      };

      const result = tokenizeTransform(
        defaultTokenize,
        context,
        editTransform
      );

      assert.equal(result.length, 4);
      assert.deepEqual(result, [
        { insert: 'y', deleteLeft: 1 },
        { insert: '.', deleteLeft: 0 },
        { insert: ' ', deleteLeft: 0 },
        { insert: '', deleteLeft: 0 }
      ]);
    });

    it('handles complex breakable cases', () => {
      const context = {
        left: 'an apple a date',
        right: ''
      };

      // 'an apple any'
      const editTransform = {
        insert: 'ny day',
        deleteLeft: 6
      };

      const result = tokenizeTransform(
        defaultTokenize,
        context,
        editTransform
      );

      assert.equal(result.length, 3);
      assert.deepEqual(result, [
        { insert: 'ny', deleteLeft: 6 },
        { insert: ' ', deleteLeft: 0 },
        { insert: 'day', deleteLeft: 0 }
      ]);
    });
  });

  describe('with mocked dictionary-based wordbreaking', () => {
    function mockedTokenization(entries) {
      return {
        left: entries.map((text) => {
          return {text: text}
        })
      };
    }

    it('properly handles simple token-edit transform', () => {
      const context = {
        left: 'anappleadate',
        right: ''
      };

      const editTransform = {
        insert: 'y',
        deleteLeft: 2
      };

      const result = tokenizeTransform(
        () => mockedTokenization(['an', 'apple', 'a', 'day']),
        context,
        editTransform
      );

      assert.equal(result.length, 1);
      assert.deepEqual(result[0], editTransform);
    });

    it('properly handles simple token-replacing transform', () => {
      const context = {
        left: 'anappleadate',
        right: ''
      };

      const editTransform = {
        insert: 'week',
        deleteLeft: 4
      };

      const result = tokenizeTransform(
        () => mockedTokenization(['an', 'apple', 'a', 'week']),
        context,
        editTransform
      );

      assert.equal(result.length, 1);
      assert.deepEqual(result[0], editTransform);
    });

    it('handles simple token-replacing transform with cross-token deleteLeft', () => {
      const context = {
        left: 'anappleadate',
        right: ''
      };

      // 'an apple any'
      const editTransform = {
        insert: 'ny',
        deleteLeft: 5
      };

      const result = tokenizeTransform(
        () => mockedTokenization(['an', 'apple', 'any']),
        context,
        editTransform
      );

      assert.equal(result.length, 1);
      assert.deepEqual(result[0], editTransform);
    });

    it('handles word-breakable transforms (case 1)', () => {
      const context = {
        left: 'anappleadat',
        right: ''
      };

      const editTransform = {
        insert: 'yk',
        deleteLeft: 1
      };

      const result = tokenizeTransform(
        () => mockedTokenization(['an', 'apple', 'any', 'k']),
        context,
        editTransform
      );

      assert.equal(result.length, 2);
      assert.deepEqual(result, [
        { insert: 'y', deleteLeft: 1 },
        { insert: 'k', deleteLeft: 0 }
      ]);
    });

    it('handles word-breakable transforms (case 2)', () => {
      const context = {
        left: 'anappleadat',
        right: ''
      };

      const editTransform = {
        insert: 'y.',
        deleteLeft: 1
      };

      const result = tokenizeTransform(
        () => mockedTokenization(['an', 'apple', 'any', 'day', '.']),
        context,
        editTransform
      );

      assert.equal(result.length, 2);
      assert.deepEqual(result, [
        { insert: 'y', deleteLeft: 1 },
        { insert: '.', deleteLeft: 0 }
      ]);
    });

    it('handles word-breakable transforms (case 2 alternate output)', () => {
      const context = {
        left: 'anappleadat',
        right: ''
      };

      const editTransform = {
        insert: 'y.',
        deleteLeft: 1
      };

      const result = tokenizeTransform(
        () => mockedTokenization(['an', 'apple', 'any', 'day', '.', '']),
        context,
        editTransform
      );

      assert.equal(result.length, 3);
      assert.deepEqual(result, [
        { insert: 'y', deleteLeft: 1 },
        { insert: '.', deleteLeft: 0 },
        { insert: '', deleteLeft: 0 }
      ]);
    });

    it('handles complex breakable cases', () => {
      const context = {
        left: 'anappleadate',
        right: ''
      };

      // 'an apple any'
      const editTransform = {
        insert: 'nyday',
        deleteLeft: 5
      };

      const result = tokenizeTransform(
        () => mockedTokenization(['an', 'apple', 'any', 'day']),
        context,
        editTransform
      );

      assert.equal(result.length, 2);
      assert.deepEqual(result, [
        { insert: 'ny', deleteLeft: 5 },
        { insert: 'day', deleteLeft: 0 }
      ]);
    });
  });
});