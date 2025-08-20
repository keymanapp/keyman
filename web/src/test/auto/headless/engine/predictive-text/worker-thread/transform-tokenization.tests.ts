import { assert } from 'chai';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { Token, Tokenization, tokenize } from '@keymanapp/models-templates';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { tokenizeTransform } from '@keymanapp/lm-worker/test-index';

import Context = LexicalModelTypes.Context;
import Transform = LexicalModelTypes.Transform;

const defaultTokenize = (context: Context) => tokenize(defaultBreaker, context);

describe('tokenizeTransform', () => {
  describe('with default wordbreaking', () => {
    it('produces a single empty transform at index 0 when an empty transform is input', () => {
      const context: Context = {
        left: 'an apple a date',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const editTransform = {
        insert: '',
        deleteLeft: 0
      };

      const result = tokenizeTransform(
        defaultTokenize,
        context,
        editTransform
      );

      assert.equal(result.size, 1);
      assert.deepEqual(result.get(0), editTransform);
    });

    it('properly handles simple token-edit transform', () => {
      const context: Context = {
        left: 'an apple a date',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
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

      assert.equal(result.size, 1);
      assert.deepEqual(result.get(0), editTransform);
    });

    it('properly handles simple token-replacing transform', () => {
      const context = {
        left: 'an apple a date',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
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

      assert.equal(result.size, 1);
      assert.deepEqual(result.get(0), editTransform);
    });

    it('handles simple token-replacing transform with cross-token deleteLeft', () => {
      const context = {
        left: 'an apple a date',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      // 'an apple any'
      const editTransform = {
        insert: 'ny',
        deleteLeft: 5
      };

      const result = tokenizeTransform(
        defaultTokenize,
        context,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(-2, {
        insert: 'ny',
        deleteLeft: 0
      });
      expectedMap.set(-1, {
        insert: '',
        deleteLeft: 1
      });
      expectedMap.set(0, {
        insert: '',
        deleteLeft: 4
      });

      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });

    it('properly handles a simple appended whitespace', () => {
      const context = {
        left: 'an apple a day',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
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

      const expectedMap = new Map<number, Transform>();
      // The whitespace belongs on the whitespace token that will be added.
      expectedMap.set(1, editTransform);
      // The default-breaker adds an empty token after whitespace, hence this
      // empty transform.
      expectedMap.set(2, { insert: '', deleteLeft: 0 });

      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });

    it('properly handles a simple appended period', () => {
      const context = {
        left: 'an apple a day',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
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
      const expectedMap = new Map<number, Transform>();
      expectedMap.set(1, editTransform);
      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });

    it('properly deletes a simple appended whitespace', () => {
      const context = {
        left: 'an apple a day ',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const editTransform = {
        insert: '',
        deleteLeft: 1
      };

      const result = tokenizeTransform(
        defaultTokenize,
        context,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      // The whitespace belongs on the whitespace token that will be added.
      expectedMap.set(-1, editTransform);
      // The default-breaker adds an empty token after whitespace, hence this
      // empty transform.
      expectedMap.set(0, { insert: '', deleteLeft: 0 });

      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });

    it('handles word-breakable transforms (case 1)', () => {
      const context = {
        left: 'an apple a dat',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
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

      const expectedMap = new Map<number, Transform>();
      // dat => day
      expectedMap.set(0, { insert: 'y', deleteLeft: 1 });
      // new whitespace
      expectedMap.set(1, { insert: ' ', deleteLeft: 0 });
      // new 'k' token
      expectedMap.set(2, { insert: 'k', deleteLeft: 0 });
      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });

    it('handles word-breakable transforms (case 2)', () => {
      const context = {
        left: 'an apple a dat',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
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

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(0, { insert: 'y', deleteLeft: 1 });
      expectedMap.set(1, { insert: '.', deleteLeft: 0 });
      expectedMap.set(2, { insert: ' ', deleteLeft: 0 });
      expectedMap.set(3, { insert: '', deleteLeft: 0 });
      assert.equal(result.size, 4);
      assert.deepEqual(result, expectedMap);
    });

    it('handles complex breakable cases', () => {
      const context = {
        left: 'an apple as date',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
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

      const expectedMap = new Map<number, Transform>();
      // as => any
      expectedMap.set(-2, { insert: 'ny', deleteLeft: 1 }); // 2 back from the last token before the text insertion point.
      // ' ' replaced with another ' ' (but still edited)
      expectedMap.set(-1, { insert: ' ', deleteLeft: 1 });
      // date => day, but with full replacement due to the large deleteLeft.
      expectedMap.set( 0, { insert: 'day', deleteLeft: 4 }); // The original token before the text insertion point.
      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });
  });

  describe('with mocked dictionary-based wordbreaking', () => {
    function mockedTokenization(map: Map<string, string[]>) {
      return (context: Context) => {
        let tokens = map.get(context.left);
        if(!tokens) {
          assert.fail("Mocked tokenization was not properly constructed");
        }
        return {
          left: tokens.map((text) => {
            return {text: text} as Token
          })
        } as Tokenization;
      }
    }

    it('properly handles simple token-edit transform', () => {
      const context = {
        left: 'anappleadate',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const editTransform = {
        insert: 'y',
        deleteLeft: 2
      };

      const mockMap = new Map<string, string[]>();
      mockMap.set('anappleadate', ['an', 'apple', 'a', 'date']);
      mockMap.set('anappleaday', ['an', 'apple', 'a', 'day']);
      const result = tokenizeTransform(
        mockedTokenization(mockMap),
        context,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(0, editTransform); // The original token before the text insertion point.
      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });

    it('properly handles simple token-replacing transform', () => {
      const context = {
        left: 'anappleadate',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const editTransform = {
        insert: 'week',
        deleteLeft: 4
      };

      const mockMap = new Map<string, string[]>();
      mockMap.set('anappleadate', ['an', 'apple', 'a', 'date']);
      mockMap.set('anappleaweek', ['an', 'apple', 'a', 'week']);
      const result = tokenizeTransform(
        mockedTokenization(mockMap),
        context,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(0, editTransform); // The original token before the text insertion point.
      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });

    it('handles simple token-replacing transform with cross-token deleteLeft', () => {
      const context = {
        left: 'anappleadate',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      // 'an apple any'
      const editTransform = {
        insert: 'ny',
        deleteLeft: 4
      };

      const mockMap = new Map<string, string[]>();
      mockMap.set('anappleadate', ['an', 'apple', 'a', 'date']);
      mockMap.set('anappleany', ['an', 'apple', 'any']);
      const result = tokenizeTransform(
        mockedTokenization(mockMap),
        context,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(-1, { insert: 'ny', deleteLeft: 0 });
      expectedMap.set( 0, { insert: '', deleteLeft: 4 });
      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });

    it('handles word-breakable transforms (case 1)', () => {
      const context = {
        left: 'anappleadat',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const editTransform = {
        insert: 'yk',
        deleteLeft: 1
      };

      const mockMap = new Map<string, string[]>();
      mockMap.set('anappleadat',  ['an', 'apple', 'a', 'dat']);
      mockMap.set('anappleadayk', ['an', 'apple', 'a', 'day', 'k']);
      const result = tokenizeTransform(
        mockedTokenization(mockMap),
        context,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(0, { insert: 'y', deleteLeft: 1 });
      expectedMap.set(1, { insert: 'k', deleteLeft: 0 });
      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });

    it('handles word-breakable transforms (case 2)', () => {
      const context = {
        left: 'anappleadat',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const editTransform = {
        insert: 'y.',
        deleteLeft: 1
      };

      const mockMap = new Map<string, string[]>();
      mockMap.set('anappleadat',  ['an', 'apple', 'a', 'dat']);
      mockMap.set('anappleaday.', ['an', 'apple', 'a', 'day', '.']);
      const result = tokenizeTransform(
        mockedTokenization(mockMap),
        context,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(0, { insert: 'y', deleteLeft: 1 });
      expectedMap.set(1, { insert: '.', deleteLeft: 0 });
      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });

    it('handles word-breakable transforms (case 2 alternate output)', () => {
      const context = {
        left: 'anappleadat',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const editTransform = {
        insert: 'y.',
        deleteLeft: 1
      };

      const mockMap = new Map<string, string[]>();
      mockMap.set('anappleadat', ['an', 'apple', 'a', 'dat']);
      mockMap.set('anappleaday.', ['an', 'apple', 'a', 'day', '.', '']);
      const result = tokenizeTransform(
        mockedTokenization(mockMap),
        context,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(0, { insert: 'y', deleteLeft: 1 });
      expectedMap.set(1, { insert: '.', deleteLeft: 0 });
      expectedMap.set(2, { insert: '',  deleteLeft: 0});
      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });

    it('handles complex breakable cases', () => {
      const context = {
        left: 'anappleadate',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      // 'an apple any'
      const editTransform = {
        insert: 'nyday',
        deleteLeft: 4
      };

      const mockMap = new Map<string, string[]>();
      mockMap.set('anappleadate', ['an', 'apple', 'a', 'date']);
      mockMap.set('anappleanyday', ['an', 'apple', 'any', 'day']);
      const result = tokenizeTransform(
        mockedTokenization(mockMap),
        context,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(-1, { insert: 'ny', deleteLeft: 0 });
      expectedMap.set( 0, { insert: 'day', deleteLeft: 4 });
      assert.equal(result.size, expectedMap.size);
      assert.deepEqual(result, expectedMap);
    });
  });
});