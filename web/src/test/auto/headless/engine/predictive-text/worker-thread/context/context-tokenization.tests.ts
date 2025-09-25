/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * This file contains low-level tests designed to validate the behavior of the
 * of the ContextTokenization class and its integration with the lower-level
 * classes that it utilizes.
 */

import { assert } from 'chai';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { analyzePathMergesAndSplits, buildEdgeWindow, ContextStateAlignment, ContextToken, ContextTokenization, EditOperation, EditTuple, ExtendedEditOperation, models, traceInsertEdits } from '@keymanapp/lm-worker/test-index';

import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

function toToken(text: string) {
  let isWhitespace = text == ' ';
  let token = new ContextToken(plainModel, text);
  token.isWhitespace = isWhitespace;
  return token;
}

describe('ContextTokenization', function() {
  describe("<constructor>", () => {
    it("constructs from just a token array", () => {
      const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));
      assert.deepEqual(tokenization.tokens.map((entry) => entry.exampleInput), rawTextTokens);
      assert.deepEqual(tokenization.tokens.map((entry) => entry.isWhitespace), rawTextTokens.map((entry) => entry == ' '));
      assert.isNotOk(tokenization.alignment);
      assert.equal(tokenization.tail.exampleInput, 'day');
      assert.isFalse(tokenization.tail.isWhitespace);
    });

    it("constructs from a token array + alignment data", () => {
      const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      let alignment: ContextStateAlignment = {
        canAlign: true,
        editPath: [
          {op: 'match', input: 0, match: 0},
          {op: 'match', input: 1, match: 1},
          {op: 'match', input: 2, match: 2},
          {op: 'match', input: 3, match: 3},
          {op: 'match', input: 4, match: 4},
          {op: 'match', input: 5, match: 5},
          {op: 'match', input: 6, match: 6}
        ],
        leadTokenShift: 0,
        leadEditLength: 0,
        matchLength: 6,
        tailEditLength: 1,
        tailTokenShift: 0
      };

      let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))), alignment);

      assert.deepEqual(tokenization.tokens.map((entry) => entry.exampleInput), rawTextTokens);
      assert.deepEqual(tokenization.tokens.map((entry) => entry.isWhitespace), rawTextTokens.map((entry) => entry == ' '));
      assert.isOk(tokenization.alignment);
      assert.deepEqual(tokenization.alignment, alignment);
      assert.equal(tokenization.tail.exampleInput, 'day');
      assert.isFalse(tokenization.tail.isWhitespace);
    });

    it('clones', () => {
      const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];

      let baseTokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))), {
        canAlign: true,
        editPath: [
          {op: 'match', input: 0, match: 0},
          {op: 'match', input: 1, match: 1},
          {op: 'match', input: 2, match: 2},
          {op: 'match', input: 3, match: 3},
          {op: 'match', input: 4, match: 4},
          {op: 'match', input: 5, match: 5},
          {op: 'match', input: 6, match: 6}
        ],
        leadTokenShift: 0,
        leadEditLength: 0,
        matchLength: 6,
        tailEditLength: 1,
        tailTokenShift: 0
      });

      let cloned = new ContextTokenization(baseTokenization);

      assert.notDeepEqual(cloned, baseTokenization);
      assert.deepEqual(cloned.tokens.map((token) => token.searchSpace.inputSequence),
        baseTokenization.tokens.map((token) => token.searchSpace.inputSequence));

      // The `.searchSpace` instances will not be deep-equal; there are class properties
      // that hold functions with closures, configured at runtime.

      // @ts-ignore - TS2704 b/c deleting a readonly property.
      baseTokenization.tokens.forEach((token) => delete token.searchSpace);
      // @ts-ignore - TS2704 b/c deleting a readonly property.
      cloned.tokens.forEach((token) => delete token.searchSpace);

      assert.deepEqual(cloned, baseTokenization);
    });
  });

  it('exampleInput', () => {
    const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    assert.deepEqual(tokenization.exampleInput, rawTextTokens);
  });

  describe('transitionTo', function() {
    it('simple case - new whitespace + new empty token', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''].map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(1, { insert: ' ', deleteLeft: 0 });
      inputTransformMap.set(2, { insert: '', deleteLeft: 0 });

      const tokenization = baseTokenization.transitionTo(
        targetTokens, {
          canAlign: true,
          editPath: [
            {op: 'match', input: 0, match: 0},
            {op: 'match', input: 1, match: 1},
            {op: 'match', input: 2, match: 2},
            {op: 'match', input: 3, match: 3},
            {op: 'match', input: 4, match: 4},
            {op: 'match', input: 5, match: 5},
            {op: 'match', input: 6, match: 6},
            {op: 'insert', match: 7},
            {op: 'insert', match: 8}
          ],
          leadTokenShift: 0,
          leadEditLength: 0,
          matchLength: 7,
          tailEditLength: 0,
          tailTokenShift: 2
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1}]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );
    });

    it('simple case - new character added to last token', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'da'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'].map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(0, { insert: 'y', deleteLeft: 0 });

      const tokenization = baseTokenization.transitionTo(
        targetTokens, {
          canAlign: true,
          editPath: [
            {op: 'match', input: 0, match: 0},
            {op: 'match', input: 1, match: 1},
            {op: 'match', input: 2, match: 2},
            {op: 'match', input: 3, match: 3},
            {op: 'match', input: 4, match: 4},
            {op: 'match', input: 5, match: 5},
            {op: 'substitute', input: 6, match: 6}
          ],
          leadTokenShift: 0,
          leadEditLength: 0,
          matchLength: 6,
          tailEditLength: 1,
          tailTokenShift: 0
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1 }]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );
    });

    it('merges new whitespace character added to last whitespace token if tail is empty', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', '  ', ''].map((t) => (
        {text: t, isWhitespace: t != '' && t.trim() == ''}
      ));
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(-1, { insert: ' ', deleteLeft: 0 });
      inputTransformMap.set( 0, { insert: '',  deleteLeft: 0 });

      const tokenization = baseTokenization.transitionTo(
        targetTokens, {
          canAlign: true,
          editPath: [
            {op: 'match', input: 0, match: 0},
            {op: 'match', input: 1, match: 1},
            {op: 'match', input: 2, match: 2},
            {op: 'match', input: 3, match: 3},
            {op: 'match', input: 4, match: 4},
            {op: 'match', input: 5, match: 5},
            {op: 'match', input: 6, match: 6},
            {op: 'substitute', input: 7, match: 7},
            {op: 'substitute', input: 8, match: 8}
          ],
          leadTokenShift: 0,
          leadEditLength: 0,
          matchLength: 7,
          tailEditLength: 2,
          tailTokenShift: 0
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1 }]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );
    });

    it('simple case - context-window slide deletes first char of word', () => {
      // string length: 64
      const baseTexts = [
        "applesauce", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ",
        "seem", " ", "like", " ", "they'd", " ", "make", " ", "for", " ", "the", " ", ""
      ];
      assert.equal(baseTexts.join('').length, 64);

      assert.equal(baseTexts.length, 23);
      const baseTokenization = new ContextTokenization(baseTexts.map(t => toToken(t)), null);

      const targetTexts = [
        "pplesauce", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ",
        "seem", " ", "like", " ", "they'd", " ", "make", " ", "for", " ", "the", " ", "b"
      ];
      const targetTokens = targetTexts.map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(0, { insert: 'b', deleteLeft: 0 });

      const tokenization = baseTokenization.transitionTo(
        targetTokens, {
          canAlign: true,
          editPath: [
            {op: 'substitute', input: 0, match: 0},
            {op: 'match', input: 1, match: 1},
            {op: 'match', input: 2, match: 2},
            {op: 'match', input: 3, match: 3},
            {op: 'match', input: 4, match: 4},
            {op: 'match', input: 5, match: 5},
            {op: 'match', input: 6, match: 6},
            {op: 'match', input: 7, match: 7},
            {op: 'match', input: 8, match: 8},
            {op: 'match', input: 9, match: 9},
            {op: 'match', input: 10, match: 10},
            {op: 'match', input: 11, match: 11},
            {op: 'match', input: 12, match: 12},
            {op: 'match', input: 13, match: 13},
            {op: 'match', input: 14, match: 14},
            {op: 'match', input: 15, match: 15},
            {op: 'match', input: 16, match: 16},
            {op: 'match', input: 17, match: 17},
            {op: 'match', input: 18, match: 18},
            {op: 'match', input: 19, match: 19},
            {op: 'match', input: 20, match: 20},
            {op: 'match', input: 21, match: 21},
            {op: 'substitute', input: 22, match: 22}
          ],
          leadTokenShift: 0,
          leadEditLength: 1,
          matchLength: 21,
          tailEditLength: 1,
          tailTokenShift: 0
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1}]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.sameOrderedMembers(tokenization.tokens.map(t => t.exampleInput), targetTexts);
    });

    it('context-window slide deletes majority of word', () => {
      // string length: 73
      const baseTexts = [
        "applesauce", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem", " ",
        "like", " ", "they'd", " ", "make", " ", "for", " ", "the", " ", "best", " ", "brea"
      ];
      assert.equal(baseTexts.join('').length, 73);

      assert.equal(baseTexts.length, 25);
      const baseTokenization = new ContextTokenization(baseTexts.map(t => toToken(t)), null);

      const targetTexts = [
        "e", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem", " ",
        "like", " ", "they'd", " ", "make", " ", "for", " ", "the", " ", "best", " ", "break"
      ];
      const targetTokens = targetTexts.map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(0, { insert: 'k', deleteLeft: 0 });

      const tokenization = baseTokenization.transitionTo(
        targetTokens, {
          canAlign: true,
          editPath: [
            {op: 'substitute', input: 0, match: 0},
            {op: 'match', input: 1, match: 1},
            {op: 'match', input: 2, match: 2},
            {op: 'match', input: 3, match: 3},
            {op: 'match', input: 4, match: 4},
            {op: 'match', input: 5, match: 5},
            {op: 'match', input: 6, match: 6},
            {op: 'match', input: 7, match: 7},
            {op: 'match', input: 8, match: 8},
            {op: 'match', input: 9, match: 9},
            {op: 'match', input: 10, match: 10},
            {op: 'match', input: 11, match: 11},
            {op: 'match', input: 12, match: 12},
            {op: 'match', input: 13, match: 13},
            {op: 'match', input: 14, match: 14},
            {op: 'match', input: 15, match: 15},
            {op: 'match', input: 16, match: 16},
            {op: 'match', input: 17, match: 17},
            {op: 'match', input: 18, match: 18},
            {op: 'match', input: 19, match: 19},
            {op: 'match', input: 20, match: 20},
            {op: 'match', input: 21, match: 21},
            {op: 'match', input: 22, match: 22},
            {op: 'match', input: 23, match: 23},
            {op: 'substitute', input: 24, match: 24}
          ],
          leadTokenShift: 0,
          leadEditLength: 1,
          matchLength: 23,
          tailEditLength: 1,
          tailTokenShift: 0
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1}]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.sameOrderedMembers(tokenization.tokens.map(t => t.exampleInput), targetTexts);
    });

    it('handles extension of head token from backward context-window slide', () => {
      const baseTexts = [
        "sauce", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem"
      ];

      const baseTokenization = new ContextTokenization(baseTexts.map(t => toToken(t)), null);

      const targetTexts = [
        "applesauce", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem"
      ];
      const targetTokens = targetTexts.map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(0, { insert: '', deleteLeft: 0 });

      const tokenization = baseTokenization.transitionTo(
        // Yay for being able to mock the alignment data for the test! We don't
        // actually need to use a full 64-char string as long as we craft this
        // properly.
        targetTokens, {
          canAlign: true,
          editPath: [
            {op: 'substitute', input: 0, match: 0},
            {op: 'match', input: 1, match: 1},
            {op: 'match', input: 2, match: 2},
            {op: 'match', input: 3, match: 3},
            {op: 'match', input: 4, match: 4},
            {op: 'match', input: 5, match: 5},
            {op: 'match', input: 6, match: 6},
            {op: 'match', input: 7, match: 7},
            {op: 'match', input: 8, match: 8},
            {op: 'match', input: 9, match: 9},
            {op: 'match', input: 10, match: 10}
          ],
          leadTokenShift: 0,
          leadEditLength: 1,
          matchLength: baseTexts.length - 1,
          tailEditLength: 0,
          tailTokenShift: 0
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1}]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.sameOrderedMembers(tokenization.tokens.map(t => t.exampleInput), targetTexts);
    });

    it('handles large backward context-window slide jump', () => {
      // Note:  this is not the actual pathway used for reverting suggestions,
      // though the scenario is somewhat analogous.
      const baseTexts = [
        "nd", " ", "orange", " ", "juice", " ", "seem", " ", "like", " ", "breakfast"
      ];

      const baseTokenization = new ContextTokenization(baseTexts.map(t => toToken(t)), null);

      const targetTexts = [
        "sauce", " ", "and", " ", "orange", " ", "juice", " ", "seem", " ", "like", " ",
        "brea"
      ];
      const targetTokens = targetTexts.map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransformMap: Map<number, Transform> = new Map();

      inputTransformMap.set(0, { insert: '', deleteLeft: 5 });

      const tokenization = baseTokenization.transitionTo(
        // Yay for being able to mock the alignment data for the test! We don't
        // actually need to use a full 64-char string as long as we craft this
        // properly.
        targetTokens, {
          canAlign: true,
          editPath: [
            {op: 'insert', match: 0},
            {op: 'insert', match: 1},
            {op: 'match', input: 0, match: 2},
            {op: 'match', input: 1, match: 3},
            {op: 'match', input: 2, match: 4},
            {op: 'match', input: 3, match: 5},
            {op: 'match', input: 4, match: 6},
            {op: 'match', input: 5, match: 7},
            {op: 'match', input: 6, match: 8},
            {op: 'match', input: 7, match: 9},
            {op: 'match', input: 8, match: 10},
            {op: 'match', input: 9, match: 11},
            {op: 'match', input: 10, match: 12}
          ],
          leadTokenShift: 2, // "applesauce", " "
          leadEditLength: 1, // "nd" / "and"
          matchLength: baseTexts.length - 2,
          tailEditLength: 1, // "breakfast" / "brea"
          tailTokenShift: 0
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1}]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.equal(tokenization.tail.exampleInput, "brea");
      assert.equal(tokenization.tail.searchSpace.inputSequence.length, "brea".length);
      assert.sameOrderedMembers(tokenization.tokens.map(t => t.exampleInput), targetTexts);
    });

    it('handles word-break boundary shift during backward context-window slide', () => {
      const baseTexts = [
        // Without any preceding adjacent char in view, we can only interpret
        // the leading `'` as an opening single-quote.
        /*isn*/ "'", "t", " ", "orange", " ", "juice", " ", "tasty", "?", "  ", "I", " ", "think"
      ];

      const baseTokenization = new ContextTokenization(baseTexts.map(t => toToken(t)), null);

      const targetTexts = [
        // With one preceding adjacent non-whitespace char in view, we now
        // realize it was part of a word... and remove a wordbreak!
        /*is"*/ "n't", " ", "orange", " ", "juice", " ", "tasty", "?", "  ", "I", " ", "thin"
      ];
      const targetTokens = targetTexts.map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransformMap: Map<number, Transform> = new Map();

      inputTransformMap.set(0, { insert: '', deleteLeft: 1 });

      const tokenization = baseTokenization.transitionTo(
        targetTokens, {
          canAlign: true,
          editPath: [
            {op: 'delete', input: 0},
            {op: 'substitute', input: 1, match: 0},
            {op: 'substitute', input: 2, match: 1},
            {op: 'substitute', input: 3, match: 2},
            {op: 'substitute', input: 4, match: 3},
            {op: 'substitute', input: 5, match: 4},
            {op: 'substitute', input: 6, match: 5},
            {op: 'substitute', input: 7, match: 6},
            {op: 'substitute', input: 8, match: 7},
            {op: 'match', input: 9, match: 8},
            {op: 'match', input: 10, match: 9},
            {op: 'match', input: 11, match: 10},
            {op: 'match', input: 12, match: 11}
          ],
          leadTokenShift: -1, // "'",
          leadEditLength: 1, // "t" / "n't"
          matchLength: baseTexts.length - 3,
          tailEditLength: 1, // "think" / "thin"
          tailTokenShift: 0
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1}]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.equal(tokenization.tail.exampleInput, "thin");
      assert.equal(tokenization.tail.searchSpace.inputSequence.length, "thin".length);
      assert.sameOrderedMembers(tokenization.tokens.map(t => t.exampleInput), targetTexts);
    });

    it('handles word-break boundary shifts at both ends during backward context-window slide', () => {
      const baseTexts = [
        // Without any preceding adjacent char in view, we can only interpret
        // the leading `'` as an opening single-quote.
        /*isn*/ "'", "t", " ", "orange", " ", "juice", " ", "tasty", "?", "  ", "I", " ", "find", " ", ""
      ];

      const baseTokenization = new ContextTokenization(baseTexts.map(t => toToken(t)), null);

      const targetTexts = [
        // With one preceding adjacent non-whitespace char in view, we now
        // realize it was part of a word... and remove a wordbreak!
        /*is"*/ "n't", " ", "orange", " ", "juice", " ", "tasty", "?", "  ", "I", " ", "find"
      ];
      const targetTokens = targetTexts.map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransformMap: Map<number, Transform> = new Map();

      inputTransformMap.set(0, { insert: '', deleteLeft: 1 });

      const tokenization = baseTokenization.transitionTo(
        targetTokens, {
          canAlign: true,
          editPath: [
            {op: 'delete', input: 0},
            {op: 'substitute', input: 1, match: 0},
            {op: 'substitute', input: 2, match: 1},
            {op: 'substitute', input: 3, match: 2},
            {op: 'substitute', input: 4, match: 3},
            {op: 'substitute', input: 5, match: 4},
            {op: 'substitute', input: 6, match: 5},
            {op: 'substitute', input: 7, match: 6},
            {op: 'substitute', input: 8, match: 7},
            {op: 'match', input: 9, match: 8},
            {op: 'match', input: 10, match: 9},
            {op: 'match', input: 11, match: 10},
            {op: 'match', input: 12, match: 11},
            {op: 'delete', input: 13},
            {op: 'delete', input: 14}
          ],
          leadTokenShift: -1, // "'",
          leadEditLength: 1, // "t" / "n't"
          matchLength: baseTexts.length - 4,
          tailEditLength: 0,
          tailTokenShift: -2 // " ", ""
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1}]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.equal(tokenization.tail.exampleInput, "find");
      assert.equal(tokenization.tail.searchSpace.inputSequence.length, "find".length);
      assert.sameOrderedMembers(tokenization.tokens.map(t => t.exampleInput), targetTexts);
    });

    it('handles word-break boundary shifts at both ends during forward context-window slide', () => {
      const baseTexts = [
        // With one preceding adjacent non-whitespace char in view, it's part of
        // a word... and remove a wordbreak!
        /*is"*/ "n't", " ", "orange", " ", "juice", " ", "tasty", "?", "  ", "I", " ", "find"
      ];

      const baseTokenization = new ContextTokenization(baseTexts.map(t => toToken(t)), null);

      const targetTexts = [
        // Without any preceding adjacent char in view, we can only interpret
        // the leading `'` as an opening single-quote.
        /*isn*/ "'", "t", " ", "orange", " ", "juice", " ", "tasty", "?", "  ", "I", " ", "find", " ", ""
      ];
      const targetTokens = targetTexts.map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransformMap: Map<number, Transform> = new Map();

      inputTransformMap.set(1, { insert: ' ', deleteLeft: 0 });
      inputTransformMap.set(2, { insert: '', deleteLeft: 0 });

      const tokenization = baseTokenization.transitionTo(
        targetTokens, {
          canAlign: true,
          editPath: [
            {op: 'delete', input: 0},
            {op: 'substitute', input: 1, match: 0},
            {op: 'substitute', input: 2, match: 1},
            {op: 'substitute', input: 3, match: 2},
            {op: 'substitute', input: 4, match: 3},
            {op: 'substitute', input: 5, match: 4},
            {op: 'substitute', input: 6, match: 5},
            {op: 'substitute', input: 7, match: 6},
            {op: 'substitute', input: 8, match: 7},
            {op: 'match', input: 9, match: 8},
            {op: 'match', input: 10, match: 9},
            {op: 'match', input: 11, match: 10},
            {op: 'match', input: 12, match: 11},
            {op: 'insert', match: 12},
            {op: 'insert', match: 13}
          ],
          leadTokenShift: 1, // "'",
          leadEditLength: 1, // "n't" / "t"
          matchLength: baseTexts.length - 1,
          tailEditLength: 0,
          tailTokenShift: 2 // " ", ""
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1}]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.equal(tokenization.tail.exampleInput, "");
      assert.equal(tokenization.tail.searchSpace.inputSequence.length, "".length);
      assert.sameOrderedMembers(tokenization.tokens.map(t => t.exampleInput), targetTexts);
    });
  });

  describe('buildEdgeWindow', () => {
    describe('with min token count 3, char count 8', () => {
      const editWindowSpec = {
        minTokens: 3,
        minChars: 8
      }

      it('handles empty contexts', () => {
        const baseTokens = [''];
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 0 }, true, editWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: '',
          editBoundary: {
            isPartial: false,
            omitsEmptyToken: false,
            text: '',
            tokenIndex: 0
          },
          deleteLengths: [0],
          sliceIndex: 1
        });
      });

      it('handles empty contexts and invalid Transforms', () => {
        const baseTokens = [''];
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 2 }, true, editWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: '',
          editBoundary: {
            isPartial: true,
            omitsEmptyToken: false,
            text: '',
            tokenIndex: 0
          },
          deleteLengths: [0],
          sliceIndex: 1
        });
      });

      it('builds edge windows for the start of context with no edits', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 0 }, true, editWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: 'an apple',
          editBoundary: {
            isPartial: false,
            omitsEmptyToken: false,
            text: 'an',
            tokenIndex: 0
          },
          deleteLengths: [0],
          sliceIndex: 3
        });
      });

      it('builds edge windows for the start of context with deletion edits (1)', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 2 }, true, editWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: ' apple a',
          editBoundary: {
            isPartial: false,
            omitsEmptyToken: false,
            text: ' ',
            tokenIndex: 1
          },
          deleteLengths: [2, 0],
          sliceIndex: 5
        });
      });

      it('builds edge windows for the start of context with deletion edits (2)', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 4 }, true, editWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: 'pple a day',
          editBoundary: {
            isPartial: true,
            omitsEmptyToken: false,
            text: 'pple',
            tokenIndex: 2
          },
          deleteLengths: [2, 1, 1],
          sliceIndex: 7
        });
      });

      it('builds edge windows for the end of context with no edits', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);
        baseTokenization.tail.isPartial = true;

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 0 }, false, editWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: 'apple a day',
          editBoundary: {
            isPartial: true,
            omitsEmptyToken: false,
            text: 'day',
            tokenIndex: 6
          },
          deleteLengths: [0],
          sliceIndex: 2
        });
      });

      it('builds edge windows for the end of context with no edits, trailing whitespace', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 0 }, false, editWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: 'apple a day ',
          editBoundary: {
            isPartial: true,
            omitsEmptyToken: true,
            text: ' ',
            tokenIndex: 7
          },
          deleteLengths: [0],
          sliceIndex: 2
        });
      });
    });
  });

  describe('applyContextSlide', () => {
    it('handles empty contexts', () => {
      const baseTokens = [''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: '', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, ['']);
      assert.isFalse(resultTokenization.tokens[0].isPartial);
    });

    it('makes no changes when context does not slide', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);
      assert.isFalse(baseTokenization.tokens[0].isPartial);

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: '', deleteLeft: 0, deleteRight: 0});

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, baseTokens);
      assert.sameOrderedMembers(resultTokenization.exampleInput, baseTokenization.exampleInput);
      assert.isFalse(resultTokenization.tokens[0].isPartial);
    });

    it('preserves tokenization patterns when word slides partially out of window', () => {
      const baseTokens = ['apples', ' ', 'and', ' ', 'bananas'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: '', deleteLeft: 0, deleteRight: 2});

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.deepEqual(resultTokenization.exampleInput, ['ples', ' ', 'and', ' ', 'bananas']);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
    });

    it('does not preserve deleted tokens', () => {
      const baseTokens = ['apples', ' ', 'and', ' ', 'bananas'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: '', deleteLeft: 0, deleteRight: 7});

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.deepEqual(resultTokenization.exampleInput, ['and', ' ', 'bananas']);
      // preserves the entirety of what is now the first token
      assert.isFalse(resultTokenization.tokens[0].isPartial);
      assert.equal(resultTokenization.tail.exampleInput, baseTokenization.tail.exampleInput);
    });

    it('creates new lead tokens as needed', () => {
      const baseTokens = ['apples', ' ', 'and', ' ', 'bananas'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: 'I like ', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, ['I', ' ', 'like', ' ', ...baseTokens]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });

    it('creates new lead tokens and edits others as needed', () => {
      const baseTokens = ['apples', ' ', 'and', ' ', 'bananas'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: 'I like pine', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      // preserves the entirety of what is now the first token
      assert.sameOrderedMembers(resultTokenization.exampleInput, ['I', ' ', 'like', ' ', 'pineapples', ...baseTokens.slice(1)]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });

    it('updates internal tracking when backward slide adds word boundary', () => {
      const baseTokens = ['apples', ' ', 'and', ' ', 'bananas'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: ' ', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, [' ', ...baseTokens]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });

    it('handles tokenization shift (from split) when text inserted at start', () => {
      const baseTokens = ['\'t', ' ', 'talk'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: ' ', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, [' ', '\'', 't', ...baseTokens.slice(1)]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });

    it('handles tokenization shift (from merge) when text inserted at start', () => {
      const baseTokens = ['\'', 't', ' ', 'talk'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: 'n', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, ['n\'t', ...baseTokens.slice(2)]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });
  });

  describe('traceInsertEdits', () => {
    it('handles zero-length insert cases (1)', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const result = traceInsertEdits(tokens, {insert: '', deleteLeft: 0});

      assert.deepEqual(result, {
        stackedInserts: [''],
        firstInsertPostIndex: tokens.length - 1
      });
    });

    it('handles zero-length insert cases (2)', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
      const result = traceInsertEdits(tokens, {insert: '', deleteLeft: 0});

      assert.deepEqual(result, {
        stackedInserts: [''],
        firstInsertPostIndex: tokens.length - 1
      });
    });

    it('ignores deleteLefts and deleteRights', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const result = traceInsertEdits(tokens, {insert: '', deleteLeft: 10, deleteRight: -10});

      assert.deepEqual(result, {
        stackedInserts: [''],
        firstInsertPostIndex: tokens.length - 1
      });
    });

    it('handles simple char output transforms', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const result = traceInsertEdits(tokens, {insert: 'y', deleteLeft: 0});

      assert.deepEqual(result, {
        stackedInserts: ['y'],
        firstInsertPostIndex: tokens.length - 1
      });
    });

    it('handles standard whitespace wordbreaks', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
      const result = traceInsertEdits(tokens, {insert: ' ', deleteLeft: 0});

      assert.deepEqual(result, {
        stackedInserts: ['', ' '],
        firstInsertPostIndex: tokens.length - 2
      });
    });

    it('handles large insert strings', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
      const result = traceInsertEdits(tokens, {insert: 'ple a day ', deleteLeft: 0});

      assert.deepEqual(result, {
        stackedInserts: ['', ' ', 'day', ' ', 'a', ' ', 'ple'],
        firstInsertPostIndex: 2
      });
    });
  });

  describe('analyzePathMergesAndSplits', () => {
    it('handles empty tokenizations', () => {
      const results = analyzePathMergesAndSplits([], []);

      assert.deepEqual(results, {
        merges: [],
        splits: [],
        mergeOffset: 0,
        splitOffset: 0,
        editPath: [],
        mappedPath: []
      });
    });

    it('returns unadjusted edit path when no merges or splits are found', () => {
      const results = analyzePathMergesAndSplits(
        ['an', ' ', 'apple', ' ', 'a', ' ', 'da'],
        ['an', ' ', 'apple', ' ', 'a', ' ', 'day']
      );

      const editPath: EditTuple<EditOperation>[] = [
        { op: 'match', input: 0, match: 0 },
        { op: 'match', input: 1, match: 1 },
        { op: 'match', input: 2, match: 2 },
        { op: 'match', input: 3, match: 3 },
        { op: 'match', input: 4, match: 4 },
        { op: 'match', input: 5, match: 5 },
        { op: 'substitute', input: 6, match: 6 },
      ];

      assert.deepEqual(results, {
        merges: [],
        splits: [],
        mergeOffset: 0,
        splitOffset: 0,
        editPath: editPath,
        mappedPath: editPath
      });
    });

    it('returns adjusted path when merges are found', () => {
      const results = analyzePathMergesAndSplits(
        ['she', ' ', 'said', ' ', 'I', ' ', 'can', '\''],
        ['she', ' ', 'said', ' ', 'I', ' ', 'can\'t']
      );

      const editPath: EditTuple<ExtendedEditOperation>[] = [
        { op: 'match', input: 0, match: 0 },
        { op: 'match', input: 1, match: 1 },
        { op: 'match', input: 2, match: 2 },
        { op: 'match', input: 3, match: 3 },
        { op: 'match', input: 4, match: 4 },
        { op: 'match', input: 5, match: 5 },
        { op: 'merge', input: 6, match: 6 }, // gets the 'can'
        { op: 'merge', input: 7, match: 6 }, // gets the \', appends the t
      ];

      const mappedPath: EditTuple<EditOperation>[] = [
        { op: 'match', input: 0, match: 0 },
        { op: 'match', input: 1, match: 1 },
        { op: 'match', input: 2, match: 2 },
        { op: 'match', input: 3, match: 3 },
        { op: 'match', input: 4, match: 4 },
        { op: 'match', input: 5, match: 5 },
        { op: 'substitute', input: 6, match: 6 }, // gets all pieces + the new input `t`.
      ];

      assert.deepEqual(results, {
        merges: [ {
          inputs: [ { text: 'can', index: 6 }, { text: '\'', index: 7 }],
          match: { text: 'can\'t', index: 6 }
        } ],
        splits: [],
        mergeOffset: -1,
        splitOffset: 0,
        editPath,
        mappedPath
      });
    });

    it('returns adjusted path when splits are found', () => {
      const results = analyzePathMergesAndSplits(
        ['\'', 'she', ' ', 'said', ' ', 'I', ' ', 'can\''],
        ['\'', 'she', ' ', 'said', ' ', 'I', ' ', 'can' , '\'', '!']
      );

      const editPath: EditTuple<ExtendedEditOperation>[] = [
        { op: 'match', input: 0, match: 0 },
        { op: 'match', input: 1, match: 1 },
        { op: 'match', input: 2, match: 2 },
        { op: 'match', input: 3, match: 3 },
        { op: 'match', input: 4, match: 4 },
        { op: 'match', input: 5, match: 5 },
        { op: 'match', input: 6, match: 6 },
        { op: 'split', input: 7, match: 7 }, // gets the 'can'
        { op: 'split', input: 7, match: 8 }, // gets the \'
        { op: 'insert', match: 9 }, // gets the t
      ];

      const mappedPath: EditTuple<EditOperation>[] = [
        { op: 'match', input: 0, match: 0 },
        { op: 'match', input: 1, match: 1 },
        { op: 'match', input: 2, match: 2 },
        { op: 'match', input: 3, match: 3 },
        { op: 'match', input: 4, match: 4 },
        { op: 'match', input: 5, match: 5 },
        { op: 'match', input: 6, match: 6 },
        { op: 'match', input: 7, match: 7 }, // gets the 'can'
        { op: 'match', input: 8, match: 8 }, // gets the \'
        // gets the t, which wasn't available as part of the original token
        // being split
        { op: 'insert', match: 9 },
      ];

      assert.deepEqual(results, {
        merges: [],
        splits: [ {
          input: { text: 'can\'', index: 7 },
          matches: [ { text: 'can', index: 7, textOffset: 0 }, { text: '\'', index: 8, textOffset: 3 }]
        } ],
        mergeOffset: 0,
        splitOffset: -1,
        editPath,
        mappedPath
      });
    });
  });
});
