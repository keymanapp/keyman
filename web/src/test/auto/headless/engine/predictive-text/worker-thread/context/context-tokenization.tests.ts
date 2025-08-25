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

import { ContextStateAlignment, ContextToken, ContextTokenization, models } from '@keymanapp/lm-worker/test-index';

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
        leadTokenShift: 0,
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
        leadTokenShift: 0,
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
          leadTokenShift: 0,
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
          leadTokenShift: 0,
          matchLength: 6,
          tailEditLength: 1,
          tailTokenShift: 0
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
          leadTokenShift: 0,
          matchLength: 22,
          tailEditLength: 1,
          tailTokenShift: 0
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1}]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
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
          leadTokenShift: 0,
          matchLength: 24,
          tailEditLength: 1,
          tailTokenShift: 0
        },
        plainModel,
        [{ sample: inputTransformMap, p: 1}]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
    });
  });
});
