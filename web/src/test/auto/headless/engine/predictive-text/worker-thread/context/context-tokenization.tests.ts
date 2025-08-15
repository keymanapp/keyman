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

import { ContextToken, ContextTokenization, models } from '@keymanapp/lm-worker/test-index';

import TrieModel = models.TrieModel;

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

function buildBaseTokenization(textTokens: string[]) {
  const tokens = textTokens.map((entry) => toToken(entry));
  return new ContextTokenization(tokens);
}

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
      let alignment = {
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

  describe('computeAlignment', () => {
    it("properly matches and aligns when contexts match", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [...baseContext];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: 0,
        matchLength: 5,
        tailEditLength: 0,
        tailTokenShift: 0
      });
    });

    it("properly matches and aligns with applied-suggestion contexts", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'o'
      ];
      const newContext = [...baseContext];
      newContext[4] = 'over';

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: 0,
        matchLength: 4,
        tailEditLength: 1,
        tailTokenShift: 0
      });
    });

    it("properly matches and aligns with applied-suggestion at start of context", () => {
      const baseContext = [
        'te'
      ];
      const newContext = [
        'testing',
        ' ',
        ''
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext, true);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: 0,
        matchLength: 0,
        tailEditLength: 1,
        tailTokenShift: 2
      });
    });

    it("detects unalignable contexts - no matching tokens", () => {
      const baseContext = [
        'swift', 'tan', 'wolf', 'leaped', 'across'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("detects unalignable contexts - too many mismatching tokens", () => {
      const baseContext = [
        'swift', 'tan', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("fails alignment for leading-edge word substitutions", () => {
      const baseContext = [
        'swift', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("fails alignment for small leading-edge word substitutions", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'sick', 'brown', 'fox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("properly matches and aligns when lead token is modified", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'uick', 'brown', 'fox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: 0,
        matchLength: 5,
        tailEditLength: 0,
        tailTokenShift: 0
      });
    });

    it("properly matches and aligns when lead token is removed", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'brown', 'fox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: -1,
        matchLength: 4,
        tailEditLength: 0,
        tailTokenShift: 0
      });
    });

    it("properly matches and aligns when lead token is added", () => {
      const baseContext = [
        'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: 1,
        matchLength: 4,
        tailEditLength: 0,
        tailTokenShift: 0
      });
    });

    it("properly matches and aligns when lead tokens are removed and modified", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'ox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: -2,
        matchLength: 3,
        tailEditLength: 0,
        tailTokenShift: 0
      });
    });

    it("properly matches and aligns when lead tokens are added and modified", () => {
      const baseContext = [
        'rown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: 1,
        matchLength: 4,
        tailEditLength: 0,
        tailTokenShift: 0
      });
    });

    it("properly matches and aligns when lead token is removed and tail token is added", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'brown', 'fox', 'jumped', 'over', 'the'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: -1,
        matchLength: 4,
        tailEditLength: 0,
        tailTokenShift: 1
      });
    });

    it("properly matches and aligns when lead token and tail token are modified", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'ove'
      ];
      const newContext = [
        'uick', 'brown', 'fox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: 0,
        matchLength: 4, // we treat 'quick' and 'uick' as the same
        tailEditLength: 1,
        tailTokenShift: 0
      });
    });

    it("properly matches and aligns when lead token and tail token are modified + new token appended", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'ove'
      ];
      const newContext = [
        'uick', 'brown', 'fox', 'jumped', 'over', 't'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: 0,
        matchLength: 4, // we treat 'quick' and 'uick' as the same
        tailEditLength: 1,
        tailTokenShift: 1
      });
    });

    it("properly handles context window sliding backward", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'e', 'quick', 'brown', 'fox', 'jumped', 'ove'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: 1,
        matchLength: 4, // we treat 'quick' and 'uick' as the same
        tailEditLength: 1,
        tailTokenShift: 0
      });
    });

    it("properly handles context window sliding far backward", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'the', 'quick', 'brown', 'fox', 'jumped'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: 1,
        matchLength: 4, // we treat 'quick' and 'uick' as the same
        tailEditLength: 0,
        tailTokenShift: -1
      });
    });

    it("properly handles context window sliding farther backward", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'the', 'quick', 'brown', 'fox', 'jumpe'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {
        canAlign: true,
        leadTokenShift: 1,
        matchLength: 3, // we treat 'quick' and 'uick' as the same
        tailEditLength: 1,
        tailTokenShift: -1
      });
    });

    it("fails alignment for mid-head deletion", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'fox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("fails alignment for mid-head insertion", () => {
      const baseContext = [
        'quick', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("fails alignment for mid-tail deletion", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("fails alignment for mid-tail insertion", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'jumped', 'far', 'over'
      ];

      const baseTokenization = buildBaseTokenization(baseContext);
      const computedAlignment = baseTokenization.computeAlignment(newContext);

      assert.deepEqual(computedAlignment, {canAlign: false});
    });
  });

  describe('transitionTo', function() {
    it('simple case - new whitespace + new empty token', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)), null);

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''].map((t) => ({text: t, isWhitespace: t == ' '}));
      const tokenization = baseTokenization.transitionTo(
        targetTokens, {
          canAlign: true,
          leadTokenShift: 0,
          matchLength: 7,
          tailEditLength: 0,
          tailTokenShift: 2
        },
        plainModel,
        [{ sample: [{ insert: ' ', deleteLeft: 0 }, { insert: '', deleteLeft: 0 }], p: 1}]
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
      const tokenization = baseTokenization.transitionTo(
        targetTokens, {
          canAlign: true,
          leadTokenShift: 0,
          matchLength: 6,
          tailEditLength: 1,
          tailTokenShift: 0
        },
        plainModel,
        [{ sample: [{ insert: 'y', deleteLeft: 0 }], p: 1}]
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
      const tokenization = baseTokenization.transitionTo(
        targetTokens, {
          canAlign: true,
          leadTokenShift: 0,
          matchLength: 22,
          tailEditLength: 1,
          tailTokenShift: 0
        },
        plainModel,
        [{ sample: [{ insert: 'b', deleteLeft: 0 }], p: 1}]
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      // The full original token should be preserved.
      assert.equal(tokenization.tokens[0].exampleInput, 'applesauce');
      assert.deepEqual(tokenization.tokens.map(
        (t, i) => ({
          text: i == 0 ? 'pplesauce' : t.exampleInput,
          isWhitespace: t.isWhitespace
        })),
        targetTokens
      );
    });
  });
});
