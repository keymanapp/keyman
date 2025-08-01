import { assert } from 'chai';
import { isSubstitutionAlignable } from "#./correction/alignment-helpers.js";

import { ContextToken } from '#./correction/context-token.js';
import { ContextTokenization } from '#./correction/context-tokenization.js';

import * as models from '#./models/index.js';
import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

var TrieModel = models.TrieModel;

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

function toToken(text) {
  let isWhitespace = text == ' ';
  let token = new ContextToken(plainModel, text);
  token.isWhitespace = isWhitespace;
  return token;
}

describe('isSubstitutionAlignable', () => {
  it(`returns true:  'ca' => 'can'`, () => {
    assert.isTrue(isSubstitutionAlignable('can', 'ca'));
  });

  // Leading word in context window starts sliding out of said window.
  it(`returns true:  'can' => 'an'`, () => {
    assert.isTrue(isSubstitutionAlignable('an', 'can'));
  });

  // Same edits on both sides:  not valid.
  it(`returns false: 'apple' => 'grapples'`, () => {
    assert.isFalse(isSubstitutionAlignable('grapples', 'apple'));
  });

  // Edits on one side:  valid.
  it(`returns true: 'apple' => 'grapple'`, () => {
    assert.isTrue(isSubstitutionAlignable('grapple', 'apple'));
  });

  // Edits on one side:  valid.
  it(`returns true: 'apple' => 'grapple'`, () => {
    assert.isTrue(isSubstitutionAlignable('apples', 'apple'));
  });

  // Same edits on both sides:  not valid.
  it(`returns false: 'grapples' => 'apple'`, () => {
    assert.isFalse(isSubstitutionAlignable('apple', 'grapples'));
  });

  // Substitution:  not valid when not permitted via parameter.
  it(`returns false:  'apple' => 'banana'`, () => {
    // edit path:  'insert' ('b' of banana), 'match' (on leading a), rest are 'substitute'.
    assert.isFalse(isSubstitutionAlignable('banana', 'apple'));
  });

  // Substitution:  not valid if too much is substituted, even if allowed via parameter.
  it(`returns false:  'apple' => 'banana' (subs allowed)`, () => {
    // edit path:  'insert' ('b' of banana), 'match' (on leading a), rest are 'substitute'.
    // 1 match vs 4 substitute = no bueno.  It'd require too niche of a keyboard rule.
    assert.isFalse(isSubstitutionAlignable('banana', 'apple', true));
  });

  it(`returns true: 'a' => 'à' (subs allowed)`, () => {
    assert.isTrue(isSubstitutionAlignable('à', 'a', true));
  });

  // Leading substitution:  valid if enough of the remaining word matches.
  // Could totally happen from a legit Keyman keyboard rule.
  it(`returns true: 'can' => 'van' (subs allowed)`, () => {
    assert.isTrue(isSubstitutionAlignable('van', 'can', true));
  });

  // Trailing substitution:  invalid if not allowed.
  it(`returns false: 'can' => 'cap' (subs not allowed)`, () => {
    assert.isFalse(isSubstitutionAlignable('cap', 'can'));
  });

  // Trailing substitution:  valid.
  it(`returns false: 'can' => 'cap' (subs allowed)`, () => {
    assert.isTrue(isSubstitutionAlignable('cap', 'can', true));
  });

  it(`returns true:  'clasts' => 'clasps' (subs allowed)`, () => {
    assert.isTrue(isSubstitutionAlignable('clasps', 'clasts', true));
  });

  // random deletion at the start + later substitution = still permitted
  it(`returns false:  'clasts' => 'lasps' (subs allowed)`, () => {
    assert.isTrue(isSubstitutionAlignable('lasps', 'clasts', true));
  });
});

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
      assert.isUndefined(tokenization.tail.appliedSuggestionId);
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
      assert.isUndefined(tokenization.tail.appliedSuggestionId);
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
      assert.notDeepEqual(cloned.tokens, baseTokenization.tokens);
      assert.deepEqual(cloned.tokens.map((token) => token.searchSpace.inputSequence),
        baseTokenization.tokens.map((token) => token.searchSpace.inputSequence));
      assert.deepEqual(cloned.alignment, baseTokenization.alignment);
    });
  });

  it('exampleInput', () => {
    const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    assert.deepEqual(tokenization.exampleInput, rawTextTokens);
  });
});
