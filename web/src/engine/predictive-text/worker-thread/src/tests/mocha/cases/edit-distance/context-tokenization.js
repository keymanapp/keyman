import { assert } from 'chai';
import { isSubstitutionAlignable } from "#./correction/alignment-helpers.js";
import { ContextTokenization } from "#./correction/context-tokenization.js";
import { ContextToken } from '#./correction/context-token.js';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { TrieModel } from '#./models/index.js';
import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';

// TODO:  consider mocking out the need for SearchSpace stuff?

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

function buildBaseTokenization(textTokens) {
  const tokens = textTokens.map((entry) => new ContextToken(plainModel, entry));
  return new ContextTokenization(tokens);
}

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
});
