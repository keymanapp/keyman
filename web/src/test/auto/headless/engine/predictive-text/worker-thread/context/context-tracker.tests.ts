import { assert } from 'chai';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { deepCopy } from '@keymanapp/web-utils';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { ContextTracker, determineModelTokenizer, ModelCompositor, models, tokenizeTransformDistribution } from '@keymanapp/lm-worker/test-index';

import Suggestion = LexicalModelTypes.Suggestion;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

const plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker}
);

const tokenizer = determineModelTokenizer(new models.DummyModel({wordbreaker: defaultBreaker}));

describe('ContextTracker', function() {
  function toWrapperDistribution(transforms: Transform | Transform[]) {
    transforms = Array.isArray(transforms) ? transforms : [transforms];
    return [{
      sample: transforms,
      p: 1.0
    }];
  }

  describe('isSubstitutionAlignable', () => {
    it(`returns true:  'ca' => 'can'`, () => {
      assert.isTrue(ContextTracker.isSubstitutionAlignable('can', 'ca'));
    });

    // Leading word in context window starts sliding out of said window.
    it(`returns true:  'can' => 'an'`, () => {
      assert.isTrue(ContextTracker.isSubstitutionAlignable('an', 'can'));
    });

    // Same edits on both sides:  not valid.
    it(`returns false: 'apple' => 'grapples'`, () => {
      assert.isFalse(ContextTracker.isSubstitutionAlignable('grapples', 'apple'));
    });

    // Edits on one side:  valid.
    it(`returns true: 'apple' => 'grapple'`, () => {
      assert.isTrue(ContextTracker.isSubstitutionAlignable('grapple', 'apple'));
    });

    // Edits on one side:  valid.
    it(`returns true: 'apple' => 'grapple'`, () => {
      assert.isTrue(ContextTracker.isSubstitutionAlignable('apples', 'apple'));
    });

    // Same edits on both sides:  not valid.
    it(`returns false: 'grapples' => 'apple'`, () => {
      assert.isFalse(ContextTracker.isSubstitutionAlignable('apple', 'grapples'));
    });

    // Substitution:  not valid when not permitted via parameter.
    it(`returns false:  'apple' => 'banana'`, () => {
      // edit path:  'insert' ('b' of banana), 'match' (on leading a), rest are 'substitute'.
      assert.isFalse(ContextTracker.isSubstitutionAlignable('banana', 'apple'));
    });

    // Substitution:  not valid if too much is substituted, even if allowed via parameter.
    it(`returns false:  'apple' => 'banana' (subs allowed)`, () => {
      // edit path:  'insert' ('b' of banana), 'match' (on leading a), rest are 'substitute'.
      // 1 match vs 4 substitute = no bueno.  It'd require too niche of a keyboard rule.
      assert.isFalse(ContextTracker.isSubstitutionAlignable('banana', 'apple', true));
    });

    it(`returns true: 'a' => 'à' (subs allowed)`, () => {
      assert.isTrue(ContextTracker.isSubstitutionAlignable('à', 'a', true));
    });

    // Leading substitution:  valid if enough of the remaining word matches.
    // Could totally happen from a legit Keyman keyboard rule.
    it(`returns true: 'can' => 'van' (subs allowed)`, () => {
      assert.isTrue(ContextTracker.isSubstitutionAlignable('van', 'can', true));
    });

    // Trailing substitution:  invalid if not allowed.
    it(`returns false: 'can' => 'cap' (subs not allowed)`, () => {
      assert.isFalse(ContextTracker.isSubstitutionAlignable('cap', 'can'));
    });

    // Trailing substitution:  valid.
    it(`returns false: 'can' => 'cap' (subs allowed)`, () => {
      assert.isTrue(ContextTracker.isSubstitutionAlignable('cap', 'can', true));
    });

    it(`returns true:  'clasts' => 'clasps' (subs allowed)`, () => {
      assert.isTrue(ContextTracker.isSubstitutionAlignable('clasps', 'clasts', true));
    });

    // random deletion at the start + later substitution = still permitted
    it(`returns false:  'clasts' => 'lasps' (subs allowed)`, () => {
      assert.isTrue(ContextTracker.isSubstitutionAlignable('lasps', 'clasts', true));
    });
  });

  describe('attemptTokenizedAlignment', () => {
    it("properly matches and aligns when contexts match", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [...baseContext];

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("detects unalignable contexts - too many mismatching tokens", () => {
      const baseContext = [
        'swift', 'tan', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("fails alignment for leading-edge word substitutions", () => {
      const baseContext = [
        'swift', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("fails alignment for small leading-edge word substitutions", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'sick', 'brown', 'fox', 'jumped', 'over'
      ];

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("properly matches and aligns when lead token is modified", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'uick', 'brown', 'fox', 'jumped', 'over'
      ];

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
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

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("fails alignment for mid-head insertion", () => {
      const baseContext = [
        'quick', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("fails alignment for mid-tail deletion", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'over'
      ];

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
      assert.deepEqual(computedAlignment, {canAlign: false});
    });

    it("fails alignment for mid-tail insertion", () => {
      const baseContext = [
        'quick', 'brown', 'fox', 'jumped', 'over'
      ];
      const newContext = [
        'quick', 'brown', 'fox', 'jumped', 'far', 'over'
      ];

      const computedAlignment = ContextTracker.attemptTokenizedAlignment(newContext, baseContext);
      assert.deepEqual(computedAlignment, {canAlign: false});
    });
  });

  describe('attemptMatchContext', function() {
    it("properly matches and aligns when lead token is removed", function() {
      let existingContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the doctor"
      });
      let transform: Transform = {
        insert: '',
        deleteLeft: 0
      }
      let newContext = deepCopy(existingContext);
      newContext.left.splice(0, 1);
      let rawTokens = [" ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left, plainModel);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      assert.equal(newContextMatch.headTokensRemoved, 1);
      assert.equal(newContextMatch.tailTokensAdded, 0);
    });

    it("properly matches and aligns when lead token + following whitespace are removed", function() {
      let existingContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the doctor"
      });
      let transform = {
        insert: '',
        deleteLeft: 0
      }
      let newContext = deepCopy(existingContext);
      newContext.left.splice(0, 2);
      let rawTokens = ["apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left, plainModel);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      assert.equal(newContextMatch.headTokensRemoved, 2);
      assert.equal(newContextMatch.tailTokensAdded, 0);
    });

    it("properly matches and aligns when final token is edited", function() {
      let existingContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the docto"
      });
      let transform: Transform = {
        insert: 'r',
        deleteLeft: 0
      }
      let newContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the doctor"
      });
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left, plainModel);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      assert.equal(newContextMatch.headTokensRemoved, 0);
      assert.equal(newContextMatch.tailTokensAdded, 0);
    });

    // Needs improved context-state management (due to 2x tokens)
    it("properly matches and aligns when a 'wordbreak' is added", function() {
      let existingContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the doctor"
      });
      let transform = {
        insert: ' ',
        deleteLeft: 0
      }
      let newContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the doctor "
      });
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left, plainModel);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      // We want to preserve the added whitespace when predicting a token that follows after it.
      assert.deepEqual(newContextMatch.preservationTransform, { insert: ' ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch?.state;
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchSpace.inputSequence);
      assert.isEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchSpace.inputSequence);
      assert.equal(newContextMatch.headTokensRemoved, 0);
      assert.equal(newContextMatch.tailTokensAdded, 2);
    });

    it("properly matches and aligns when a 'wordbreak' is removed via backspace", function() {
      let existingContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the doctor "
      });
      let transform = {
        insert: '',
        deleteLeft: 1
      }
      let newContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the doctor"
      });
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left, plainModel);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isOk(newContextMatch?.state);
      assert.deepEqual(newContextMatch?.state.tokenization.tokens.map(token => token.exampleInput), rawTokens);

      // The 'wordbreak' transform
      assert.equal(newContextMatch.headTokensRemoved, 0);
      assert.equal(newContextMatch.tailTokensAdded, -2);
    });

    it("properly matches and aligns when an implied 'wordbreak' occurs (as when following \"'\")", function() {
      let existingContext = models.tokenize(defaultBreaker, {
        left: "'"
      });
      let transform = {
        insert: 'a',
        deleteLeft: 0
      }
      let newContext = models.tokenize(defaultBreaker, {
        left: "'a"
      });
      let rawTokens = ["'", "a"];

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left, plainModel);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      assert.deepEqual(newContextMatch.preservationTransform, { insert: '', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch.state;
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchSpace.inputSequence);
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchSpace.inputSequence);

      assert.equal(newContextMatch.headTokensRemoved, 0);
      assert.equal(newContextMatch.tailTokensAdded, 1);
    })

    // Needs improved context-state management (due to 2x tokens)
    it("properly matches and aligns when lead token is removed AND a 'wordbreak' is added'", function() {
      let existingContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the doctor"
      });
      let transform = {
        insert: ' ',
        deleteLeft: 0
      }
      let newContext = models.tokenize(defaultBreaker, {
        left: "apple a day keeps the doctor "
      });
      let rawTokens = ["apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left, plainModel);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      // We want to preserve the added whitespace when predicting a token that follows after it.
      assert.deepEqual(newContextMatch.preservationTransform, { insert: ' ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch.state;
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchSpace.inputSequence);
      assert.isEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchSpace.inputSequence);

      assert.equal(newContextMatch.headTokensRemoved, 2);
      assert.equal(newContextMatch.tailTokensAdded, 2);
    });

    it("properly matches and aligns when initial token is modified AND a 'wordbreak' is added'", function() {
      let existingContext = models.tokenize(defaultBreaker, {
        left: "an"
      });
      let transform = {
        insert: 'd ',
        deleteLeft: 0
      }
      let newContext = models.tokenize(defaultBreaker, {
        left: "and "
      });
      let rawTokens = ["and", " ", ""];

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left, plainModel);
      let newContextMatch = ContextTracker.attemptMatchContext(
        newContext.left,
        baseContextMatch,
        tokenizeTransformDistribution(tokenizer, {left: "an", startOfBuffer: true, endOfBuffer: true}, [{sample: transform, p: 1}])
      );
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      // We want to preserve all text preceding the new token when applying a suggestion.
      assert.deepEqual(newContextMatch.preservationTransform, { insert: 'd ', deleteLeft: 0});

      // The 'wordbreak' transform
      let state = newContextMatch.state;
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchSpace.inputSequence);
      assert.isEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchSpace.inputSequence);

      assert.equal(newContextMatch.headTokensRemoved, 0);
      assert.equal(newContextMatch.tailTokensAdded, 2);
    });

    it("properly matches and aligns when tail token is modified AND a 'wordbreak' is added'", function() {
      let existingContext = models.tokenize(defaultBreaker, {
        left: "apple a day keeps the doc"
      });
      let transform = {
        insert: 'tor ',
        deleteLeft: 0
      }
      let newContext = models.tokenize(defaultBreaker, {
        left: "apple a day keeps the doctor "
      });
      let rawTokens = ["apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left, plainModel);
      let newContextMatch = ContextTracker.attemptMatchContext(
        newContext.left,
        baseContextMatch,
        tokenizeTransformDistribution(tokenizer, {left: "apple a day keeps the doc", startOfBuffer: true, endOfBuffer: true}, [{sample: transform, p: 1}])
      );
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      // We want to preserve all text preceding the new token when applying a suggestion.
      assert.deepEqual(newContextMatch.preservationTransform, { insert: 'tor ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch.state;
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchSpace.inputSequence);
      assert.isEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchSpace.inputSequence);

      assert.equal(newContextMatch.headTokensRemoved, 0);
      assert.equal(newContextMatch.tailTokensAdded, 2);
    });

    it('rejects hard-to-handle case: tail token is split into three rather than two', function() {
      let baseContext = models.tokenize(defaultBreaker, {
        left: "text'"
      });
      assert.equal(baseContext.left.length, 1);
      let baseContextMatch = ContextTracker.modelContextState(baseContext.left, plainModel);

      // Now the actual check.
      let newContext = models.tokenize(defaultBreaker, {
        left: "text'\""
      });
      // The reason it's a problem - current internal logic isn't prepared to shift
      // from 1 to 3 tokens in a single step.
      assert.equal(newContext.left.length, 3);

      let transform = {
        insert: '\"',
        deleteLeft: 0
      }
      let problemContextMatch = ContextTracker.attemptMatchContext(
        newContext.left,
        baseContextMatch,
        tokenizeTransformDistribution(tokenizer, {left: "text'", startOfBuffer: true, endOfBuffer: true}, [{sample: transform, p: 1}])
      );
      assert.isNull(problemContextMatch);
    });
  });

  describe('modelContextState', function() {
    it('models without final wordbreak', function() {
      let tokenized = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"].map((entry) => {
        return {
          text: entry,
          isWhitespace: entry == " "
        };
      });
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let state = ContextTracker.modelContextState(tokenized, plainModel);
      assert.deepEqual(state.tokenization.tokens.map(token => token.exampleInput), rawTokens);
    });

    it('models with final wordbreak', function() {
      let tokenized = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""].map((entry) => {
        return {
          text: entry,
          isWhitespace: entry == " "
        };
      });
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let state = ContextTracker.modelContextState(tokenized, plainModel);
      assert.deepEqual(state.tokenization.tokens.map(token => token.exampleInput), rawTokens);
    });
  });

  describe('suggestion acceptance tracking', function() {
    let englishPunctuation = {
      quotesForKeepSuggestion: { open: `“`, close: `”`},
      insertAfterWord: ' '
    };

    // Needs improved context-state management (due to 2x tokens)
    it('tracks an accepted suggestion', function() {
      let baseSuggestion: Suggestion = {
        transform: {
          insert: 'world ',
          deleteLeft: 3,
          id: 0
        },
        transformId: 0,
        id: 1,
        displayAs: 'world'
      };

      let baseContext = {
        left: 'hello wor', startOfBuffer: true, endOfBuffer: true
      };

      // Represents the keystroke that triggered the suggestion.  It's not technically part
      // of the Context when the suggestion is built.
      let postTransform = {
        insert: 'l',
        deleteLeft: 0
      };

      let options = {
        punctuation: englishPunctuation
      };

      let model = new models.TrieModel(jsonFixture('models/tries/english-1000'), options);
      let compositor = new ModelCompositor(model);
      let baseContextMatch = compositor.contextTracker.analyzeState(model, baseContext);

      baseContextMatch.state.tokenization.tail.suggestions = [ baseSuggestion ];

      let reversion = compositor.acceptSuggestion(baseSuggestion, baseContext, postTransform);

      // Actual test assertion - was the replacement tracked?
      assert.equal(baseContextMatch.state.tokenization.tail.appliedSuggestionId, baseSuggestion.id);
      assert.equal(reversion.id, -baseSuggestion.id);

      // Next step - on the followup context, is the replacement still active?
      let postContext = models.applyTransform(baseSuggestion.transform, baseContext);
      let postContextMatch = compositor.contextTracker.analyzeState(model, postContext);

      // Penultimate token corresponds to whitespace, which does not have a 'raw' representation.
      assert.equal(postContextMatch.state.tokenization.tokens[postContextMatch.state.tokenization.tokens.length - 2].exampleInput, ' ');

      // Final token is empty (follows a wordbreak)
      assert.equal(postContextMatch.state.tokenization.tail.exampleInput, '');
    });
  });
});