import { assert } from 'chai';

import { ContextState } from '#./correction/context-state.js';
import * as models from '#./models/index.js';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

var TrieModel = models.TrieModel;

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

function toWrapperDistribution(transform) {
  return [{
    sample: transform,
    p: 1.0
  }];
}

describe('ContextState', () => {
  it('<constructor>', () => {
    let context = { left: '', right: '' };
    let state = new ContextState(context, plainModel);

    assert.equal(state.context, context);
    assert.equal(state.model, plainModel);
    assert.isOk(state.tokenization);
    assert.isUndefined(state.isManuallyApplied);
    assert.isNotOk(state.suggestions);
    assert.isNotOk(state.appliedSuggestionId);
  });

  describe('initializing without prior tokenization', () => {
    it('<empty context>', () => {
      let context = { left: '', right: '' };
      let state = new ContextState(context, plainModel);
      assert.isOk(state.tokenization);
      assert.equal(state.tokenization.tokens.length, 1);
      assert.equal(state.tokenization.tail.exampleInput, '');
    });

    it('with initial text (without ending whitespace)', () => {
      let context = { left: 'the quick brown fox', right: '' };
      let state = new ContextState(context, plainModel);
      assert.isOk(state.tokenization);
      assert.equal(state.tokenization.tokens.length, 7);
      assert.deepEqual(state.tokenization.exampleInput, ['the', ' ', 'quick', ' ', 'brown', ' ', 'fox']);

      let context2 = { left: "an apple a day keeps the doctor" };
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];
      let state2 = new ContextState(context2, plainModel);
      assert.deepEqual(state2.tokenization.tokens.map(token => token.exampleInput), rawTokens);
    });

    it('with initial text (with ending whitespace', () => {
      let context = { left: 'the quick brown fox ', right: '' };
      let state = new ContextState(context, plainModel);
      assert.isOk(state.tokenization);
      assert.equal(state.tokenization.tokens.length, 9);
      assert.deepEqual(state.tokenization.exampleInput, ['the', ' ', 'quick', ' ', 'brown', ' ', 'fox', ' ', '']);

      let context2 = { left: "an apple a day keeps the doctor " };
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let state2 = new ContextState(context2, plainModel);
      assert.deepEqual(state2.tokenization.tokens.map(token => token.exampleInput), rawTokens);
    });
  });


  describe('analyzeTransition', function() {
    it("properly matches and aligns when lead token is removed", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor"
      };
      let transform = {
        insert: '',
        deleteLeft: 0
      };
      let newContext = {
        left: " apple a day keeps the doctor"
      };
      let rawTokens = [" ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(newContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, -1);
      assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 0);
    });

    it("properly matches and aligns when lead token + following whitespace are removed", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor"
      };
      let transform = {
        insert: '',
        deleteLeft: 0
      };
      let newContext = {
        left: "apple a day keeps the doctor"
      };
      let rawTokens = ["apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(newContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, -2);
      assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 0);
    });

    it("properly matches and aligns when final token is edited", function() {
      let existingContext = {
        left: "an apple a day keeps the docto"
      };
      let transform = {
        insert: 'r',
        deleteLeft: 0
      }
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(existingContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, 0);
      assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 0);
    });

    // Needs improved context-state management (due to 2x tokens)
    it("properly matches and aligns when a 'wordbreak' is added", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor"
      };
      let transform = {
        insert: ' ',
        deleteLeft: 0
      }
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(existingContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      // We want to preserve the added whitespace when predicting a token that follows after it.
      assert.deepEqual(newContextMatch.preservationTransform, { insert: ' ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch?.final;
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchSpace.inputSequence);
      assert.isEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchSpace.inputSequence);
      assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, 0);
      assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 2);
    });

    it("properly matches and aligns when a 'wordbreak' is removed via backspace", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor "
      };
      let transform = {
        insert: '',
        deleteLeft: 1
      }
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(existingContext, toWrapperDistribution(transform));
      assert.isOk(newContextMatch?.final);
      assert.deepEqual(newContextMatch?.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);

      // The 'wordbreak' transform
      assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, 0);
      assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, -2);
    });

    it("properly matches and aligns when an implied 'wordbreak' occurs (as when following \"'\")", function() {
      let existingContext = {
        left: "'"
      };
      let transform = {
        insert: 'a',
        deleteLeft: 0
      }
      let rawTokens = ["'", "a"];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(existingContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      assert.deepEqual(newContextMatch.preservationTransform, { insert: '', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch.final;
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchSpace.inputSequence);
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchSpace.inputSequence);

      assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, 0);
      assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 1);
    })

    // Needs improved context-state management (due to 2x tokens)
    it("properly matches and aligns when lead token is removed AND a 'wordbreak' is added'", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor"
      };
      let transform = {
        insert: ' ',
        deleteLeft: 0
      }
      let newContext = {
        left: "apple a day keeps the doctor"
      };
      let rawTokens = ["apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(newContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      // We want to preserve the added whitespace when predicting a token that follows after it.
      assert.deepEqual(newContextMatch.preservationTransform, { insert: ' ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch.final;
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchSpace.inputSequence);
      assert.isEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchSpace.inputSequence);

      assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, -2);
      assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 2);
    });

    it("properly matches and aligns when initial token is modified AND a 'wordbreak' is added'", function() {
      let existingContext = {
        left: "an"
      };
      let transform = {
        insert: 'd ',
        deleteLeft: 0
      }
      let rawTokens = ["and", " ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(existingContext, [{sample: transform, p: 1}]);
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      // We want to preserve all text preceding the new token when applying a suggestion.
      assert.deepEqual(newContextMatch.preservationTransform, { insert: 'd ', deleteLeft: 0});

      // The 'wordbreak' transform
      let state = newContextMatch.final;
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchSpace.inputSequence);
      assert.isEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchSpace.inputSequence);

      assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, 0);
      assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 2);
    });

    it("properly matches and aligns when tail token is modified AND a 'wordbreak' is added'", function() {
      let existingContext = {
        left: "apple a day keeps the doc"
      };
      let transform = {
        insert: 'tor ',
        deleteLeft: 0
      }
      let rawTokens = ["apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(existingContext, [{sample: transform, p: 1}]);
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      // We want to preserve all text preceding the new token when applying a suggestion.
      assert.deepEqual(newContextMatch.preservationTransform, { insert: 'tor ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch.final;
      assert.isNotEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchSpace.inputSequence);
      assert.isEmpty(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchSpace.inputSequence);

      assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, 0);
      assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 2);
    });

    it('rejects hard-to-handle case: tail token is split into three rather than two', function() {
      let baseContext = models.tokenize(defaultBreaker, {
        left: "text'"
      });
      assert.equal(baseContext.left.length, 1);

      let baseState = new ContextState({ left: "text'" }, plainModel);

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
      let problemContextMatch = baseState.analyzeTransition({left: "text'"}, [{sample: transform, p: 1}]);
      assert.isNull(problemContextMatch);
    });
  });
});