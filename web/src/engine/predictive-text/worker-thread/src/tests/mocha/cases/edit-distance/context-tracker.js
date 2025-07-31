import { assert } from 'chai';

import { ContextTracker } from '#./correction/context-tracker.js';
import { tokenizeTransformDistribution } from '#./correction/transform-tokenization.js';
import ModelCompositor from '#./model-compositor.js';
import * as models from '#./models/index.js';
import * as wordBreakers from '@keymanapp/models-wordbreakers';
import { determineModelTokenizer } from '#./model-helpers.js';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { deepCopy } from '@keymanapp/web-utils';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { ContextState } from '#./correction/context-state.js';

const tokenizer = determineModelTokenizer(new models.DummyModel({wordbreaker: defaultBreaker}));

var TrieModel = models.TrieModel;

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: wordBreakers.default});

describe('ContextTracker', function() {
  function toWrapperDistribution(transform) {
    return [{
      sample: transform,
      p: 1.0
    }];
  }

  describe('attemptMatchContext', function() {
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
      baseState.initFromReset();
      let newContextMatch = ContextTracker.attemptMatchContext(newContext, plainModel, baseState, toWrapperDistribution(transform));
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
      baseState.initFromReset();
      let newContextMatch = ContextTracker.attemptMatchContext(newContext, plainModel, baseState, toWrapperDistribution(transform));
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
      let newContext = {
        left: "an apple a day keeps the doctor"
      };
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseState = new ContextState(existingContext, plainModel);
      baseState.initFromReset();
      let newContextMatch = ContextTracker.attemptMatchContext(newContext, plainModel, baseState, toWrapperDistribution(transform));
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
      let newContext = {
        left: "an apple a day keeps the doctor "
      };
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      baseState.initFromReset();
      let newContextMatch = ContextTracker.attemptMatchContext(newContext, plainModel, baseState, toWrapperDistribution(transform));
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
      let newContext = {
        left: "an apple a day keeps the doctor"
      };
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseState = new ContextState(existingContext, plainModel);
      baseState.initFromReset();
      let newContextMatch = ContextTracker.attemptMatchContext(newContext, plainModel, baseState, toWrapperDistribution(transform));
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
      let newContext = {
        left: "'a"
      };
      let rawTokens = ["'", "a"];

      let baseState = new ContextState(existingContext, plainModel);
      baseState.initFromReset();
      let newContextMatch = ContextTracker.attemptMatchContext(newContext, plainModel, baseState, toWrapperDistribution(transform));
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
        left: "apple a day keeps the doctor "
      };
      let rawTokens = ["apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      baseState.initFromReset();
      let newContextMatch = ContextTracker.attemptMatchContext(newContext, plainModel, baseState, toWrapperDistribution(transform));
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
      let newContext = {
        left: "and "
      };
      let rawTokens = ["and", " ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      baseState.initFromReset();
      let newContextMatch = ContextTracker.attemptMatchContext(
        newContext,
        plainModel,
        baseState,
        [{sample: transform, p: 1}]
      );
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
      let newContext = {
        left: "apple a day keeps the doctor "
      };
      let rawTokens = ["apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      baseState.initFromReset();
      let newContextMatch = ContextTracker.attemptMatchContext(
        newContext,
        plainModel,
        baseState,
        [{sample: transform, p: 1}]
      );
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
      baseState.initFromReset();

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
        {left: "text'\""},
        plainModel,
        baseState,
        [{sample: transform, p: 1}]
      );
      assert.isNull(problemContextMatch);
    });
  });

  describe('modelContextState', function() {
    it('models without final wordbreak', function() {
      let context = { left: "an apple a day keeps the doctor" };
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let state = new ContextState(context, plainModel);
      state.initFromReset();
      assert.deepEqual(state.tokenization.tokens.map(token => token.exampleInput), rawTokens);
    });

    it('models with final wordbreak', function() {
      let context = { left: "an apple a day keeps the doctor " };
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let state = new ContextState(context, plainModel);
      state.initFromReset();
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
      let baseSuggestion = {
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

      baseContextMatch.final.tokenization.tail.replacements = [{
        suggestion: baseSuggestion,
        tokenWidth: 1
      }];

      let reversion = compositor.acceptSuggestion(baseSuggestion, baseContext, postTransform);

      // Actual test assertion - was the replacement tracked?
      assert.equal(baseContextMatch.final.tokenization.tail.appliedSuggestionId, baseSuggestion.id);
      assert.equal(reversion.id, -baseSuggestion.id);

      // Next step - on the followup context, is the replacement still active?
      let postContext = models.applyTransform(baseSuggestion.transform, baseContext);
      let postContextMatch = compositor.contextTracker.analyzeState(model, postContext);

      // Penultimate token corresponds to whitespace, which does not have a 'raw' representation.
      assert.equal(postContextMatch.final.tokenization.tokens[postContextMatch.final.tokenization.tokens.length - 2].exampleInput, ' ');

      // Final token is empty (follows a wordbreak)
      assert.equal(postContextMatch.final.tokenization.tail.exampleInput, '');
    });
  });
});