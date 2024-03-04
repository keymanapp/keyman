import { assert } from 'chai';

import { ContextTracker } from '#./correction/context-tracker.js';
import ModelCompositor from '#./model-compositor.js';
import * as models from '#./models/index.js';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

describe('ContextTracker', function() {
  function toWrapperDistribution(transform) {
    return [{
      sample: transform,
      p: 1.0
    }];
  }

  describe('attemptMatchContext', function() {
    it("properly matches and aligns when lead token is removed", function() {
      let existingContext = ["an", "apple", "a", "day", "keeps", "the", "doctor"];
      let transform = {
        insert: '',
        deleteLeft: 0
      }
      let newContext = existingContext.slice(0);
      newContext.splice(0, 1);
      let rawTokens = ["apple", null, "a", null, "day", null, "keeps", null, "the", null, "doctor"];

      let existingState = ContextTracker.modelContextState(existingContext);
      let state = ContextTracker.attemptMatchContext(newContext, existingState, null, toWrapperDistribution(transform));
      assert.isNotNull(state);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);
    });

    it("properly matches and aligns when final token is edited", function() {
      let existingContext = ["an", "apple", "a", "day", "keeps", "the", "docto"];
      let transform = {
        insert: 'r',
        deleteLeft: 0
      }
      let newContext = existingContext.slice(0);
      newContext[newContext.length - 1] = 'doctor';
      let rawTokens = ["an", null, "apple", null, "a", null, "day", null, "keeps", null, "the", null, "doctor"];

      let existingState = ContextTracker.modelContextState(existingContext);
      let state = ContextTracker.attemptMatchContext(newContext, existingState, null, toWrapperDistribution(transform));
      assert.isNotNull(state);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);
    });

    it("properly matches and aligns when a 'wordbreak' is added", function() {
      let existingContext = ["an", "apple", "a", "day", "keeps", "the", "doctor"];
      let transform = {
        insert: ' ',
        deleteLeft: 0
      }
      let newContext = existingContext.slice(0);
      newContext.push('');
      let rawTokens = ["an", null, "apple", null, "a", null, "day", null, "keeps", null, "the", null, "doctor", null, ""];

      let existingState = ContextTracker.modelContextState(existingContext);
      let state = ContextTracker.attemptMatchContext(newContext, existingState, toWrapperDistribution(transform));
      assert.isNotNull(state);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);

      // The 'wordbreak' transform
      assert.isNotEmpty(state.tokens[state.tokens.length - 2].transformDistributions);
      assert.isEmpty(state.tokens[state.tokens.length - 1].transformDistributions);
    });

    it("properly matches and aligns when an implied 'wordbreak' occurs (as when following \"'\")", function() {
      let existingContext = ["'"];
      let transform = {
        insert: 'a',
        deleteLeft: 0
      }
      let newContext = existingContext.slice(0);
      newContext.push('a'); // The incoming transform should produce a new token WITH TEXT.
      let rawTokens = ["'", null, "a"];

      let existingState = ContextTracker.modelContextState(existingContext);
      let state = ContextTracker.attemptMatchContext(newContext, existingState, toWrapperDistribution(transform));
      assert.isNotNull(state);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);

      // The 'wordbreak' transform
      assert.isEmpty(state.tokens[state.tokens.length - 2].transformDistributions);
      assert.isNotEmpty(state.tokens[state.tokens.length - 1].transformDistributions);
    });

    it("properly matches and aligns when lead token is removed AND a 'wordbreak' is added'", function() {
      let existingContext = ["an", "apple", "a", "day", "keeps", "the", "doctor"];
      let transform = {
        insert: ' ',
        deleteLeft: 0
      }
      let newContext = existingContext.slice(0);
      newContext.splice(0, 1);
      newContext.push('');
      let rawTokens = ["apple", null, "a", null, "day", null, "keeps", null, "the", null, "doctor", null, ""];

      let existingState = ContextTracker.modelContextState(existingContext);
      let state = ContextTracker.attemptMatchContext(newContext, existingState, toWrapperDistribution(transform));
      assert.isNotNull(state);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);

      // The 'wordbreak' transform
      assert.isNotEmpty(state.tokens[state.tokens.length - 2].transformDistributions);
      assert.isEmpty(state.tokens[state.tokens.length - 1].transformDistributions);
    });
  });

  describe('modelContextState', function() {
    it('models without final wordbreak', function() {
      let context = ["an", "apple", "a", "day", "keeps", "the", "doctor"];
      let rawTokens = ["an", null, "apple", null, "a", null, "day", null, "keeps", null, "the", null, "doctor"];

      let state = ContextTracker.modelContextState(context);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);
    });

    it('models with final wordbreak', function() {
      let context = ["an", "apple", "a", "day", "keeps", "the", "doctor", ""];
      let rawTokens = ["an", null, "apple", null, "a", null, "day", null, "keeps", null, "the", null, "doctor", null, ""];

      let state = ContextTracker.modelContextState(context);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);
    });
  });

  describe('suggestion acceptance tracking', function() {
    let englishPunctuation = {
      quotesForKeepSuggestion: { open: `“`, close: `”`},
      insertAfterWord: ' '
    };

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
      let baseContextState = compositor.contextTracker.analyzeState(model, baseContext);

      baseContextState.tail.replacements = [{
        suggestion: baseSuggestion,
        tokenWidth: 1
      }];

      let reversion = compositor.acceptSuggestion(baseSuggestion, baseContext, postTransform);

      // Actual test assertion - was the replacement tracked?
      assert.equal(baseContextState.tail.activeReplacementId, baseSuggestion.id);
      assert.equal(reversion.id, -baseSuggestion.id);

      // Next step - on the followup context, is the replacement still active?
      let postContext = models.applyTransform(baseSuggestion.transform, baseContext);
      let postContextState = compositor.contextTracker.analyzeState(model, postContext);

      // Penultimate token corresponds to whitespace, which does not have a 'raw' representation.
      assert.isNull(postContextState.tokens[postContextState.tokens.length - 2].raw);

      // Final token is empty (follows a wordbreak)
      assert.equal(postContextState.tail.raw, '');
    });
  });
});