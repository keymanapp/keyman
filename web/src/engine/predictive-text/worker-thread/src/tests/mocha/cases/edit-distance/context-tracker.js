import { assert } from 'chai';

import { ContextTracker } from '#./correction/context-tracker.js';
import ModelCompositor from '#./model-compositor.js';
import * as models from '#./models/index.js';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { deepCopy } from '@keymanapp/web-utils';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

describe('ContextTracker', function() {
  function toWrapperDistribution(transforms) {
    transforms = Array.isArray(transforms) ? transforms : [transforms];
    return [{
      sample: transforms,
      p: 1.0
    }];
  }

  describe('attemptMatchContext', function() {
    it("properly matches and aligns when lead token is removed", function() {
      let existingContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the doctor"
      });
      let transform = {
        insert: '',
        deleteLeft: 0
      }
      let newContext = deepCopy(existingContext);
      newContext.left.splice(0, 1);
      let rawTokens = [" ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokens.map(token => token.raw), rawTokens);
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

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokens.map(token => token.raw), rawTokens);
    });

    it("properly matches and aligns when final token is edited", function() {
      let existingContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the docto"
      });
      let transform = {
        insert: 'r',
        deleteLeft: 0
      }
      let newContext = models.tokenize(defaultBreaker, {
        left: "an apple a day keeps the doctor"
      });
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokens.map(token => token.raw), rawTokens);
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

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokens.map(token => token.raw), rawTokens);
      // We want to preserve the added whitespace when predicting a token that follows after it.
      assert.deepEqual(newContextMatch.preservationTransform, { insert: ' ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch?.state;
      assert.isNotEmpty(state.tokens[state.tokens.length - 2].transformDistributions);
      assert.isEmpty(state.tokens[state.tokens.length - 1].transformDistributions);
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

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokens.map(token => token.raw), rawTokens);
      assert.deepEqual(newContextMatch.preservationTransform, { insert: '', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch.state;
      assert.isNotEmpty(state.tokens[state.tokens.length - 2].transformDistributions);
      assert.isNotEmpty(state.tokens[state.tokens.length - 1].transformDistributions);
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

      let baseContextMatch = ContextTracker.modelContextState(existingContext.left);
      let newContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.state);
      assert.deepEqual(newContextMatch.state.tokens.map(token => token.raw), rawTokens);
      // We want to preserve the added whitespace when predicting a token that follows after it.
      assert.deepEqual(newContextMatch.preservationTransform, { insert: ' ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch.state;
      assert.isNotEmpty(state.tokens[state.tokens.length - 2].transformDistributions);
      assert.isEmpty(state.tokens[state.tokens.length - 1].transformDistributions);
    });

    it('rejects hard-to-handle case: tail token is split into three rather than two', function() {
      let baseContext = models.tokenize(defaultBreaker, {
        left: "text'"
      });
      assert.equal(baseContext.left.length, 1);
      let baseContextMatch = ContextTracker.modelContextState(baseContext.left);

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
      let problemContextMatch = ContextTracker.attemptMatchContext(newContext.left, baseContextMatch, toWrapperDistribution(transform));
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

      let state = ContextTracker.modelContextState(tokenized);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);
    });

    it('models with final wordbreak', function() {
      let tokenized = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""].map((entry) => {
        return {
          text: entry,
          isWhitespace: entry == " "
        };
      });
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let state = ContextTracker.modelContextState(tokenized);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);
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

      baseContextMatch.state.tail.replacements = [{
        suggestion: baseSuggestion,
        tokenWidth: 1
      }];

      let reversion = compositor.acceptSuggestion(baseSuggestion, baseContext, postTransform);

      // Actual test assertion - was the replacement tracked?
      assert.equal(baseContextMatch.state.tail.activeReplacementId, baseSuggestion.id);
      assert.equal(reversion.id, -baseSuggestion.id);

      // Next step - on the followup context, is the replacement still active?
      let postContext = models.applyTransform(baseSuggestion.transform, baseContext);
      let postContextMatch = compositor.contextTracker.analyzeState(model, postContext);

      // Penultimate token corresponds to whitespace, which does not have a 'raw' representation.
      assert.equal(postContextMatch.state.tokens[postContextMatch.state.tokens.length - 2].raw, ' ');

      // Final token is empty (follows a wordbreak)
      assert.equal(postContextMatch.state.tail.raw, '');
    });
  });
});