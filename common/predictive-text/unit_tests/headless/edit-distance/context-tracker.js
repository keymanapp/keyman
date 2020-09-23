var assert = require('chai').assert;
var ContextTracker = require('../../../build/intermediate/index').correction.ContextTracker;

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
      let newContext = Array.from(existingContext);
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
      let newContext = Array.from(existingContext);
      newContext[newContext.length - 1] = 'doctor';
      let rawTokens = ["an", null, "apple", null, "a", null, "day", null, "keeps", null, "the", null, "doctor"];

      let existingState = ContextTracker.modelContextState(existingContext);
      let state = ContextTracker.attemptMatchContext(newContext, existingState, null, toWrapperDistribution(transform));
      assert.isNotNull(state);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);
    });

    it("properly matches and aligns when a 'wordbreak' is added'", function() {
      let existingContext = ["an", "apple", "a", "day", "keeps", "the", "doctor"];
      let transform = {
        insert: ' ',
        deleteLeft: 0
      }
      let newContext = Array.from(existingContext);
      newContext.push('');
      let rawTokens = ["an", null, "apple", null, "a", null, "day", null, "keeps", null, "the", null, "doctor", null, ""];

      let existingState = ContextTracker.modelContextState(existingContext);
      let state = ContextTracker.attemptMatchContext(newContext, existingState, null, toWrapperDistribution(transform));
      assert.isNotNull(state);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);

      // The 'wordbreak' transform
      assert.isNotEmpty(state.tokens[state.tokens.length - 2].transformDistributions);
      assert.isEmpty(state.tokens[state.tokens.length - 1].transformDistributions);
    });

    it("properly matches and aligns when lead token is removed AND a 'wordbreak' is added'", function() {
      let existingContext = ["an", "apple", "a", "day", "keeps", "the", "doctor"];
      let transform = {
        insert: ' ',
        deleteLeft: 0
      }
      let newContext = Array.from(existingContext);
      newContext.splice(0, 1);
      newContext.push('');
      let rawTokens = ["apple", null, "a", null, "day", null, "keeps", null, "the", null, "doctor", null, ""];

      let existingState = ContextTracker.modelContextState(existingContext);
      let state = ContextTracker.attemptMatchContext(newContext, existingState, null, toWrapperDistribution(transform));
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
});