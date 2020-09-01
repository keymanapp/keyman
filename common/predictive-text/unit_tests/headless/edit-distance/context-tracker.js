var assert = require('chai').assert;
var ClassicalDistanceCalculation = require('../../../build/intermediate').correction.ClassicalDistanceCalculation;
var ContextTracker = require('../../../build/intermediate/index').correction.ContextTracker;

describe('ContextTracker', function() {
  describe.only('attemptMatchContext', function() {
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
      let state = ContextTracker.attemptMatchContext(newContext, existingState, transform);
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
      let rawTokens = ["an", null, "apple", null, "a", null, "day", null, "keeps", null, "the", null, "doctor"];

      let existingState = ContextTracker.modelContextState(existingContext);
      let state = ContextTracker.attemptMatchContext(newContext, existingState, transform);
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
      let state = ContextTracker.attemptMatchContext(newContext, existingState, transform);
      assert.isNotNull(state);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);

      // The 'wordbreak' transform
      assert.isNotEmpty(state.tokens[state.tokens.length - 2].transforms);
      assert.isEmpty(state.tokens[state.tokens.length - 1].transforms);
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
      let state = ContextTracker.attemptMatchContext(newContext, existingState, transform);
      assert.isNotNull(state);
      assert.deepEqual(state.tokens.map(token => token.raw), rawTokens);

      // The 'wordbreak' transform
      assert.isNotEmpty(state.tokens[state.tokens.length - 2].transforms);
      assert.isEmpty(state.tokens[state.tokens.length - 1].transforms);
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

  // describe('context alignment', function() {
  //   it("scratchspace 1", function() {
  //     let context1 = ["an", "apple", "a", "day", "keeps", "the", "doctor"];
  //     let context2 = ["apple", "a", "day", "keeps", "the", "doctor", "away"];

  //     let buffer = ClassicalDistanceCalculation.computeDistance(context1.map(value => ({key: value})), context2.map(value => ({key: value})), 1);

  //     console.log('-------------------');
  //     console.log(context1);
  //     console.log('->');
  //     console.log(context2);
  //     console.log();
  //     console.log("Distance: " + buffer.getHeuristicFinalCost());
  //     console.log(buffer.editPath());
  //     console.log();
  //   });

  //   it("scratchspace 2", function() {
  //     let context1 = ["an", "apple", "a", "day", "keeps", "the", "doctor"];
  //     let context2 = ["apple", "a", "day", "keeps", "the", "doctors"];

  //     let buffer = ClassicalDistanceCalculation.computeDistance(context1.map(value => ({key: value})), context2.map(value => ({key: value})), 1);

  //     console.log('-------------------');
  //     console.log(context1);
  //     console.log('->');
  //     console.log(context2);
  //     console.log();
  //     console.log("Distance: " + buffer.getHeuristicFinalCost());
  //     console.log(buffer.editPath());
  //     console.log();
  //   });
  // });
});