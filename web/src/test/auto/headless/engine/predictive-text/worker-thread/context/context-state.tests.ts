/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-08-01
 *
 * This file tests designed to validate the behavior of ContextState class and
 * its integration with the lower-level classes that it utilizes.
 */

import { assert } from 'chai';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { ContextState, determineContextSlideTransform, models, SearchQuotientSpur } from '@keymanapp/lm-worker/test-index';

import Context = LexicalModelTypes.Context;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

function toWrapperDistribution(transform: Transform) {
  return [{
    sample: transform,
    p: 1.0
  }];
}

describe('ContextState', () => {
  it('<constructor>', () => {
    let context = { left: '', right: '', startOfBuffer: true, endOfBuffer: true };
    let state = new ContextState(context, plainModel);

    assert.equal(state.context, context);
    assert.equal(state.model, plainModel);
    assert.isOk(state.tokenization);
    assert.isUndefined(state.isManuallyApplied);
    assert.isNotOk(state.suggestions);
    assert.isNotOk(state.appliedSuggestionId);
  });

  describe('initializing without prior tokenization', () => {
    it('creates one empty token for an empty context', () => {
      let context = { left: '', right: '', startOfBuffer: true, endOfBuffer: true };
      let state = new ContextState(context, plainModel);
      assert.isOk(state.tokenization);
      assert.equal(state.tokenization.tokens.length, 1);
      assert.equal(state.tokenization.tail.exampleInput, '');
    });

    it('creates tokens for initial text (without ending whitespace)', () => {
      let context = { left: 'the quick brown fox', right: '', startOfBuffer: true, endOfBuffer: true };
      let state = new ContextState(context, plainModel);
      assert.isOk(state.tokenization);
      assert.equal(state.tokenization.tokens.length, 7);
      assert.deepEqual(state.tokenization.exampleInput, ['the', ' ', 'quick', ' ', 'brown', ' ', 'fox']);

      let context2 = { left: "an apple a day keeps the doctor", startOfBuffer: true, endOfBuffer: true };
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];
      let state2 = new ContextState(context2, plainModel);
      assert.deepEqual(state2.tokenization.tokens.map(token => token.exampleInput), rawTokens);
    });

    it('creates tokens for initial text (with extra empty token for ending whitespace)', () => {
      let context = { left: 'the quick brown fox ', right: '', startOfBuffer: true, endOfBuffer: true };
      let state = new ContextState(context, plainModel);
      assert.isOk(state.tokenization);
      assert.equal(state.tokenization.tokens.length, 9);
      assert.deepEqual(state.tokenization.exampleInput, ['the', ' ', 'quick', ' ', 'brown', ' ', 'fox', ' ', '']);

      let context2 = { left: "an apple a day keeps the doctor ", startOfBuffer: true, endOfBuffer: true };
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let state2 = new ContextState(context2, plainModel);
      assert.deepEqual(state2.tokenization.tokens.map(token => token.exampleInput), rawTokens);
    });
  });


  describe('analyzeTransition', function() {
    it("properly matches and aligns when no context changes occur (end of word)", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor", startOfBuffer: true, endOfBuffer: true
      };
      let transform = {
        insert: '',
        deleteLeft: 0
      };
      let newContext = {
        left: "an apple a day keeps the doctor", startOfBuffer: true, endOfBuffer: true
      };
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(newContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);

      // // Phrased this way to facilitate TS type-inference; assert.isTrue() does
      // // NOT do this for us!
      // if(!newContextMatch.final.tokenization.alignment.canAlign) {
      //   assert.fail("context alignment failed");
      // }
      // assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, 0);
      // assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 0);
    });

    it("properly matches and aligns when no context changes occur (after whitespace)", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor ", startOfBuffer: true, endOfBuffer: true
      };
      let transform = {
        insert: '',
        deleteLeft: 0
      };
      let newContext = {
        left: "an apple a day keeps the doctor ", startOfBuffer: true, endOfBuffer: true
      };
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(newContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);

      // // Phrased this way to facilitate TS type-inference; assert.isTrue() does
      // // NOT do this for us!
      // if(!newContextMatch.final.tokenization.alignment.canAlign) {
      //   assert.fail("context alignment failed");
      // }
      // assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, 0);
      // assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 0);
    });

    it("properly matches and aligns when lead token is removed (end of word)", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor", startOfBuffer: true, endOfBuffer: true
      };
      let transform = {
        insert: '',
        deleteLeft: 0
      };
      let newContext = {
        left: " apple a day keeps the doctor", startOfBuffer: false, endOfBuffer: true
      };
      let rawTokens = [" ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(newContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);

      // if(!newContextMatch.final.tokenization.alignment.canAlign) {
      //   assert.fail("context alignment failed");
      // }
      // assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, -1);
      // assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 0);
    });

    it("properly matches and aligns when lead token is removed (after whitespace)", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor ", startOfBuffer: true, endOfBuffer: true
      };
      let transform = {
        insert: '',
        deleteLeft: 0
      };
      let newContext = {
        left: " apple a day keeps the doctor ", startOfBuffer: false, endOfBuffer: true
      };
      let rawTokens = [" ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(newContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);

      // if(!newContextMatch.final.tokenization.alignment.canAlign) {
      //   assert.fail("context alignment failed");
      // }
      // assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, -1);
      // assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 0);
    });

    it("properly matches and aligns when lead token + following whitespace are removed", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor", startOfBuffer: true, endOfBuffer: true
      };
      let transform = {
        insert: '',
        deleteLeft: 0
      };
      let newContext = {
        left: "apple a day keeps the doctor", startOfBuffer: false, endOfBuffer: true
      };
      let rawTokens = ["apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor"];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(newContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);

      // if(!newContextMatch.final.tokenization.alignment.canAlign) {
      //   assert.fail("context alignment failed");
      // }
      // assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, -2);
      // assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 0);
    });

    it("properly matches and aligns when final token is edited", function() {
      let existingContext = {
        left: "an apple a day keeps the docto", startOfBuffer: true, endOfBuffer: true
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

      // if(!newContextMatch.final.tokenization.alignment.canAlign) {
      //   assert.fail("context alignment failed");
      // }
      // assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, 0);
      // assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 0);
    });

    // Needs improved context-state management (due to 2x tokens)
    it("properly matches and aligns when a 'wordbreak' is added", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor", startOfBuffer: true, endOfBuffer: true
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
      assert.deepEqual(newContextMatch.final.tokenization.taillessTrueKeystroke, { insert: ' ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch?.final;
      // space transform
      assert.equal(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchModule.inputCount, 1);
      // empty transform
      assert.equal(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchModule.inputCount, 1);
      assert.isTrue(state.tokenization.tail.searchModule instanceof SearchQuotientSpur);
      assert.deepEqual((state.tokenization.tail.searchModule as SearchQuotientSpur).lastInput, [{sample: { insert: '', deleteLeft: 0 }, p: 1}]);
    });

    it("properly matches and aligns when whitespace before final empty token is extended", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor ", startOfBuffer: true, endOfBuffer: true
      };
      let transform = {
        insert: ' ',
        deleteLeft: 0
      }
      let rawTokens = ["an", " ", "apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", "  ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(existingContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      // We want to preserve the added whitespace when predicting a token that follows after it.
      assert.deepEqual(newContextMatch.final.tokenization.taillessTrueKeystroke, { insert: ' ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch?.final;
      // Two whitespaces, one of which is new!
      const preTail = state.tokenization.tokens[state.tokenization.tokens.length - 2];
      assert.equal(preTail.searchModule.inputCount, 2);
      assert.deepEqual((preTail.searchModule.parents[0] as SearchQuotientSpur).lastInput, [{sample: transform, p: 1}]);
      assert.equal(state.tokenization.tail.searchModule.inputCount, 1);
      assert.isTrue(state.tokenization.tail.searchModule instanceof SearchQuotientSpur);
      assert.deepEqual((state.tokenization.tail.searchModule as SearchQuotientSpur).lastInput, [{sample: { insert: '', deleteLeft: 0 }, p: 1}]);
    });

    it("properly matches and aligns when a 'wordbreak' is removed via backspace", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor ", startOfBuffer: true, endOfBuffer: true
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
    });

    it("properly matches and aligns when an implied 'wordbreak' occurs (as when following \"'\")", function() {
      let existingContext = {
        left: "'", startOfBuffer: true, endOfBuffer: true
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
      assert.deepEqual(newContextMatch.final.tokenization.taillessTrueKeystroke, { insert: '', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch.final;
      assert.equal(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchModule.inputCount, 1);
      assert.equal(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchModule.inputCount, 1);
    })

    // Needs improved context-state management (due to 2x tokens)
    it("properly matches and aligns when lead token is removed AND a 'wordbreak' is added'", function() {
      let existingContext = {
        left: "an apple a day keeps the doctor", startOfBuffer: true, endOfBuffer: true
      };
      let transform = {
        insert: ' ',
        deleteLeft: 0
      }
      let newContext = {
        left: "apple a day keeps the doctor", startOfBuffer: false, endOfBuffer: true
      };
      let rawTokens = ["apple", " ", "a", " ", "day", " ", "keeps", " ", "the", " ", "doctor", " ", ""];

      let baseState = new ContextState(existingContext, plainModel);
      let newContextMatch = baseState.analyzeTransition(newContext, toWrapperDistribution(transform));
      assert.isNotNull(newContextMatch?.final);
      assert.deepEqual(newContextMatch.final.tokenization.tokens.map(token => token.exampleInput), rawTokens);
      // We want to preserve the added whitespace when predicting a token that follows after it.
      assert.deepEqual(newContextMatch.final.tokenization.taillessTrueKeystroke, { insert: ' ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch.final;
      assert.equal(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchModule.inputCount, 1);
      assert.equal(
        state.tokenization.tokens[state.tokenization.tokens.length - 1].searchModule.inputCount, 1
      );

      // if(!newContextMatch.final.tokenization.alignment.canAlign) {
      //   assert.fail("context alignment failed");
      // }
      // assert.equal(newContextMatch.final.tokenization.alignment.leadTokenShift, -2);
      // assert.equal(newContextMatch.final.tokenization.alignment.tailTokenShift, 2);
    });

    it("properly matches and aligns when initial token is modified AND a 'wordbreak' is added'", function() {
      let existingContext = {
        left: "an", startOfBuffer: true, endOfBuffer: true
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
      assert.deepEqual(newContextMatch.final.tokenization.taillessTrueKeystroke, { insert: 'd ', deleteLeft: 0});

      // The 'wordbreak' transform
      let state = newContextMatch.final;
      assert.equal(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchModule.inputCount, 1);
      assert.equal(
        state.tokenization.tokens[state.tokenization.tokens.length - 1].searchModule.inputCount, 1
      );
    });

    it("properly matches and aligns when tail token is modified AND a 'wordbreak' is added'", function() {
      let existingContext = {
        left: "apple a day keeps the doc", startOfBuffer: true, endOfBuffer: true
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
      assert.deepEqual(newContextMatch.final.tokenization.taillessTrueKeystroke, { insert: 'tor ', deleteLeft: 0 });

      // The 'wordbreak' transform
      let state = newContextMatch.final;
      assert.equal(state.tokenization.tokens[state.tokenization.tokens.length - 2].searchModule.inputCount, 1);
      assert.equal(state.tokenization.tokens[state.tokenization.tokens.length - 1].searchModule.inputCount, 1);
    });

    it('handles case where tail token is split into three rather than two', function() {
      let baseContext = models.tokenize(defaultBreaker, {
        left: "text'", startOfBuffer: true, endOfBuffer: true
      });
      assert.equal(baseContext.left.length, 1);

      let baseState = new ContextState({ left: "text'", startOfBuffer: true, endOfBuffer: true }, plainModel);

      // Now the actual check.
      let newContext = models.tokenize(defaultBreaker, {
        left: "text'\"", startOfBuffer: true, endOfBuffer: true
      });
      // The reason it's a problem - current internal logic isn't prepared to shift
      // from 1 to 3 tokens in a single step.
      assert.equal(newContext.left.length, 3);

      let transform = {
        insert: '\"',
        deleteLeft: 0
      }
      let problemContextMatch = baseState.analyzeTransition({left: "text'", startOfBuffer: true, endOfBuffer: true}, [{sample: transform, p: 1}]);
      assert.isNotNull(problemContextMatch);

      assert.deepEqual(problemContextMatch.final.tokenization.exampleInput, ['text', '\'', '"']);
    });
  });
});

describe('determineContextSlideDeltas', () => {
  it('finds prefixed text for backward sliding context window', () => {
    const before: Context = {
      left: 'ples and bananas',
      startOfBuffer: false,
      endOfBuffer: false
    };

    const after: Context = {
      left: 'apples and bananas',
      startOfBuffer: false,
      endOfBuffer: false
    }

    const results = determineContextSlideTransform(before, after);
    assert.deepEqual(results, {insert: 'ap', deleteLeft: 0, deleteRight: 0});
  });

  it('finds prefixed text for large backward sliding context window jump', () => {
    const before: Context = {
      left: 'nanas',
      startOfBuffer: false,
      endOfBuffer: false
    };

    const after: Context = {
      left: 'apples and bananas',
      startOfBuffer: false,
      endOfBuffer: false
    }

    const results = determineContextSlideTransform(before, after);
    assert.deepEqual(results, {insert: 'apples and ba', deleteLeft: 0, deleteRight: 0});
  });

  it('properly handles cases with unaltered context', () => {
    const before: Context = {
      left: 'apples and bananas',
      startOfBuffer: false,
      endOfBuffer: false
    };

    const after: Context = {
      left: 'apples and bananas',
      startOfBuffer: false,
      endOfBuffer: false
    }

    const results = determineContextSlideTransform(before, after);
    assert.deepEqual(results, {insert: '', deleteLeft: 0, deleteRight: 0});
  });

  it('computes dropped-char count for forward sliding context window', () => {
    const before: Context = {
      left: 'apples and bananas',
      startOfBuffer: false,
      endOfBuffer: false
    };

    const after: Context = {
      left: 'ples and bananas',
      startOfBuffer: false,
      endOfBuffer: false
    }

    const results = determineContextSlideTransform(before, after);
    assert.deepEqual(results, {insert: '', deleteLeft: 0, deleteRight: 2});
  });

  it('computes dropped-char count for large forward sliding context window jump', () => {
    const before: Context = {
      left: 'apples and bananas',
      startOfBuffer: false,
      endOfBuffer: false
    };

    const after: Context = {
      left: 'nanas',
      startOfBuffer: false,
      endOfBuffer: false
    }

    const results = determineContextSlideTransform(before, after);
    assert.deepEqual(results, {insert: '', deleteLeft: 0, deleteRight: 'apples and ba'.length});
  });
});