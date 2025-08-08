import { assert } from 'chai';

import { ContextTracker } from '#./correction/context-tracker.js';
import { ContextTransition } from '#./correction/context-transition.js';
import { ContextState } from '#./correction/context-state.js';
import * as models from '#./models/index.js';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

var TrieModel = models.TrieModel;

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

function assertClonedStateMatch(a, b) {
  assert.notEqual(a, b);
  assert.notEqual(a.tokenization, b.tokenization);
  assert.notSameOrderedMembers(a.tokenization.tokens, b.tokenization.tokens);
  assert.sameOrderedMembers(a.tokenization.exampleInput, b.tokenization.exampleInput);

  assert.deepEqual(a.suggestions, b.suggestions);
}

function assertClonedTransitionMatch(a, b) {
  assert.notEqual(a, b);

  assertClonedStateMatch(a.base, b.base);
  assertClonedStateMatch(a.final, b.final);
}

describe('ContextTransition', () => {
  describe('<constructor>', () => {
    it('initializes from a ContextState and transition ID', () => {
      const baseState = new ContextState({
        left: "hello world ",
        startOfBuffer: true
      }, plainModel);

      const transition = new ContextTransition(baseState, 1);
      assert.sameOrderedMembers(
        transition.base.tokenization.tokens.map((t) => t.exampleInput),
        ['hello', ' ', 'world', ' ', '']
      );
      assert.equal(transition.transitionId, 1);
      assert.isNotOk(transition.final);
      assert.isNotOk(transition.inputDistribution);
      assert.isNotOk(transition.preservationTransform);
    });

    it('deep-copies when given a previous ContextState instance (no `final`)', () => {
      const baseState = new ContextState({
        left: "hello world ",
        startOfBuffer: true
      }, plainModel);

      const transition = new ContextTransition(baseState, 1);
      assert.sameOrderedMembers(
        transition.base.tokenization.tokens.map((t) => t.exampleInput),
        ['hello', ' ', 'world', ' ', '']
      );

      const cloned = new ContextTransition(transition);
      assert.notEqual(cloned, transition);
      assertClonedStateMatch(cloned.base, transition.base);
    });

    it('deep-copies when given a previous ContextState instance (with `final`)', () => {
      const baseState = new ContextState({
        left: "hello world ",
        startOfBuffer: true
      }, plainModel);

      // currently, also calls the .finalize method internally.
      const transition = ContextTracker.attemptMatchContext(baseState, plainModel, baseState, [
        { sample: { insert: '!', deleteLeft: 0 } }
      ]);

      const cloned = new ContextTransition(transition);
      assert.notEqual(cloned, transition);
      assertClonedTransitionMatch(cloned, transition);
    });
  });

  it('applySuggestion', () => {
    const baseContext = {
      left: "hello wo",
      startOfBuffer: true
    };
    const baseState = new ContextState(baseContext, plainModel);

    // currently, also calls the .finalize method internally.
    const transition = ContextTracker.attemptMatchContext(baseContext, plainModel, baseState, [
      { sample: { insert: 'r', deleteLeft: 0, id: 2 }, p: 1 }
    ]);
    assert.isOk(transition);

    let suggestions = transition.final.suggestions = [{
      transform: {
        insert: 'rld',
        deleteLeft: 0,
        id: 2
      },
      appendedTransform: {
        insert: ' ',
        deleteLeft: 0,
        id: 2
      },
      transformId: 2,
      id: 10,
      displayAs: 'world'
    }, {
      transform: {
        insert: 'n',
        deleteLeft: 0,
        id: 2
      },
      appendedTransform: {
        insert: ' ',
        deleteLeft: 0,
        id: 2
      },
      transformId: 2,
      id: 11,
      displayAs: 'won'
    }];

    const appliedTransition = transition.applySuggestion(suggestions[0]);
    assert.notEqual(appliedTransition, transition);
    assert.sameOrderedMembers(appliedTransition.final.tokenization.exampleInput, [
      'hello', ' ', 'world', ' ', ''
    ]);
    assert.equal(appliedTransition.final.appliedSuggestionId, suggestions[0].id);
    appliedTransition.final.tokenization.tokens.forEach((token, index) => {
      if(index >= 2) {
        assert.equal(token.appliedTransitionId, suggestions[0].transformId);
      } else {
        assert.isUndefined(token.appliedTransitionId);
      }
    });
    assert.deepEqual(appliedTransition.final.suggestions, transition.final.suggestions);
    assert.deepEqual(appliedTransition.final.inputTransforms, transition.final.inputTransforms);
  });

  describe('reproduceOriginal', () => {
    it('replicates the base instance if no suggestion was applied', () => {
      const baseContext = {
        left: "hello wo",
        startOfBuffer: true
      };
      const baseState = new ContextState(baseContext, plainModel);

      // currently, also calls the .finalize method internally.
      const transition = ContextTracker.attemptMatchContext(baseContext, plainModel, baseState, [
        { sample: { insert: 'r', deleteLeft: 0, id: 2 }, p: 1 }
      ]);

      let suggestions = transition.final.suggestions = [{
        transform: {
          insert: 'rld',
          deleteLeft: 0,
          id: 2
        },
        appendedTransform: {
          insert: ' ',
          deleteLeft: 0,
          id: 2
        },
        transformId: 2,
        id: 10,
        displayAs: 'world'
      }, {
        transform: {
          insert: 'n',
          deleteLeft: 0,
          id: 2
        },
        appendedTransform: {
          insert: ' ',
          deleteLeft: 0,
          id: 2
        },
        transformId: 2,
        id: 11,
        displayAs: 'won'
      }];

      const restoredTransition = transition.reproduceOriginal();
      assert.equal(restoredTransition.base, transition.base);
      assertClonedStateMatch(restoredTransition.final, transition.final);
      assert.equal(restoredTransition.final.suggestions, transition.final.suggestions);
      assert.equal(restoredTransition.final.suggestions, suggestions);
    });

    it('restores the original, pre-application transition', () => {
      const baseContext = {
        left: "hello wo",
        startOfBuffer: true
      };
      const baseState = new ContextState(baseContext, plainModel);

      // currently, also calls the .finalize method internally.
      const transition = ContextTracker.attemptMatchContext(baseContext, plainModel, baseState, [
        { sample: { insert: 'r', deleteLeft: 0, id: 2 }, p: 1 }
      ]);

      let suggestions = transition.final.suggestions = [{
        transform: {
          insert: 'rld',
          deleteLeft: 0,
          id: 2
        },
        appendedTransform: {
          insert: ' ',
          deleteLeft: 0,
          id: 2
        },
        transformId: 2,
        id: 10,
        displayAs: 'world'
      }, {
        transform: {
          insert: 'n',
          deleteLeft: 0,
          id: 2
        },
        appendedTransform: {
          insert: ' ',
          deleteLeft: 0,
          id: 2
        },
        transformId: 2,
        id: 11,
        displayAs: 'won'
      }];

      const appliedTransition = transition.applySuggestion(suggestions[0]);
      // To the point above, this matches the 'applySuggestion' test case.

      const restoredTransition = appliedTransition.reproduceOriginal();
      assertClonedTransitionMatch(restoredTransition, transition);
    });
  });
});