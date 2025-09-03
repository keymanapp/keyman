/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * This file contains tests designed to validate behaviors for
 * tracking context-transitions within the Keyman predictive-text
 * worker - especially those supported by the ContextTransition class.
 */

import { assert } from 'chai';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import { ContextState, ContextTransition, models } from '@keymanapp/lm-worker/test-index';

import TrieModel = models.TrieModel;

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

function assertClonedStateMatch(a: ContextState, b: ContextState) {
  assert.notEqual(a, b);
  assert.notEqual(a.tokenization, b.tokenization);
  assert.notSameOrderedMembers(a.tokenization.tokens, b.tokenization.tokens);
  assert.sameOrderedMembers(a.tokenization.exampleInput, b.tokenization.exampleInput);

  assert.deepEqual(a.suggestions, b.suggestions);
}

function assertClonedTransitionMatch(a: ContextTransition, b: ContextTransition) {
  assert.notEqual(a, b);

  assertClonedStateMatch(a.base, b.base);
  assertClonedStateMatch(a.final, b.final);
}

describe('ContextTransition', () => {
  describe('<constructor>', () => {
    it('initializes from a ContextState and transition ID', () => {
      const baseState = new ContextState({
        left: "hello world ",
        startOfBuffer: true,
        endOfBuffer: true
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
        startOfBuffer: true,
        endOfBuffer: true
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
        startOfBuffer: true,
        endOfBuffer: true
      }, plainModel);

      // currently, also calls the .finalize method internally.
      const transition = baseState.analyzeTransition(baseState.context, [
        { sample: { insert: '!', deleteLeft: 0 }, p: 1 }
      ]);

      const cloned = new ContextTransition(transition);
      assert.notEqual(cloned, transition);
      assertClonedTransitionMatch(cloned, transition);
    });
  });

  describe('applySuggestion', () => {
    it('properly handles standard cases without suggestion-preserved text', () => {
      const baseContext = {
        left: "hello wo",
        startOfBuffer: true,
        endOfBuffer: true
      };
      const baseState = new ContextState(baseContext, plainModel);

      // currently, also calls the .finalize method internally.
      const transition = baseState.analyzeTransition(baseContext, [
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

     it('properly handles cases with suggestion-preserved text', () => {
      const baseContext = {
        left: "hello world ",
        startOfBuffer: true,
        endOfBuffer: true
      };
      const baseState = new ContextState(baseContext, plainModel);

      // currently, also calls the .finalize method internally.
      const transition = baseState.analyzeTransition(baseContext, [
        { sample: { insert: ' ', deleteLeft: 0, id: 2 }, p: 1 }
      ]);
      assert.isOk(transition);

      let suggestions = transition.final.suggestions = [{
        transform: {
          insert: ' the',
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
        displayAs: 'the'
      }, {
        transform: {
          insert: ' and',
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
        displayAs: 'and'
      }];

      const appliedTransition = transition.applySuggestion(suggestions[0]);
      assert.notEqual(appliedTransition, transition);
      assert.sameOrderedMembers(appliedTransition.final.tokenization.exampleInput, [
        'hello', ' ', 'world', '  ', 'the', ' ', ''
      ]);
      assert.equal(appliedTransition.final.appliedSuggestionId, suggestions[0].id);
      appliedTransition.final.tokenization.tokens.forEach((token, index) => {
        if(index >= 3) {
          assert.equal(token.appliedTransitionId, suggestions[0].transformId);
        } else {
          assert.isUndefined(token.appliedTransitionId);
        }
      });
      assert.deepEqual(appliedTransition.final.suggestions, transition.final.suggestions);
      assert.deepEqual(appliedTransition.final.inputTransforms, transition.final.inputTransforms);
    });
  });

  describe('reproduceOriginal', () => {
    it('replicates the base instance if no suggestion was applied', () => {
      const baseContext = {
        left: "hello wo",
        startOfBuffer: true,
        endOfBuffer: true
      };
      const baseState = new ContextState(baseContext, plainModel);

      // currently, also calls the .finalize method internally.
      const transition = baseState.analyzeTransition(baseContext, [
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
        startOfBuffer: true,
        endOfBuffer: true
      };
      const baseState = new ContextState(baseContext, plainModel);

      // currently, also calls the .finalize method internally.
      const transition = baseState.analyzeTransition(baseContext, [
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