/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * This file contains tests designed to validate the context-caching
 * and context-tracking components for the Keyman predictive-text worker.
 */

import { assert } from 'chai';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { ModelCompositor, models } from '@keymanapp/lm-worker/test-index';

import Suggestion = LexicalModelTypes.Suggestion;
import TrieModel = models.TrieModel;


var emptyInput = (id: number) => [{sample: {insert: '', deleteLeft: 0, id: id}, p: 1}];

describe('ContextTracker', function() {
  describe('suggestion acceptance tracking', function() {
    let englishPunctuation = {
      quotesForKeepSuggestion: { open: `“`, close: `”`},
      insertAfterWord: ' '
    };

    // Needs improved context-state management (due to 2x tokens)
    it('tracks an accepted suggestion', function() {
      let baseSuggestion: Suggestion = {
        transform: {
          insert: 'world',
          deleteLeft: 3,
          id: 2
        },
        appendedTransform: {
          insert: ' ',
          deleteLeft: 0
        },
        transformId: 2,
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
        deleteLeft: 0,
        id: 2
      };

      let options = {
        punctuation: englishPunctuation
      };

      let model = new TrieModel(jsonFixture('models/tries/english-1000'), options);
      let compositor = new ModelCompositor(model);
      let preAppliedTransition = compositor.contextTracker.analyzeState(model, baseContext, emptyInput(0));
      // Mocks a prior prediction request without having done it.
      preAppliedTransition.final.suggestions = [baseSuggestion];
      let reversion = compositor.acceptSuggestion(baseSuggestion, baseContext, postTransform);

      // Actual test assertion - was the replacement tracked?
      assert.isUndefined(preAppliedTransition.final.appliedSuggestionId);
      assert.equal(reversion.id, -baseSuggestion.id);
      compositor.contextTracker.unitTestEndPoints.cache().keys().forEach((key) => assert.isDefined(key));

      // Next step - on the followup context, is the replacement still active?
      let postContext = models.applyTransform(baseSuggestion.appendedTransform, models.applyTransform(baseSuggestion.transform, baseContext));
      let postContextMatch = compositor.contextTracker.analyzeState(model, postContext, emptyInput(2));
      assert.equal(postContextMatch.final.appliedSuggestionId, baseSuggestion.id);

      // Penultimate token corresponds to whitespace, which does not have a 'raw' representation.
      assert.equal(postContextMatch.final.tokenization.tokens[postContextMatch.final.tokenization.tokens.length - 2].exampleInput, ' ');

      // Final token is empty (follows a wordbreak)
      assert.equal(postContextMatch.final.tokenization.tail.exampleInput, '');
    });
  });
});