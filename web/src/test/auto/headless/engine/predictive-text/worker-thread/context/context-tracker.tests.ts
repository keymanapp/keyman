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

import { ModelCompositor, models } from '@keymanapp/lm-worker/test-index';

import TrieModel = models.TrieModel;

describe('ContextTracker', function() {
  describe('suggestion acceptance tracking', function() {
    let englishPunctuation = {
      quotesForKeepSuggestion: { open: `“`, close: `”`},
      insertAfterWord: ' '
    };

    // Needs improved context-state management (due to 2x tokens)
    it('tracks an accepted suggestion', async function() {
      let baseSuggestion = {
        transform: {
          insert: 'world',
          deleteLeft: 3,
          id: 2
        },
        appendedTransform: {
          insert: ' ',
          deleteLeft: 0,
          id: 15
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
      compositor.initContextTracker(baseContext, 0);
      const contextTracker = compositor.contextTracker;

      let preAppliedTransition = contextTracker.latest;
      // We'll ignore and overwrite the results.  We do need the prediction round to occur, though.
      await compositor.predict([{sample: postTransform, p: 1}], baseContext);
      contextTracker.latest.final.suggestions = [baseSuggestion];
      let reversion = compositor.acceptSuggestion(baseSuggestion, baseContext, postTransform);

      // Actual test assertion - was the replacement tracked?
      assert.isUndefined(preAppliedTransition.final.appliedSuggestionId);
      assert.equal(reversion.id, -baseSuggestion.id);
      contextTracker.unitTestEndPoints.cache().keys().forEach((key) => assert.isDefined(key));

      // Next step - on the followup context, is the replacement still active?
      let postContextMatch = contextTracker.unitTestEndPoints.cache().get(baseSuggestion.appendedTransform.id);
      assert.equal(postContextMatch.final.appliedSuggestionId, baseSuggestion.id);

      // Penultimate token corresponds to whitespace, which does not have a 'raw' representation.
      assert.equal(postContextMatch.final.tokenizations.length, 1);
      assert.equal(postContextMatch.final.tokenizations[0].tokens[postContextMatch.final.tokenizations[0].tokens.length - 2].exampleInput, ' ');

      // Final token is empty (follows a wordbreak)
      assert.equal(postContextMatch.final.tokenizations[0].tail.exampleInput, '');
    });
  });
});