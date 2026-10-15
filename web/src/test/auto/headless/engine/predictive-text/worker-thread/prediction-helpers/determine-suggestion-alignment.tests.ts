import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import { ContextState, ContextTransition, determineSuggestionAlignment, models } from "@keymanapp/lm-worker/test-index";

import CasingFunction = LexicalModelTypes.CasingFunction;
import Context = LexicalModelTypes.Context;
import TrieModel = models.TrieModel;

const plainApplyCasing: CasingFunction = function(caseToApply, text) {
  switch(caseToApply) {
    case 'lower':
      return text.toLowerCase();
    case 'upper':
      return text.toUpperCase();
    case 'initial':
      return plainApplyCasing('upper', text.charAt(0)) . concat(text.substring(1));
    default:
      return text;
  }
};

const plainCasedModel = new TrieModel(
  jsonFixture('models/tries/english-1000'), {
    languageUsesCasing: true,
    applyCasing: plainApplyCasing,
    wordBreaker: defaultBreaker,
    searchTermToKey: function(text: string) {
      // We're dealing with very simple English text; no need to normalize or remove diacritics here.
      return plainApplyCasing('lower', text);
    }
  }
);

describe('determineSuggestionAlignment', () => {
  it('handles standard cases well - same token, no preservationTransforms', () => {
    const context: Context = {
      left: 'this is techn',
      startOfBuffer: true,
      endOfBuffer: true
    };
    const baseState = new ContextState(context, plainCasedModel);

    const transition = new ContextTransition(baseState, 0);
    transition.finalize(transition.base, [{sample: { insert: '', deleteLeft: 0 }, p: 1}]);

    // transition, model
    const results = determineSuggestionAlignment(transition, transition.final.tokenization, plainCasedModel);

    assert.deepEqual(results.predictionContext, context);
    assert.equal(results.correctionDeleteLeft, "techn".length /* does not include the deleted whitespace */);
    assert.equal(results.committedDeleteLeft, 0);
  });

  it('handles extension of prior token after backspace', () => {
    const context: Context = {
      left: 'this is tech ',
      startOfBuffer: true,
      endOfBuffer: true
    };
    const baseState = new ContextState(context, plainCasedModel);

    const transition = baseState.analyzeTransition(context, [{sample: { insert: '', deleteLeft: 1 }, p: 1}])

    // transition, model
    const results = determineSuggestionAlignment(transition, transition.final.tokenization, plainCasedModel);

    assert.deepEqual(results.predictionContext, {
      ...context,
      left: context.left.substring(0, context.left.length - 1),
      right: '',
      casingForm: undefined
    });
    assert.equal(results.correctionDeleteLeft, "tech".length /* does not include the deleted whitespace */);
    assert.equal(results.committedDeleteLeft, 1 /* for the deleted whitespace */);
  });

  it('handles extension of prior token after complex input with delete-left', () => {
    const context: Context = {
      left: 'this is tech ',
      startOfBuffer: true,
      endOfBuffer: true
    };
    const baseState = new ContextState(context, plainCasedModel);

    const transition = baseState.analyzeTransition(context, [{sample: { insert: 'n', deleteLeft: 1 }, p: 1}])

    // transition, model
    const results = determineSuggestionAlignment(transition, transition.final.tokenization, plainCasedModel);

    assert.deepEqual(results.predictionContext, {
      ...context,
      left: context.left.substring(0, context.left.length - 1),
      right: '',
      casingForm: undefined
    });
    assert.equal(results.correctionDeleteLeft, "tech".length /* does not include the deleted whitespace */);
    assert.equal(results.committedDeleteLeft, 1 /* for the deleted whitespace */);
  });
});