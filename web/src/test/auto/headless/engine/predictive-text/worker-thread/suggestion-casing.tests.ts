/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-08-13.
 *
 * Defines simple unit tests for the internal method that applies a
 * lexical-model's casing rules to generated suggestions.
 */

import { assert } from 'chai';

import * as wordBreakers from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { TokenizedPredictionData, applySuggestionCasing, models } from '@keymanapp/lm-worker/test-index';

import CasingFunction = LexicalModelTypes.CasingFunction;
import TrieModel = models.TrieModel;

describe('applySuggestionCasing', function() {
  let plainApplyCasing: CasingFunction = function(caseToApply, text) {
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

  var plainCasedModel = new TrieModel(
    jsonFixture('models/tries/english-1000'), {
      languageUsesCasing: true,
      applyCasing: plainApplyCasing,
      wordBreaker: wordBreakers.default,
      searchTermToKey: function(text: string) {
        // We're dealing with very simple English text; no need to normalize or remove diacritics here.
        return plainApplyCasing('lower', text);
      }
    }
  );

  it('properly cases suggestions with no suggestion root', function() {
    let suggestion: TokenizedPredictionData[] = [{
      prediction: {
        transform: {
          insert: 'the',
          deleteLeft: 0
        },
        displayAs: 'the'
      },
      correction: '',
      casingRoot: 'th'
    }];

    applySuggestionCasing(suggestion[0], plainCasedModel);
    assert.equal(suggestion[0].prediction.displayAs, 'the');
    assert.equal(suggestion[0].prediction.transform.insert, 'the');

    suggestion = [{
      prediction: {
        transform: {
          insert: 'ThE',
          deleteLeft: 0
        },
        displayAs: 'ThE'
      },
      correction: '',
      casingRoot: 'Th'
    }];

    applySuggestionCasing(suggestion[0], plainCasedModel);
    assert.equal(suggestion[0].prediction.displayAs, 'ThE');
    assert.equal(suggestion[0].prediction.transform.insert, 'ThE');
  });

  it('properly cases suggestions that fully replace the suggestion root', function() {
    let suggestion: TokenizedPredictionData[] = [{
      prediction: {
        transform: {
          insert: 'therefore',
          deleteLeft: 3
        },
        displayAs: 'therefore'
      },
      correction: 'The',
      casingRoot: 'Th'
    }];

    applySuggestionCasing(suggestion[0], plainCasedModel);
    assert.equal(suggestion[0].prediction.displayAs, 'Therefore');
    assert.equal(suggestion[0].prediction.transform.insert, 'Therefore');

    suggestion = [{
      prediction: {
        transform: {
          insert: 'thereFore',
          deleteLeft: 3
        },
        displayAs: 'thereFore'
      },
      correction: 'The',
      casingRoot: 'Th'
    }];

    applySuggestionCasing(suggestion[0], plainCasedModel);
    assert.equal(suggestion[0].prediction.displayAs, 'ThereFore');
    assert.equal(suggestion[0].prediction.transform.insert, 'ThereFore');

    suggestion = [{
      prediction: {
        transform: {
          insert: 'therefore',
          deleteLeft: 3
        },
        displayAs: 'therefore'
      },
      correction: 'THE',
      casingRoot: 'TH'
    }];

    applySuggestionCasing(suggestion[0], plainCasedModel);
    assert.equal(suggestion[0].prediction.displayAs, 'THEREFORE');
    assert.equal(suggestion[0].prediction.transform.insert, 'THEREFORE');
  });

  it('properly cases suggestions that do not fully replace the suggestion root', function() {
    let suggestion: TokenizedPredictionData[] = [{
      prediction: {
        transform: {
          insert: 'therefore',
          deleteLeft: 3
        },
        displayAs: 'therefore'
      },
      correction: 'The',
      casingRoot: 'Th'
    }];

    // When integrated, the 'the' string comes from a wordbreak operation on the current context.
    applySuggestionCasing(suggestion[0], plainCasedModel);
    assert.equal(suggestion[0].prediction.displayAs, 'Therefore');
    assert.equal(suggestion[0].prediction.transform.insert, 'Therefore');

    suggestion = [{
      prediction: {
        transform: {
          insert: 'ThereFore',
          deleteLeft: 3
        },
        displayAs: 'thereFore'
      },
      correction: 'The',
      casingRoot: 'Th'
    }];

    applySuggestionCasing(suggestion[0], plainCasedModel);
    assert.equal(suggestion[0].prediction.displayAs, 'ThereFore');
    assert.equal(suggestion[0].prediction.transform.insert, 'ThereFore');

    suggestion = [{
      prediction: {
        transform: {
          insert: 'therefore',
          deleteLeft: 3
        },
        displayAs: 'therefore'
      },
      correction: 'THE',
      casingRoot: 'TH'
    }];

    applySuggestionCasing(suggestion[0], plainCasedModel);
    assert.equal(suggestion[0].prediction.displayAs, 'THEREFORE');
    assert.equal(suggestion[0].prediction.transform.insert, 'THEREFORE');
  });
});