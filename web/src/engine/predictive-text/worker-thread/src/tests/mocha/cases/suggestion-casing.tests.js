/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-08-13.
 *
 * Defines simple unit tests for the internal method that applies a
 * lexical-model's casing rules to generated suggestions.
 */

import { applySuggestionCasing } from '#./predict-helpers.js';
import * as models from '#./models/index.js';
import * as wordBreakers from '@keymanapp/models-wordbreakers';

import { assert } from 'chai';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

var TrieModel = models.TrieModel;

describe('applySuggestionCasing', function() {
  let plainApplyCasing = function(caseToApply, text) {
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
      searchTermToKey: function(text) {
        // We're dealing with very simple English text; no need to normalize or remove diacritics here.
        return applyCasing('lower', text);
      }
    }
  );

  it('properly cases suggestions with no suggestion root', function() {
    let suggestion = {
      transform: {
        insert: 'the',
        deleteLeft: 0
      },
      displayAs: 'the'
    };

    applySuggestionCasing(suggestion, '', plainCasedModel, 'initial');
    assert.equal(suggestion.displayAs, 'The');
    assert.equal(suggestion.transform.insert, 'The');

    suggestion = {
      transform: {
        insert: 'thE',
        deleteLeft: 0
      },
      displayAs: 'thE'
    };

    applySuggestionCasing(suggestion, '', plainCasedModel, 'initial');
    assert.equal(suggestion.displayAs, 'ThE');
    assert.equal(suggestion.transform.insert, 'ThE');

    suggestion = {
      transform: {
        insert: 'the',
        deleteLeft: 0
      },
      displayAs: 'the'
    };

    applySuggestionCasing(suggestion, '', plainCasedModel, 'upper');
    assert.equal(suggestion.displayAs, 'THE');
    assert.equal(suggestion.transform.insert, 'THE');
  });

  it('properly cases suggestions that fully replace the suggestion root', function() {
    let suggestion = {
      transform: {
        insert: 'therefore',
        deleteLeft: 3
      },
      displayAs: 'therefore'
    };

    applySuggestionCasing(suggestion, 'the', plainCasedModel, 'initial');
    assert.equal(suggestion.displayAs, 'Therefore');
    assert.equal(suggestion.transform.insert, 'Therefore');

    suggestion = {
      transform: {
        insert: 'thereFore',
        deleteLeft: 3
      },
      displayAs: 'thereFore'
    };

    applySuggestionCasing(suggestion, 'the', plainCasedModel, 'initial');
    assert.equal(suggestion.displayAs, 'ThereFore');
    assert.equal(suggestion.transform.insert, 'ThereFore');

    suggestion = {
      transform: {
        insert: 'therefore',
        deleteLeft: 3
      },
      displayAs: 'therefore'
    };

    applySuggestionCasing(suggestion, 'the', plainCasedModel, 'upper');
    assert.equal(suggestion.displayAs, 'THEREFORE');
    assert.equal(suggestion.transform.insert, 'THEREFORE');
  });

  it('properly cases suggestions that do not fully replace the suggestion root', function() {
    let suggestion = {
      transform: {
        insert: 'erefore',
        deleteLeft: 1
      },
      displayAs: 'therefore'
    };

    // When integrated, the 'the' string comes from a wordbreak operation on the current context.
    applySuggestionCasing(suggestion, 'the', plainCasedModel, 'initial');
    assert.equal(suggestion.displayAs, 'Therefore');
    assert.equal(suggestion.transform.insert, 'Therefore');

    suggestion = {
      transform: {
        insert: 'ereFore',
        deleteLeft: 1
      },
      displayAs: 'thereFore'
    };

    applySuggestionCasing(suggestion, 'the', plainCasedModel, 'initial');
    assert.equal(suggestion.displayAs, 'ThereFore');
    assert.equal(suggestion.transform.insert, 'ThereFore');

    suggestion = {
      transform: {
        insert: 'erefore',
        deleteLeft: 1
      },
      displayAs: 'therefore'
    };

    applySuggestionCasing(suggestion, 'the', plainCasedModel, 'upper');
    assert.equal(suggestion.displayAs, 'THEREFORE');
    assert.equal(suggestion.transform.insert, 'THEREFORE');
  });
});