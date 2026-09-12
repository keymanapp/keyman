/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-17
 *
 * This file tests the prediction helper-method responsible for constructing
 * 'keep' suggestions when no lexicon-based suggestion fits the role.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from "@keymanapp/common-types";
import * as wordBreakers from '@keymanapp/models-wordbreakers';

import { CorrectionPredictionTuple, createDefaultKeep, models, SuggestionSimilarity } from "@keymanapp/lm-worker/test-index";

import CasingFunction = LexicalModelTypes.CasingFunction;
import Context = LexicalModelTypes.Context;
import DummyModel = models.DummyModel;
import DummyOptions = models.DummyOptions;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;


/*
 * This file's tests use these parts of a lexical model:
 * - model.wordbreaker
 * - model.toKey
 * - model.applyCasing
 * - model.punctuation
 */

const DUMMY_MODEL_CONFIG: DummyOptions = {
  punctuation: {
    quotesForKeepSuggestion: {
      open: '<',
      close: '>'
    },
    insertAfterWord: '\u00a0' // non-breaking space
  },
  wordbreaker: wordBreakers.default
};

// See: developer/src/kmc-model/model-defaults.ts, defaultApplyCasing
const applyCasing: CasingFunction = (casing, text) => {
  switch(casing) {
    case 'lower':
      return text.toLowerCase();
    case 'upper':
      return text.toUpperCase();
    case 'initial':
      var headCode = text.charCodeAt(0);
      // The length of the first code unit, as measured in code points.
      var headUnitLength = 1;

      // Is the first character a high surrogate, indicating possible use of UTF-16
      // surrogate pairs?  Also, is the string long enough for there to BE a pair?
      if(text.length > 1 && headCode >= 0xD800 && headCode <= 0xDBFF) {
        // It's possible, so now we check for low surrogates.
        var lowSurrogateCode = text.charCodeAt(1);

        if(lowSurrogateCode >= 0xDC00 && lowSurrogateCode <= 0xDFFF) {
          // We have a surrogate pair; this pair is the 'first' character.
          headUnitLength++;
        }
      }

      // Capitalizes the first code unit of the string, leaving the rest intact.
      return text.substring(0, headUnitLength).toUpperCase() // head - uppercased
             .concat(text.substring(headUnitLength));        // tail - lowercased
  }
};
const testModelWithCasing = new DummyModel({
  ...DUMMY_MODEL_CONFIG,
  applyCasing: applyCasing,
  toKey: (wordform) => {
    // See: developer/src/kmc-model/model-defaults.ts, defaultCasedSearchTermToKey
    return applyCasing('lower', wordform)
      .normalize('NFKD')
      // Remove any combining diacritics (if input is in NFKD)
      .replace(/[\u0300-\u036F]/g, '')
      // Replace directional quotation marks with plain apostrophes
      .replace(/[‘’]/g, "'")
      // Also double-quote marks.
      .replace(/[“”]/g, '"')
      // ** Difference from model-defaults here **
      // And finally, erase single-quotation marks.
      .replace(/'/, '');
  },
  languageUsesCasing: true
  // No suggestions needed here, so we don't define any.
});

describe('produceKeep', () => {
  it(`creates an 'exact'-match suggestion based on primary input and current context`, () => {
    const context: Context = {
      left: 'iphon',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const trueInput: ProbabilityMass<Transform> = {
      sample: {
        insert: 'e',
        deleteLeft: 0
      },
      p: 1
    };

    const expectedKeep: CorrectionPredictionTuple = {
      correction: {
        sample: 'iphone',
        p: 1
      },
      prediction: {
        sample: {
          transform: {
            insert: 'iphone',
            deleteLeft: 5
          },
          displayAs: '<iphone>',
          matchesModel: false,
          tag: 'keep'
        },
        p: 1
      },
      totalProb: 1,
      matchLevel: SuggestionSimilarity.exact
    };

    const tuple = createDefaultKeep(testModelWithCasing, context, trueInput);
    assert.deepEqual(tuple, expectedKeep);
  });
});