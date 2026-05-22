/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-05-18
 *
 * This file tests the prediction helper-method responsible for preparing
 * corrections for multi-token prediction for some custom and all legacy models.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from "@keymanapp/common-types";
import * as wordBreakers from '@keymanapp/models-wordbreakers';

import { determineTraversallessCorrectionSequences, models } from "@keymanapp/lm-worker/test-index";

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

const testModel = new DummyModel({
  ...DUMMY_MODEL_CONFIG,
  // No suggestions needed here, so we don't define any.
});

describe('determineTraversallessCorrectionSequences', () => {
  it(`processes standard-case corrections correctly - text appended to existing token`, () => {
    const context: Context = {
      left: 'I want an iPhon',
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

    const predictionRootEntries = determineTraversallessCorrectionSequences(testModel, [trueInput], context);

    assert.equal(predictionRootEntries.length, 1);
    const entry = predictionRootEntries[0];

    assert.deepEqual(
      {
        ...entry.rootContext, casingForm: entry.rootContext.casingForm ?? undefined
      }, {
        casingForm: undefined,
        left: 'I want an ',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      }
    );

    assert.deepEqual(entry.tokenizedCorrection, [{
      sample: {
        insert: 'iPhone',
        deleteLeft: 0
      },
      p: 1
    }]);
  });
});