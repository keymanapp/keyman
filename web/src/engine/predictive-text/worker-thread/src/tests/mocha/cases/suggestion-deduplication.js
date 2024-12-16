import * as wordBreakers from '@keymanapp/models-wordbreakers';
import { deepCopy } from '@keymanapp/web-utils';
import { assert } from 'chai';

import { dedupeSuggestions } from "#./predict-helpers.js";
import { DummyModel } from "#./models/dummy-model.js";

/*
 * This file's tests use these parts of a lexical model:
 * - model.wordbreaker
 */
const testModel = new DummyModel({
  wordbreaker: wordBreakers.default
  // No suggestions needed here, so we don't define any.
});

/**
 * Builds a fresh copy of test values useful for suggestion-similarity
 * testing.
 * @returns
 */
const build_its_is_set = () => {
  /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple} */
  const its = {
    correction: {
      sample: 'its',
      p: 0.8
    },
    prediction: {
      sample: {
        transform: {
          insert: 's',
          deleteLeft: 0
        },
        displayAs: 'its'
      },
      p: 0.2
    },
    totalProb: 0.16
    // matchLevel does not yet exist.
  };

  /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple} */
  const it_is = {
    correction: {
      sample: 'its',
      p: 0.8
    },
    prediction: {
      sample: {
        transform: {
          insert: '\'s',
          deleteLeft: 0
        },
        displayAs: 'it\'s'
      },
      p: 0.8
    },
    totalProb: 0.64
  };

  /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple} */
  const is = {
    correction: {
      sample: 'is',
      p: 0.2
    },
    prediction: {
      sample: {
        transform: {
          insert: 's',
          deleteLeft: 1
        },
        displayAs: 'is'
      },
      p: 0.5
    },
    totalProb: 0.1
  };

  /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple} */
  const is_not = {
    correction: {
      sample: 'is',
      p: 0.2
    },
    prediction: {
      sample: {
        transform: {
          insert: 'sn\'t',
          deleteLeft: 1
        },
        displayAs: 'isn\'t'
      },
      p: 0.5
    },
    totalProb: 0.1
  };

  return {
    its,
    it_is,
    is,
    is_not
  }
};

describe('dedupeSuggestions', () => {
  it('preserves all entries when there are no duplicates', () => {
    /** @type {Context} */
    const context = {
      left: 'It',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const testSet = build_its_is_set();
    const predictions = [...Object.values(testSet)];

    const deduplicated = dedupeSuggestions(testModel, predictions, context);

    assert.notStrictEqual(deduplicated, predictions);
    assert.sameMembers(deduplicated, predictions);
  });

  it('removes duplicates, combining their total-probabilities', () => {
    /** @type {Context} */
    const context = {
      left: 'It',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const testSet = build_its_is_set();
    const predictions = [
      ...Object.values(testSet).map((entry) => deepCopy(entry)),
      ...Object.values(testSet).map((entry) => deepCopy(entry)),
      deepCopy(testSet.it_is) // as in, `it's`, the contraction.
    ];

    const deduplicated = dedupeSuggestions(testModel, predictions, context);
    const expected = [...Object.values(testSet)];
    // Note:  only changes the _total_ probability.
    //
    // There's no mathematically safe way to combine the components if the
    // underlying correction sources differ between duplicated suggestions,
    // though it's mathematically safe to combine their product.
    expected.forEach((entry) => entry.totalProb *= (entry.prediction.sample.transform.insert == '\'s') ? 3 : 2);

    assert.deepEqual(deduplicated, expected);
  });
});