import { assert } from 'chai';

import * as wordBreakers from '@keymanapp/models-wordbreakers';
import { deepCopy } from 'keyman/common/web-utils';
import { LexicalModelTypes } from '@keymanapp/common-types';

import {
  CorrectionPredictionTuple,
  CorrectionPredictionTupleCore,
  SuggestionSimilarity,
  dedupeSuggestions,
  models
} from "@keymanapp/lm-worker/test-index";

import Context = LexicalModelTypes.Context;
import DummyModel = models.DummyModel;

/*
 * This file's tests use these parts of a lexical model:
 * - model.wordbreaker
 */
const testModel = new DummyModel({
  wordbreaker: wordBreakers.default
  // No suggestions needed here, so we don't define any.
});

const mockMetadata: (tc: CorrectionPredictionTupleCore) => CorrectionPredictionTuple = (t: CorrectionPredictionTupleCore) => {
  return {
    ...t,
    metadata: {
      preservationTransform: null,
      matchLevel: SuggestionSimilarity.none,
      rawEditCount: 0,    // does not matter for these tests.
      predictionLength: 0 // does not matter for these tests.
    }
  }
};

/**
 * Builds a fresh copy of test values useful for suggestion-similarity
 * testing.
 * @returns
 */
const build_its_is_set = () => {
  const its: CorrectionPredictionTupleCore = {
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

  const it_is: CorrectionPredictionTupleCore = {
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

  const is: CorrectionPredictionTupleCore = {
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

  const is_not: CorrectionPredictionTupleCore = {
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
    const context: Context = {
      left: 'It',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const testSet = build_its_is_set();
    const predictions: CorrectionPredictionTuple[] = [...Object.values(testSet)].map(mockMetadata) ;

    const deduplicated = dedupeSuggestions(testModel, predictions, context);

    assert.notStrictEqual(deduplicated, predictions);
    assert.sameMembers(deduplicated, predictions);
  });

  it('removes duplicates, combining their total-probabilities', () => {
    const context: Context = {
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
    ].map(mockMetadata);

    const deduplicated = dedupeSuggestions(testModel, predictions, context);
    const expected = [...Object.values(testSet)].map(mockMetadata);
    // Note:  only changes the _total_ probability.
    //
    // There's no mathematically safe way to combine the components if the
    // underlying correction sources differ between duplicated suggestions,
    // though it's mathematically safe to combine their product.
    expected.forEach((entry) => entry.totalProb *= (entry.prediction.sample.transform.insert == '\'s') ? 3 : 2);

    assert.deepEqual(deduplicated, expected);
  });
});