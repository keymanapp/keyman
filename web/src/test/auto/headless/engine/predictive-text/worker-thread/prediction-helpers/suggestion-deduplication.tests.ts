import { assert } from 'chai';

import * as wordBreakers from '@keymanapp/models-wordbreakers';
import { deepCopy } from 'keyman/common/web-utils';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { IntermediateCompositedPrediction, dedupeSuggestions, models } from "@keymanapp/lm-worker/test-index";

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

/**
 * Builds a fresh copy of test values useful for suggestion-similarity
 * testing.
 * @returns
 */
const build_its_is_set = () => {
  const its: IntermediateCompositedPrediction = {
    components: {
      prediction: {
        transform: {
          insert: 's',
          deleteLeft: 0
        },
        displayAs: 'its'
      },
      correction: 'its'
    },
    metadata: {
      probabilities: {
        prediction: .2,
        correction: .8,
        total: .2 * .8
      },
      autoSelectable: true
      // matchLevel does not yet exist.
    }
  };

  const it_is: IntermediateCompositedPrediction = {
    components: {
      prediction: {
        transform: {
          insert: '\'s',
          deleteLeft: 0
        },
        displayAs: 'it\'s'
      },
      correction: 'its'
    },
    metadata: {
      probabilities: {
        prediction: .8,
        correction: .8,
        total: .8 * .8
      },
      autoSelectable: true
    }
  };

  const is: IntermediateCompositedPrediction = {
    components: {
      prediction: {
        transform: {
          insert: 's',
          deleteLeft: 1
        },
        displayAs: 'is'
      },
      correction: 'is'
    },
    metadata: {
      probabilities: {
        prediction: .5,
        correction: .2,
        total: .5 * .2
      },
      autoSelectable: true
    }
  };

  const is_not: IntermediateCompositedPrediction = {
    components: {
      prediction: {
        transform: {
          insert: 'sn\'t',
          deleteLeft: 1
        },
        displayAs: 'isn\'t'
      },
      correction: 'is'
    },
    metadata: {
      probabilities: {
        prediction: .5,
        correction: .2,
        total: .5 * .2
      },
      autoSelectable: true
    }
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
    const predictions = [...Object.values(testSet)];

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
    ];

    const deduplicated = dedupeSuggestions(testModel, predictions, context);
    const expected = [...Object.values(testSet)];
    // Note:  only changes the _total_ probability.
    //
    // There's no mathematically safe way to combine the components if the
    // underlying correction sources differ between duplicated suggestions,
    // though it's mathematically safe to combine their product.
    expected.forEach((entry) => entry.metadata.probabilities.total *= (entry.components.prediction.transform.insert == '\'s') ? 3 : 2);

    assert.deepEqual(deduplicated, expected);
  });
});