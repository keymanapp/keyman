import { assert } from 'chai';

import {
  AUTOSELECT_PROPORTION_THRESHOLD,
  CompositedIntermediatePrediction,
  correctionValidForAutoSelect,
  predictionAutoSelect,
  PredictionMetadata,
  SuggestionSimilarity,
  tupleDisplayOrderSort
} from "@keymanapp/lm-worker/test-index";

const defaultMetadata: PredictionMetadata = {
  autoSelectable: true,
  matchLevel: SuggestionSimilarity.none,
  rawEditCount: 0,
  predictionLength: 0
}

/*
  * Preconditions:
  * - there should always be a 'keep' option.  Now, whether or not that option
  *   `.matchesModel` - that can vary.
  * - Predictions should be in sorted order (see tupleDisplaySortOrder).
  */
describe('predictionAutoSelect', () => {
  it(`does not throw when no suggestions are available`, () => {
    const predictions: CompositedIntermediatePrediction[] = [];
    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));

    assert.sameDeepOrderedMembers(predictions, originalPredictions);
  });

  it(`selects nothing if solitary 'keep' suggestion does match the model`, () => {
    const predictions: CompositedIntermediatePrediction[] = [
      {
        components: {
          prediction: {
            tag: 'keep',
            transform: {  // can be null / "mocked out"
              insert: 'e',
              deleteLeft: 0
            },
            matchesModel: true,
            displayAs: 'apple'
          },
          correction: 'apple',
        },
        probabilities: {
          prediction: 1,
          correction: 1,
          total: 1
        },
        metadata: defaultMetadata
      }
    ];

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.isNotOk(autoselected);
  });

  it(`does not select suggestions if the root correction has no letters`, () => {
    const predictions: CompositedIntermediatePrediction[] = [
      {
        components: {
          prediction: {
            tag: 'keep',
            transform: {
              insert: '5',
              deleteLeft: 0
            },
            matchesModel: false,
            displayAs: '5'
          },
          correction: '5'
        },
          probabilities: {
            prediction: 0.01,
            correction: 1,
            total: 0.01
          },
        metadata: {...defaultMetadata}
      },
      {
        components: {
          prediction: {
            transform: {
              insert: '5th',
              deleteLeft: 0
            },
            matchesModel: true,
            displayAs: '5th'
          },
          correction: '5'
        },
        probabilities: {
          prediction: 0.8,
          correction: 1,
          total: 0.8
        },
        metadata: {...defaultMetadata, autoSelectable: correctionValidForAutoSelect('5')}
      }
    ];

    const originalPredictions = [...predictions];
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.isNotOk(autoselected);
  });

  it(`does not select solitary 'keep' suggestion that doesn't match the model`, () => {
    const predictions: CompositedIntermediatePrediction[] = [
      {
        components: {
          prediction: {
            tag: 'keep',
            transform: { // can be null / "mocked out"
              insert: 'l',
              deleteLeft: 0
            },
            matchesModel: false,
            displayAs: '"appl"'
          },
          correction: 'appl'
        },
        probabilities: {
          prediction: 1,
          correction: 1,
          total: 1
        },
        metadata: {...defaultMetadata}
      }
    ];

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.isNotOk(autoselected);
  });

  it(`selects nothing for 'keep' suggestion that does match the model even with alternatives`, () => {
    const keepSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          matchesModel: true,
          displayAs: 'thin'
        },
        correction: 'thin'
      },
      probabilities: {
        prediction: .05,
        correction: .8,
        total: .05 * .8
      },
      metadata: {...defaultMetadata}
    }

    const highestNonKeepSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
          displayAs: 'think'
        },
        correction: 'thin'
      },
      probabilities: {
        prediction: .55,
        correction: .8,
        total: .55 * .8
      },
      metadata: {...defaultMetadata}
    };

    const predictions: CompositedIntermediatePrediction[] = [
      keepSuggestion,
      highestNonKeepSuggestion,
      {
        components: {
          prediction: {
            transform: {  // can be null / "mocked out"
              insert: 'ng',
              deleteLeft: 0
            },
            displayAs: 'thing'
          },
          correction: 'thin'
        },
        probabilities: {
          prediction: .4,
          correction: .8,
          total: .4 * .8
        },
        metadata: {...defaultMetadata}
      },
      {
        components: {
          prediction: {
            transform: {  // can be null / "mocked out"
              insert: 'ck',
              deleteLeft: 0
            },
            displayAs: 'thick'
          },
          correction: 'thic'
        },
        probabilities: {
          prediction: 1,
          correction: .2,
          total: 1 * .2
        },
        metadata: {...defaultMetadata}
      }
    ];

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.isNotOk(autoselected);
  });

  it(`selects solitary non-'keep' suggestion when 'keep' does not match model`, () => {
    const keepSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          displayAs: '"thin"',
          matchesModel: false
        },
        correction: 'thin'
      },
        probabilities: {
          prediction: .05,
          correction: .8,
          total: .8 * .05
        },
      metadata: {...defaultMetadata}
    }

    // To 'win', a suggestion (currently) needs at least twice the probability of the sum of all alternatives.
    // This threshold may be subject to change.
    //
    // Refer to AUTOSELECT_PROPORTION_THRESHOLD in predict-helpers.ts.
    const onlyNonKeepSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
          displayAs: 'think'
        },
        correction: 'thin'
      },
      probabilities: {
        prediction: .01,
        correction: .8,
        total: .01 * .8
      },
      metadata: {...defaultMetadata}
    };

    const predictions: CompositedIntermediatePrediction[] = [
      keepSuggestion,
      onlyNonKeepSuggestion
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.probabilities.total, 0);
    assert.isBelow(onlyNonKeepSuggestion.probabilities.total, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    predictions.sort(tupleDisplayOrderSort);

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.equal(autoselected, onlyNonKeepSuggestion);
  });

  it(`does not select non-'keep' without sufficient winning probability`, () => {
    const keepSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          displayAs: '"thin"',
          matchesModel: false
        },
        correction: 'thin'
      },
      probabilities: {
        prediction: .05,
        correction: .8,
        total: .05 * .8
      },
      metadata: {...defaultMetadata}
    }

    // To 'win', a suggestion (currently) needs at least twice the probability of the sum of all alternatives.
    // This threshold may be subject to change.
    //
    // Refer to AUTOSELECT_PROPORTION_THRESHOLD in predict-helpers.ts.
    const highestNonKeepSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
          displayAs: 'think'
        },
        correction: 'thin'
      },
      probabilities: {
        prediction: .55,
        correction: .8,
        total: .55 * .8
      },
      metadata: {...defaultMetadata}
    };

    const predictions: CompositedIntermediatePrediction[] = [
      keepSuggestion,
      highestNonKeepSuggestion,
      {
        components: {
          prediction: {
            transform: {  // can be null / "mocked out"
              insert: 'ng',
              deleteLeft: 0
            },
            displayAs: 'thing'
          },
          correction: 'thin'
        },
        probabilities: {
          prediction: .4,
          correction: .8,
          total: .4 * .8
        },
        metadata: {...defaultMetadata}
      },
      {
        components: {
          prediction: {
            transform: {  // can be null / "mocked out"
              insert: 'ck',
              deleteLeft: 0
            },
            displayAs: 'thick'
          },
          correction: 'thic'
        },
        probabilities: {
          prediction: 1,
          correction: .2,
          total: 1 * .2
        },
        metadata: {...defaultMetadata}
      }
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.probabilities.total, 0);
    assert.isBelow(highestNonKeepSuggestion.probabilities.total, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    predictions.sort(tupleDisplayOrderSort);

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.isNotOk(autoselected);
  });

  it(`does select non-'keep' with sufficient winning probability`, () => {
    const keepSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          displayAs: '"thin"',
          matchesModel: false
        },
        correction: 'thin'
      },
        probabilities: {
          prediction: .05,
          correction: .8,
          total: .05 * .8
        },
      metadata: {...defaultMetadata}
    };

    const highestNonKeepSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
          displayAs: 'think'
        },
        correction: 'thin'
      },
      probabilities: {
        prediction: .75,
        correction: .9,
        total: .75 * .9
      },
      metadata: {...defaultMetadata}
    };

    const predictions: CompositedIntermediatePrediction[] = [
      keepSuggestion,
      highestNonKeepSuggestion,
      {
        components: {
          prediction: {
            transform: {  // can be null / "mocked out"
              insert: 'ng',
              deleteLeft: 0
            },
            displayAs: 'thing'
          },
          correction: 'thin'
        },
        probabilities: {
          prediction: .2,
          correction: .9,
          total: .2 * .9
        },
        metadata: {...defaultMetadata}
      },
      {
        components: {
          prediction: {
            transform: {  // can be null / "mocked out"
              insert: 'ck',
              deleteLeft: 0
            },
            displayAs: 'thick'
          },
          correction: 'thic'
        },
        probabilities: {
          prediction: 1,
          correction: .1,
          total: 1 * .1
        },
        metadata: {...defaultMetadata}
      }
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.probabilities.total, 0);
    assert.isAbove(highestNonKeepSuggestion.probabilities.total, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.equal(autoselected, highestNonKeepSuggestion);
  });

  it('ignores non key-matched suggestions when key-matched suggestions exist', () => {
    const keepSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 't',
            deleteLeft: 0
          },
          displayAs: '"cant"',
          matchesModel: false
        },
        correction: 'cant'
      },
      probabilities: {
        prediction: 1,
        correction: 1,
        total: 1 * 1
      },
      metadata: { ...defaultMetadata, matchLevel: SuggestionSimilarity.exact }
    }

    const expectedSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          transform: {  // can be null / "mocked out"
            insert: '\'t',
            deleteLeft: 0
          },
          displayAs: "can't"
        },
        correction: 'cant'
      },
      probabilities: {
        prediction: .2,
        correction: 1,
        total: .2 * 1
      },
      metadata: { ...defaultMetadata, matchLevel: SuggestionSimilarity.sameKey }
    };

    const predictions: CompositedIntermediatePrediction[] = [
      keepSuggestion,
      expectedSuggestion,
      {
        components: {
          prediction: {
            transform: {  // can be null / "mocked out"
              insert: 'teen',
              deleteLeft: 0
            },
            displayAs: 'canteen'
          },
          correction: 'cant'
        },
        probabilities: {
          prediction: .8,
          correction: 1,
          total: .8 * 1
        },
        metadata: { ...defaultMetadata, matchLevel: SuggestionSimilarity.none, predictionLength: 3 }
      }
    ];

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));

    assert.sameDeepMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.equal(autoselected, expectedSuggestion);
  });

  // The idea:  avoid "over-correcting" when a potential correction has a
  // super-high-frequency word.
  it('does not auto-select suggestion if its root correction is not most likely', () => {
    const keepSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'i',
            deleteLeft: 0
          },
          displayAs: '"thi"',
          matchesModel: false
        },
        correction: 'thi'
      },
      probabilities: {
        prediction: .05,
        correction: .7,
        total: .05 * .7
      },
      metadata: {...defaultMetadata}
    };

    const highestCorrectionSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          transform: {  // can be null / "mocked out"
            insert: 'in',
            deleteLeft: 0
          },
          displayAs: 'thin'
        },
        correction: 'thi',
      },
      probabilities: {
        prediction: .1,
        correction: .7,
        total: .1 * .7
      },
      metadata: {...defaultMetadata}
    };

    const highestNonKeepSuggestion: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          transform: {  // can be null / "mocked out"
            insert: 'e',
            deleteLeft: 0
          },
          displayAs: 'the'
        },
        correction: 'the'
      },
      probabilities: {
        prediction: 1,
        correction: .3,
        total: 1 * .3
      },
      metadata: {...defaultMetadata}
    };

    const predictions: CompositedIntermediatePrediction[] = [
      keepSuggestion,
      highestNonKeepSuggestion,
      highestCorrectionSuggestion
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.probabilities.total, 0);
    assert.isAbove(highestNonKeepSuggestion.probabilities.total, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.isNotOk(autoselected);
  });

  // // If we add a setting allowing 'exact', 'sameText', and 'sameKey' tiers to
  // // all compete equally, rather than having each instantly win over those
  // // after it, we'd want to add a test such as this.
  //
  // it.skip("properly groups sufficiently-similar suggestions for auto-correction based on engine settings", () => {
  //   //
  // });
});