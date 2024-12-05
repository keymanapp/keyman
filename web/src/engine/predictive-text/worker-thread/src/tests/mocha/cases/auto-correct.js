import { AUTOSELECT_PROPORTION_THRESHOLD, SuggestionSimilarity, predictionAutoSelect, tupleDisplayOrderSort } from "#./predict-helpers.js";
import { assert } from 'chai';

/*
  * Preconditions:
  * - there should always be a 'keep' option.  Now, whether or not that option
  *   `.matchesModel` - that can vary.
  * - Predictions should be in sorted order (see tupleDisplaySortOrder).
  */
describe('predictionAutoSelect', () => {
  it(`does not throw when no suggestions are available`, () => {
    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]}
     */
    const predictions = [];
    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));

    assert.sameDeepOrderedMembers(predictions, originalPredictions);
  });

  it(`selects solitary 'keep' suggestion that does match the model`, () => {
    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]}
     */
    const predictions = [
      {
        correction: {
          sample: 'apple', // can be null / "mocked out"
          p: 1
        },
        prediction: {
          sample: {
            tag: 'keep',
            transform: {  // can be null / "mocked out"
              insert: 'e',
              deleteLeft: 0
            },
            matchesModel: true
          },
          p: 1
        },
        totalProb: 1
      }
    ];

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
    assert.isOk(autoselected);
  });

  it(`does not select solitary 'keep' suggestion that doesn't match the model`, () => {
    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]}
     */
    const predictions = [
      {
        correction: {
          sample: 'appl', // can be null / "mocked out"
          p: 1
        },
        prediction: {
          sample: {
            tag: 'keep',
            transform: { // can be null / "mocked out"
              insert: 'l',
              deleteLeft: 0
            },
            matchesModel: false
          },
          p: 1
        },
        totalProb: 1
      }
    ];

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
    assert.isNotOk(autoselected);
  });

  it(`selects 'keep' suggestion that does match the model over any alternatives`, () => {
    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple}
     */
    const keepSuggestion = {
      correction: {
        sample: 'thin', // can be null / "mocked out"
        p: .8
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          matchesModel: true
        },
        p: .05
      },
      totalProb: .04
    }

    const highestNonKeepSuggestion = {
      correction: {
        sample: 'thin', // can be null / "mocked out"
        p: .8
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
        },
        p: .55
      },
      totalProb: .44
    };

    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]}
     */
    const predictions = [
      keepSuggestion,
      highestNonKeepSuggestion,
      {
        correction: {
          sample: 'thin', // can be null / "mocked out"
          p: .8
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ng',
              deleteLeft: 0
            },
          },
          p: .4
        },
        totalProb: .32
      },
      {
        correction: {
          sample: 'thic', // can be null / "mocked out"
          p: .2
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ck',
              deleteLeft: 0
            },
          },
          p: 1
        },
        totalProb: .2
      }
    ];

    predictions.sort(tupleDisplayOrderSort);

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
    assert.equal(autoselected, keepSuggestion);
  });

  it(`selects solitary non-'keep' suggestion when 'keep' does not match model`, () => {
    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple}
     */
    const keepSuggestion = {
      correction: {
        sample: 'thin', // can be null / "mocked out"
        p: .8
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          matchesModel: false
        },
        p: .05
      },
      totalProb: .04
    }

    // To 'win', a suggestion (currently) needs at least twice the probability of the sum of all alternatives.
    // This threshold may be subject to change.
    //
    // Refer to AUTOSELECT_PROPORTION_THRESHOLD in predict-helpers.ts.
    const onlyNonKeepSuggestion = {
      correction: {
        sample: 'thin', // can be null / "mocked out"
        p: .8
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
        },
        p: .01
      },
      totalProb: .008
    };

    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]}
     */
    const predictions = [
      keepSuggestion,
      onlyNonKeepSuggestion
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.totalProb, 0);
    assert.isBelow(onlyNonKeepSuggestion.totalProb, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    predictions.sort(tupleDisplayOrderSort);

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
    assert.equal(autoselected, onlyNonKeepSuggestion);
  });

  it(`does not select non-'keep' without sufficient winning probability`, () => {
    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple}
     */
    const keepSuggestion = {
      correction: {
        sample: 'thin', // can be null / "mocked out"
        p: .8
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          matchesModel: false
        },
        p: .05
      },
      totalProb: .04
    }

    // To 'win', a suggestion (currently) needs at least twice the probability of the sum of all alternatives.
    // This threshold may be subject to change.
    //
    // Refer to AUTOSELECT_PROPORTION_THRESHOLD in predict-helpers.ts.
    const highestNonKeepSuggestion = {
      correction: {
        sample: 'thin', // can be null / "mocked out"
        p: .8
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
        },
        p: .55
      },
      totalProb: .44
    };

    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]}
     */
    const predictions = [
      keepSuggestion,
      highestNonKeepSuggestion,
      {
        correction: {
          sample: 'thin', // can be null / "mocked out"
          p: .8
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ng',
              deleteLeft: 0
            },
          },
          p: .4
        },
        totalProb: .32
      },
      {
        correction: {
          sample: 'thic', // can be null / "mocked out"
          p: .2
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ck',
              deleteLeft: 0
            },
          },
          p: 1
        },
        totalProb: .2
      }
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.totalProb, 0);
    assert.isBelow(highestNonKeepSuggestion.totalProb, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    predictions.sort(tupleDisplayOrderSort);

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
    assert.isNotOk(autoselected);
  });

  it(`does select non-'keep' with sufficient winning probability`, () => {
    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple}
     */
    const keepSuggestion = {
      correction: {
        sample: 'thin', // can be null / "mocked out"
        p: .8
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          matchesModel: false
        },
        p: .05
      },
      totalProb: .04
    }

    const highestNonKeepSuggestion = {
      correction: {
        sample: 'thin', // can be null / "mocked out"
        p: .9
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
        },
        p: .75
      },
      totalProb: .675
    };

    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]}
     */
    const predictions = [
      keepSuggestion,
      highestNonKeepSuggestion,
      {
        correction: {
          sample: 'thin', // can be null / "mocked out"
          p: .9
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ng',
              deleteLeft: 0
            },
          },
          p: .2
        },
        totalProb: .18
      },
      {
        correction: {
          sample: 'thic', // can be null / "mocked out"
          p: .1
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ck',
              deleteLeft: 0
            },
          },
          p: 1
        },
        totalProb: .1
      }
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.totalProb, 0);
    assert.isAbove(highestNonKeepSuggestion.totalProb, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    predictions.sort(tupleDisplayOrderSort);

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
    assert.equal(autoselected, highestNonKeepSuggestion);
  });

  it('ignores non key-matched suggestions when key-matched suggestions exist', () => {
    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple}
     */
    const keepSuggestion = {
      correction: {
        sample: 'cant', // can be null / "mocked out"
        p: 1
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 't',
            deleteLeft: 0
          },
          matchesModel: false
        },
        p: 1
      },
      totalProb: 1,
      matchLevel: SuggestionSimilarity.exact
    }

    const expectedSuggestion = {
      correction: {
        sample: 'cant', // can be null / "mocked out"
        p: 1
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: '\'t',
            deleteLeft: 0
          },
        },
        p: .2
      },
      totalProb: .2,
      matchLevel: SuggestionSimilarity.sameKey
    };

    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]}
     */
    const predictions = [
      keepSuggestion,
      expectedSuggestion,
      {
        correction: {
          sample: 'cant', // can be null / "mocked out"
          p: 1
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'teen',
              deleteLeft: 0
            },
          },
          p: .8
        },
        totalProb: .8,
        matchLevel: SuggestionSimilarity.none
      }
    ];

    predictions.sort(tupleDisplayOrderSort);

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
    assert.equal(autoselected, expectedSuggestion);
  });

  // The idea:  avoid "over-correcting" when a potential correction has a
  // super-high-frequency word.
  it('does not auto-select suggestion if its root correction is not most likely', () => {
    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple}
     */
    const keepSuggestion = {
      correction: {
        sample: 'thi', // can be null / "mocked out"
        p: .7
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'i',
            deleteLeft: 0
          },
          matchesModel: false
        },
        p: .05
      },
      totalProb: .035
    }

    const highestCorrectionSuggestion = {
      correction: {
        sample: 'thi', // can be null / "mocked out"
        p: .7
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'in',
            deleteLeft: 0
          },
        },
        p: .1
      },
      totalProb: .07
    };

    const highestNonKeepSuggestion = {
      correction: {
        sample: 'the', // can be null / "mocked out"
        p: .3
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'e',
            deleteLeft: 0
          },
        },
        p: 1
      },
      totalProb: .3
    };

    /**
     * @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]}
     */
    const predictions = [
      keepSuggestion,
      highestNonKeepSuggestion,
      highestCorrectionSuggestion
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.totalProb, 0);
    assert.isAbove(highestNonKeepSuggestion.totalProb, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    predictions.sort(tupleDisplayOrderSort);

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
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