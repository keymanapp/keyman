import { assert } from 'chai';

import { AUTOSELECT_PROPORTION_THRESHOLD, CorrectionPredictionTuple, predictionAutoSelect, SuggestionSimilarity, tupleDisplayOrderSort } from "@keymanapp/lm-worker/test-index";
/*
  * Preconditions:
  * - there should always be a 'keep' option.  Now, whether or not that option
  *   `.matchesModel` - that can vary.
  * - Predictions should be in sorted order (see tupleDisplaySortOrder).
  */
describe('predictionAutoSelect', () => {
  it(`does not throw when no suggestions are available`, () => {
    const predictions: CorrectionPredictionTuple[] = [];
    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));

    assert.sameDeepOrderedMembers(predictions, originalPredictions);
  });

  it(`selects solitary 'keep' suggestion that does match the model`, () => {
    const predictions: CorrectionPredictionTuple[] = [
      {
        correction: {
          sample: 'apple',
          p: 1
        },
        prediction: {
          sample: {
            tag: 'keep',
            transform: {  // can be null / "mocked out"
              insert: 'e',
              deleteLeft: 0
            },
            matchesModel: true,
            displayAs: 'apple'
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

  it(`does not select suggestions if the root correction has no letters`, () => {
    const predictions: CorrectionPredictionTuple[] = [
      {
        correction: {
          sample: '5',
          p: 1
        },
        prediction: {
          sample: {
            tag: 'keep',
            transform: {
              insert: '5',
              deleteLeft: 0
            },
            matchesModel: false,
            displayAs: '5'
          },
          p: 0.01
        },
        totalProb: 0.01
      },
      {
        correction: {
          sample: '5',
          p: 1
        },
        prediction: {
          sample: {
            transform: {
              insert: '5th',
              deleteLeft: 0
            },
            matchesModel: true,
            displayAs: '5th'
          },
          p: 0.8
        },
        totalProb: 0.8
      }
    ];

    const originalPredictions = [...predictions];
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
    assert.isNotOk(autoselected);
  });

  it(`does not select solitary 'keep' suggestion that doesn't match the model`, () => {
    const predictions: CorrectionPredictionTuple[] = [
      {
        correction: {
          sample: 'appl',
          p: 1
        },
        prediction: {
          sample: {
            tag: 'keep',
            transform: { // can be null / "mocked out"
              insert: 'l',
              deleteLeft: 0
            },
            matchesModel: false,
            displayAs: '"appl"'
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
    const keepSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'thin',
        p: .8
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          matchesModel: true,
          displayAs: 'thin'
        },
        p: .05
      },
      totalProb: .04
    }

    const highestNonKeepSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'thin',
        p: .8
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
          displayAs: 'think'
        },
        p: .55
      },
      totalProb: .44
    };

    const predictions: CorrectionPredictionTuple[] = [
      keepSuggestion,
      highestNonKeepSuggestion,
      {
        correction: {
          sample: 'thin',
          p: .8
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ng',
              deleteLeft: 0
            },
            displayAs: 'thing'
          },
          p: .4
        },
        totalProb: .32
      },
      {
        correction: {
          sample: 'thic',
          p: .2
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ck',
              deleteLeft: 0
            },
            displayAs: 'thick'
          },
          p: 1
        },
        totalProb: .2
      }
    ];

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
    assert.equal(autoselected, keepSuggestion);
  });

  it(`selects solitary non-'keep' suggestion when 'keep' does not match model`, () => {
    const keepSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'thin',
        p: .8
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          displayAs: '"thin"',
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
    const onlyNonKeepSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'thin',
        p: .8
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
          displayAs: 'think'
        },
        p: .01
      },
      totalProb: .008
    };

    const predictions: CorrectionPredictionTuple[] = [
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
    const keepSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'thin',
        p: .8
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          displayAs: '"thin"',
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
    const highestNonKeepSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'thin',
        p: .8
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
          displayAs: 'think'
        },
        p: .55
      },
      totalProb: .44
    };

    const predictions: CorrectionPredictionTuple[] = [
      keepSuggestion,
      highestNonKeepSuggestion,
      {
        correction: {
          sample: 'thin',
          p: .8
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ng',
              deleteLeft: 0
            },
            displayAs: 'thing'
          },
          p: .4
        },
        totalProb: .32
      },
      {
        correction: {
          sample: 'thic',
          p: .2
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ck',
              deleteLeft: 0
            },
            displayAs: 'thick'
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
    const keepSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'thin',
        p: .8
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'n',
            deleteLeft: 0
          },
          displayAs: '"thin"',
          matchesModel: false
        },
        p: .05
      },
      totalProb: .04
    }

    const highestNonKeepSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'thin',
        p: .9
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'nk',
            deleteLeft: 0
          },
          displayAs: 'think'
        },
        p: .75
      },
      totalProb: .675
    };

    const predictions: CorrectionPredictionTuple[] = [
      keepSuggestion,
      highestNonKeepSuggestion,
      {
        correction: {
          sample: 'thin',
          p: .9
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ng',
              deleteLeft: 0
            },
            displayAs: 'thing'
          },
          p: .2
        },
        totalProb: .18
      },
      {
        correction: {
          sample: 'thic',
          p: .1
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'ck',
              deleteLeft: 0
            },
            displayAs: 'thick'
          },
          p: 1
        },
        totalProb: .1
      }
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.totalProb, 0);
    assert.isAbove(highestNonKeepSuggestion.totalProb, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
    assert.equal(autoselected, highestNonKeepSuggestion);
  });

  it('ignores non key-matched suggestions when key-matched suggestions exist', () => {
    const keepSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'cant',
        p: 1
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 't',
            deleteLeft: 0
          },
          displayAs: '"cant"',
          matchesModel: false
        },
        p: 1
      },
      totalProb: 1,
      matchLevel: SuggestionSimilarity.exact
    }

    const expectedSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'cant',
        p: 1
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: '\'t',
            deleteLeft: 0
          },
          displayAs: "can't"
        },
        p: .2
      },
      totalProb: .2,
      matchLevel: SuggestionSimilarity.sameKey
    };

    const predictions: CorrectionPredictionTuple[] = [
      keepSuggestion,
      expectedSuggestion,
      {
        correction: {
          sample: 'cant',
          p: 1
        },
        prediction: {
          sample: {
            transform: {  // can be null / "mocked out"
              insert: 'teen',
              deleteLeft: 0
            },
            displayAs: 'canteen'
          },
          p: .8
        },
        totalProb: .8,
        matchLevel: SuggestionSimilarity.none
      }
    ];

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));

    assert.sameDeepMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.prediction.sample.autoAccept);
    assert.equal(autoselected, expectedSuggestion);
  });

  // The idea:  avoid "over-correcting" when a potential correction has a
  // super-high-frequency word.
  it('does not auto-select suggestion if its root correction is not most likely', () => {
    const keepSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'thi',
        p: .7
      },
      prediction: {
        sample: {
          tag: 'keep',
          transform: {  // can be null / "mocked out"
            insert: 'i',
            deleteLeft: 0
          },
          displayAs: '"thi"',
          matchesModel: false
        },
        p: .05
      },
      totalProb: .035
    }

    const highestCorrectionSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'thi',
        p: .7
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'in',
            deleteLeft: 0
          },
          displayAs: 'thin'
        },
        p: .1
      },
      totalProb: .07
    };

    const highestNonKeepSuggestion: CorrectionPredictionTuple = {
      correction: {
        sample: 'the',
        p: .3
      },
      prediction: {
        sample: {
          transform: {  // can be null / "mocked out"
            insert: 'e',
            deleteLeft: 0
          },
          displayAs: 'the'
        },
        p: 1
      },
      totalProb: .3
    };

    const predictions: CorrectionPredictionTuple[] = [
      keepSuggestion,
      highestNonKeepSuggestion,
      highestCorrectionSuggestion
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.totalProb, 0);
    assert.isAbove(highestNonKeepSuggestion.totalProb, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepMembers(predictions, originalPredictions);

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