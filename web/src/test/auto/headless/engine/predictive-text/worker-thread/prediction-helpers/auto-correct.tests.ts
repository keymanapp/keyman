import { assert } from 'chai';

import { AUTOSELECT_PROPORTION_THRESHOLD, IntermediateCompositedPrediction, predictionAutoSelect, SuggestionSimilarity, tupleDisplayOrderSort } from "@keymanapp/lm-worker/test-index";
/*
  * Preconditions:
  * - there should always be a 'keep' option.  Now, whether or not that option
  *   `.matchesModel` - that can vary.
  * - Predictions should be in sorted order (see tupleDisplaySortOrder).
  */
describe('predictionAutoSelect', () => {
  it(`does not throw when no suggestions are available`, () => {
    const predictions: IntermediateCompositedPrediction[] = [];
    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));

    assert.sameDeepOrderedMembers(predictions, originalPredictions);
  });

  it(`selects solitary 'keep' suggestion that does match the model`, () => {
    const predictions: IntermediateCompositedPrediction[] = [
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
        metadata: {
          probabilities: {
            prediction: 1,
            correction: 1,
            total: 1
          },
          autoSelectable: true
        }
      }
    ];

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.isOk(autoselected);
  });

  it(`does not select suggestions if the root correction has no letters`, () => {
    const predictions: IntermediateCompositedPrediction[] = [
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
        metadata: {
          probabilities: {
            prediction: 0.01,
            correction: 1,
            total: 0.01
          },
          autoSelectable: false
        }
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
        metadata: {
          probabilities: {
            prediction: 0.8,
            correction: 1,
            total: 0.8
          },
          autoSelectable: false
        }
      }
    ];

    const originalPredictions = [...predictions];
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.isNotOk(autoselected);
  });

  it(`does not select solitary 'keep' suggestion that doesn't match the model`, () => {
    const predictions: IntermediateCompositedPrediction[] = [
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
        metadata: {
          probabilities: {
            prediction: 1,
            correction: 1,
            total: 1
          },
          autoSelectable: true
        }
      }
    ];

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.isNotOk(autoselected);
  });

  it(`selects 'keep' suggestion that does match the model over any alternatives`, () => {
    const keepSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: .05,
          correction: .8,
          total: .05 * .8
        },
        autoSelectable: true
      }
    }

    const highestNonKeepSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: .55,
          correction: .8,
          total: .55 * .8
        },
        autoSelectable: true
      }
    };

    const predictions: IntermediateCompositedPrediction[] = [
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
        metadata: {
          probabilities: {
            prediction: .4,
            correction: .8,
            total: .4 * .8
          },
          autoSelectable: true
        }
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
        metadata: {
          probabilities: {
            prediction: 1,
            correction: .2,
            total: 1 * .2
          },
          autoSelectable: true
        }
      }
    ];

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.equal(autoselected, keepSuggestion);
  });

  it(`selects solitary non-'keep' suggestion when 'keep' does not match model`, () => {
    const keepSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: .05,
          correction: .8,
          total: .8 * .05
        },
        autoSelectable: true
      }
    }

    // To 'win', a suggestion (currently) needs at least twice the probability of the sum of all alternatives.
    // This threshold may be subject to change.
    //
    // Refer to AUTOSELECT_PROPORTION_THRESHOLD in predict-helpers.ts.
    const onlyNonKeepSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: .01,
          correction: .8,
          total: .01 * .8
        },
        autoSelectable: true
      }
    };

    const predictions: IntermediateCompositedPrediction[] = [
      keepSuggestion,
      onlyNonKeepSuggestion
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.metadata.probabilities.total, 0);
    assert.isBelow(onlyNonKeepSuggestion.metadata.probabilities.total, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    predictions.sort(tupleDisplayOrderSort);

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.equal(autoselected, onlyNonKeepSuggestion);
  });

  it(`does not select non-'keep' without sufficient winning probability`, () => {
    const keepSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: .05,
          correction: .8,
          total: .05 * .8
        },
        autoSelectable: true
      }
    }

    // To 'win', a suggestion (currently) needs at least twice the probability of the sum of all alternatives.
    // This threshold may be subject to change.
    //
    // Refer to AUTOSELECT_PROPORTION_THRESHOLD in predict-helpers.ts.
    const highestNonKeepSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: .55,
          correction: .8,
          total: .55 * .8
        },
        autoSelectable: true
      }
    };

    const predictions: IntermediateCompositedPrediction[] = [
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
        metadata: {
          probabilities: {
            prediction: .4,
            correction: .8,
            total: .4 * .8
          },
          autoSelectable: true
        }
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
        metadata: {
          probabilities: {
            prediction: 1,
            correction: .2,
            total: 1 * .2
          },
          autoSelectable: true
        }
      }
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.metadata.probabilities.total, 0);
    assert.isBelow(highestNonKeepSuggestion.metadata.probabilities.total, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    predictions.sort(tupleDisplayOrderSort);

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepOrderedMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.isNotOk(autoselected);
  });

  it(`does select non-'keep' with sufficient winning probability`, () => {
    const keepSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: .05,
          correction: .8,
          total: .05 * .8
        },
        autoSelectable: true
      }
    }

    const highestNonKeepSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: .75,
          correction: .9,
          total: .75 * .9
        },
        autoSelectable: true
      }
    };

    const predictions: IntermediateCompositedPrediction[] = [
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
        metadata: {
          probabilities: {
            prediction: .2,
            correction: .9,
            total: .2 * .9
          },
          autoSelectable: true
        }
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
        metadata: {
          probabilities: {
            prediction: 1,
            correction: .1,
            total: 1 * .1
          },
          autoSelectable: true
        }
      }
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.metadata.probabilities.total, 0);
    assert.isAbove(highestNonKeepSuggestion.metadata.probabilities.total, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

    const originalPredictions = [].concat(predictions);
    assert.doesNotThrow(() => predictionAutoSelect(predictions));
    assert.sameDeepMembers(predictions, originalPredictions);

    const autoselected = predictions.find((entry) => entry.components.prediction.autoAccept);
    assert.equal(autoselected, highestNonKeepSuggestion);
  });

  it('ignores non key-matched suggestions when key-matched suggestions exist', () => {
    const keepSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: 1,
          correction: 1,
          total: 1 * 1
        },
        autoSelectable: true,
        matchLevel: SuggestionSimilarity.exact
      }
    }

    const expectedSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: .2,
          correction: 1,
          total: .2 * 1
        },
        autoSelectable: true,
        matchLevel: SuggestionSimilarity.sameKey
      }
    };

    const predictions: IntermediateCompositedPrediction[] = [
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
        metadata: {
          probabilities: {
            prediction: .8,
            correction: 1,
            total: .8 * 1
          },
          autoSelectable: true,
          matchLevel: SuggestionSimilarity.none
        }
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
    const keepSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: .05,
          correction: .7,
          total: .05 * .7
        },
        autoSelectable: true
      }
    }

    const highestCorrectionSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: .1,
          correction: .7,
          total: .1 * .7
        },
        autoSelectable: true
      }
    };

    const highestNonKeepSuggestion: IntermediateCompositedPrediction = {
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
      metadata: {
        probabilities: {
          prediction: 1,
          correction: .3,
          total: 1 * .3
        },
        autoSelectable: true
      }
    };

    const predictions: IntermediateCompositedPrediction[] = [
      keepSuggestion,
      highestNonKeepSuggestion,
      highestCorrectionSuggestion
    ];

    const totalProb = predictions.reduce((accum, current) => accum + current.metadata.probabilities.total, 0);
    assert.isAbove(highestNonKeepSuggestion.metadata.probabilities.total, totalProb * AUTOSELECT_PROPORTION_THRESHOLD, 'test setup is no longer valid');

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