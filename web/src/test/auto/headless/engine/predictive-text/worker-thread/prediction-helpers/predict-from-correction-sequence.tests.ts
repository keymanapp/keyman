/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-04-13
 *
 * This file unit tests against the `predictFromCorrectionSequence`
 * prediction-helper function, validating construction of predictions based on
 * their root correction sequences.
 */

import { assert } from 'chai';

import { deepCopy } from "keyman/common/web-utils";
import { LexicalModelTypes } from '@keymanapp/common-types';

import { EDIT_DISTANCE_COST_SCALE, PredictionParameters, models, predictFromCorrectionSequence, tupleDisplayOrderSort } from "@keymanapp/lm-worker/test-index";

import CasingFunction = LexicalModelTypes.CasingFunction;
import DummyModel = models.DummyModel;
import Outcome = LexicalModelTypes.Outcome;
import Suggestion = LexicalModelTypes.Suggestion;

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
             .concat(text.substring(headUnitLength));        // tail - unchanged
  }
};

/** @type { import("#./models/dummy-model.js").DummyOptions } */
const DUMMY_MODEL_CONFIG = {
  punctuation: {
    quotesForKeepSuggestion: {
      open: '<',
      close: '>'
    },
    insertAfterWord: '\u00a0' // non-breaking space
  },
  applyCasing: applyCasing,
  searchTermToKey: (wordform: string) => {
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
};

describe('predictFromCorrectionSequence', () => {
  describe('on a single correction', () => {
    it('constructs suggestions matching multiple lexical entries directly - no transform ID', () => {
      const transitionID = 12345;

      const parameters: PredictionParameters = {
        rootContext: {
          left: '',
          right: '',
          startOfBuffer: true,
          endOfBuffer: true
        },
        tokens: [
          {
            correction: {
              sample: {
                insert: 'Its',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.6
            },
            casingRoot: '',
            autoSelectable: true
          }
        ],
        applyInPost: (x) => x,
        deleteLeft: 0
      };

      const dummied_suggestions: Outcome<Suggestion>[] = [
        {
          transform: {
            insert: "it's",
            deleteLeft: 2
          },
          displayAs: "it's",
          p: 0.18
        }, {
          transform: {
            insert: "its",
            deleteLeft: 2
          },
          displayAs: "its",
          p: 0.02
        }
      ];

      const model = new DummyModel({
        ...DUMMY_MODEL_CONFIG,
        futureSuggestions: [ dummied_suggestions ]
      });

      const predictions = predictFromCorrectionSequence(model, parameters);
      predictions.forEach((entry) => assert.equal(entry.components[0].correction, 'Its'));
      predictions.forEach((entry) => assert.equal(entry.metadata.probabilities.correction, 0.6));
      predictions.sort(tupleDisplayOrderSort);

      assert.sameDeepOrderedMembers(predictions.map((entry) => entry.components[0].prediction), dummied_suggestions.map((s) => {
        delete s.p;
        s.transformId = transitionID;
        s.transform.id = transitionID;
        return s;
      }));

      assert.approximately(predictions[0].metadata.probabilities.total, 0.18 * 0.6, 0.00001);
      assert.approximately(predictions[1].metadata.probabilities.total, 0.02 * 0.6, 0.00001);
    });

    it('constructs suggestions matching multiple lexical entries directly - with transform ID', () => {
      const transitionID = 314159;

      const parameters: PredictionParameters = {
        rootContext: {
          left: '',
          right: '',
          startOfBuffer: true,
          endOfBuffer: true
        },
        tokens: [
          {
            correction: {
              sample: {
                insert: 'Its',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.6
            },
            casingRoot: '',
            autoSelectable: true
          }
        ],
        applyInPost: (x) => x,
        deleteLeft: 0
      };

      const dummied_suggestions: Outcome<Suggestion>[] = [
        {
          transform: {
            insert: "it's",
            deleteLeft: 2
          },
          displayAs: "it's",
          p: 0.18
        }, {
          transform: {
            insert: "its",
            deleteLeft: 2
          },
          displayAs: "its",
          p: 0.02
        }
      ];

      const model = new DummyModel({
        ...DUMMY_MODEL_CONFIG,
        futureSuggestions: [ dummied_suggestions ]
      });

      const predictions = predictFromCorrectionSequence(model, parameters);
      predictions.forEach((entry) => assert.equal(entry.components[0].correction, 'Its'));
      predictions.forEach((entry) => assert.equal(entry.metadata.probabilities.correction, 0.6));
      predictions.sort(tupleDisplayOrderSort);

      assert.sameOrderedMembers(predictions.map((entry) => entry.components[0].prediction.displayAs), ["it's", "its"]);
      assert.sameDeepOrderedMembers(predictions.map((entry) => entry.components[0].prediction), dummied_suggestions.map((entry) => {
        entry = deepCopy(entry);
        entry.transformId = transitionID;
        entry.transform.id = transitionID;
        return entry;
      }));

      assert.approximately(predictions[0].metadata.probabilities.total, 0.18 * 0.6, 0.00001);
      assert.approximately(predictions[1].metadata.probabilities.total, 0.02 * 0.6, 0.00001);
      predictions.forEach((prediction) => assert.equal(prediction.components[0].prediction.transformId, transitionID));
    });

    it('constructs suggestions without input (as if after a context reset)', () => {
      const transitionID = 271828;

      const parameters: PredictionParameters = {
        rootContext: {
          left: '',
          right: '',
          startOfBuffer: true,
          endOfBuffer: true
        },
        tokens: [
          {
            correction: {
              sample: {
                insert: 'appl',
                deleteLeft: 0,
                id: transitionID
              },
              p: 1
            },
            casingRoot: 'appl',
            autoSelectable: true
          }
        ],
        applyInPost: (x) => x,
        deleteLeft: 0
      };

      const dummied_suggestions: Outcome<Suggestion>[] = [
        {
          transform: {
            insert: "apple",
            deleteLeft: 4
          },
          displayAs: "apple",
          p: 0.5
        }
      ];

      const model = new DummyModel({
        ...DUMMY_MODEL_CONFIG,
        futureSuggestions: [ dummied_suggestions ]
      });

      const predictions = predictFromCorrectionSequence(model, parameters);
      predictions.forEach((entry) => assert.deepEqual(entry.components.map((c => c.correction)), ['appl']));
      predictions.forEach((entry) => assert.equal(entry.metadata.probabilities.correction, 1));
      predictions.sort(tupleDisplayOrderSort);

      assert.sameDeepOrderedMembers(predictions.map((entry) => entry.components.map((c) => c.prediction)), [dummied_suggestions.map((s) => {
        delete s.p;
        s.transformId = transitionID;
        s.transform.id = transitionID;
        return s;
      })]);
    });
  });

  describe('on a sequence of corrections', () => {
    it('returns results even if some correction tokens lack predictions', () => {
      const transitionID = 101;

      const parameters: PredictionParameters = {
        rootContext: {
          left: 'i want to eat a ',
          right: '',
          startOfBuffer: true,
          endOfBuffer: true
        },
        tokens: [
          {
            correction: {
              sample: {
                insert: 'g',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.1
            },
            casingRoot: 'g',
            autoSelectable: true
          }, {
            correction: {
              sample: {
                insert: ' ',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.2
            },
            casingRoot: ' ',
            autoSelectable: true
          }, {
            correction: {
              sample: {
                insert: 'apple',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.2
            },
            casingRoot: 'apple',
            autoSelectable: true
          }
        ],
        applyInPost: (x) => x,
        deleteLeft: 0
      };

      const dummied_suggestion_sequences: Outcome<Suggestion>[][] = [
        [
          {
            transform: {
              insert: "g",
              deleteLeft: 0
            },
            displayAs: "g",
            p: 0.1
          }
        ],
        [],
        [
          {
            transform: {
              insert: "apple",
              deleteLeft: 0
            },
            displayAs: "apple",
            p: 0.5
          }
        ]
      ];

      const expected_predictions: Suggestion[] = [
        {
          transform: {
            insert: 'g',
            deleteLeft: 0,
            id: transitionID
          },
          displayAs: 'g',
          transformId: transitionID,
        }, {
          transform: {
            insert: ' ',
            deleteLeft: 0,
            id: transitionID
          },
          displayAs: ' ',
          transformId: transitionID
        }, {
          transform: {
            insert: 'apple',
            deleteLeft: 0,
            id: transitionID
          },
          displayAs: 'apple',
          transformId: transitionID
        }
      ];

      const expected_prediction_p = dummied_suggestion_sequences.map((dist) => {
          return dist[0]
        }).reduce((accum, curr) => {
          return accum * (curr ? curr.p : Math.exp(-EDIT_DISTANCE_COST_SCALE))
        }, 1);

      const model = new DummyModel({
        ...DUMMY_MODEL_CONFIG,
        futureSuggestions: dummied_suggestion_sequences
      });

      const predictions = predictFromCorrectionSequence(model, parameters);
      predictions.forEach((entry) => assert.deepEqual(entry.components.map((c) => c.correction), ['g', ' ', 'apple']));
      predictions.forEach((entry) => assert.equal(entry.metadata.probabilities.correction, parameters.tokens.reduce((accum, curr) => accum * curr.correction.p, 1)));
      predictions.sort(tupleDisplayOrderSort);

      assert.sameDeepOrderedMembers(predictions[0].components.map((c) => c.prediction), expected_predictions);

      assert.approximately(predictions[0].metadata.probabilities.prediction, expected_prediction_p, 0.00001);
    });

    it('returns no results if all correction tokens lack predictions', () => {
      const transitionID = 3;

      const parameters: PredictionParameters = {
        rootContext: {
          left: 'i want to eat a ',
          right: '',
          startOfBuffer: true,
          endOfBuffer: true
        },
        tokens: [
          {
            correction: {
              sample: {
                insert: 'golden',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.1
            },
            casingRoot: 'golden',
            autoSelectable: true
          }, {
            correction: {
              sample: {
                insert: ' ',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.2
            },
            casingRoot: ' ',
            autoSelectable: true
          }, {
            correction: {
              sample: {
                insert: 'app',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.2
            },
            casingRoot: 'app',
            autoSelectable: true
          }
        ],
        applyInPost: (x) => x,
        deleteLeft: 0
      };

      const dummied_suggestion_sequences: Outcome<Suggestion>[][] = [
        [],
        [],
        []
      ];

      const model = new DummyModel({
        ...DUMMY_MODEL_CONFIG,
        futureSuggestions: dummied_suggestion_sequences
      });

      const predictions = predictFromCorrectionSequence(model, parameters);
      assert.deepEqual(predictions, []);
    });

    it('uses only the best suggestion for non-final corrected tokens', () => {
      const transitionID = 42;

      const parameters: PredictionParameters = {
        rootContext: {
          left: 'i want to eat a ',
          right: '',
          startOfBuffer: true,
          endOfBuffer: true
        },
        tokens: [
          {
            correction: {
              sample: {
                insert: 'g',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.1
            },
            casingRoot: 'g',
            autoSelectable: true
          }, {
            correction: {
              sample: {
                insert: ' ',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.2
            },
            casingRoot: ' ',
            autoSelectable: true
          }, {
            correction: {
              sample: {
                insert: 'app',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.2
            },
            casingRoot: 'app',
            autoSelectable: true
          }
        ],
        applyInPost: (x) => x,
        deleteLeft: 0
      };

      const dummied_suggestion_sequences: Outcome<Suggestion>[][] = [
        [
          {
            transform: {
              insert: "golden",
              deleteLeft: 0
            },
            displayAs: "golden",
            p: 0.2
          }, {
            transform: {
              insert: "green",
              deleteLeft: 0
            },
            displayAs: "green",
            p: 0.15
          }, {
            transform: {
              insert: "gray",
              deleteLeft: 0
            },
            displayAs: "gray",
            p: 0.1
          }
        ],
        [],
        [
          {
            transform: {
              insert: "apple",
              deleteLeft: 0
            },
            displayAs: "apple",
            p: 0.5
          }
        ]
      ];

      const expected_prediction_p = dummied_suggestion_sequences
        .map((dist, i) => {
          // There is no valid 'g' entry corresponding to token index 0.
          return i == 0 ? null : dist[0]
        }).reduce((accum, curr) => {
          return accum * (curr ? curr.p : Math.exp(-EDIT_DISTANCE_COST_SCALE))
        }, 1);

      const expected_predictions: Suggestion[] = [
        {
          transform: {
            insert: 'g',
            deleteLeft: 0,
            id: transitionID
          },
          displayAs: 'g',
          transformId: transitionID
        }, {
          transform: {
            insert: ' ',
            deleteLeft: 0,
            id: transitionID
          },
          displayAs: ' ',
          transformId: transitionID
        }, {
          transform: {
            insert: 'apple',
            deleteLeft: 0,
            id: transitionID
          },
          displayAs: 'apple',
          transformId: transitionID
        }
      ];

      const model = new DummyModel({
        ...DUMMY_MODEL_CONFIG,
        futureSuggestions: dummied_suggestion_sequences
      });

      const predictions = predictFromCorrectionSequence(model, parameters);
      // There should be no variations with 'green' or 'gray' apples.
      assert.equal(predictions.length, 1);

      predictions.forEach((entry) => assert.deepEqual(entry.components.map((c) => c.correction), ['g', ' ', 'app']));
      predictions.forEach((entry) => assert.equal(entry.metadata.probabilities.correction, parameters.tokens.reduce((accum, curr) => accum * curr.correction.p, 1)));
      predictions.sort(tupleDisplayOrderSort);

      assert.deepEqual(predictions[0].components.map((c) => c.prediction.transform.insert), ['g', ' ', 'apple']);
      assert.sameDeepOrderedMembers(predictions[0].components.map((entry) => entry.prediction), expected_predictions);

      assert.approximately(predictions[0].metadata.probabilities.prediction, expected_prediction_p, 0.00001);
    });

    it('uses all suggestions generated from context-final correction-tokens', () => {
      const transitionID = 13;

      const parameters: PredictionParameters = {
        rootContext: {
          left: 'i want to eat a ',
          right: '',
          startOfBuffer: true,
          endOfBuffer: true
        },
        tokens: [
          {
            correction: {
              sample: {
                insert: 'golden',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.1
            },
            casingRoot: 'golden',
            autoSelectable: true
          }, {
            correction: {
              sample: {
                insert: ' ',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.2
            },
            casingRoot: ' ',
            autoSelectable: true
          }, {
            correction: {
              sample: {
                insert: 'app',
                deleteLeft: 0,
                id: transitionID
              },
              p: 0.2
            },
            casingRoot: 'app',
            autoSelectable: true
          }
        ],
        applyInPost: (x) => x,
        deleteLeft: 0
      };

      const dummied_suggestion_sequences: Outcome<Suggestion>[][] = [
        [
          {
            transform: {
              insert: "golden",
              deleteLeft: 0
            },
            displayAs: "golden",
            p: 0.1
          }
        ],
        [],
        [
          {
            transform: {
              insert: "apple",
              deleteLeft: 0
            },
            displayAs: "apple",
            p: 0.5
          }, {
            transform: {
              insert: "application",
              deleteLeft: 0
            },
            displayAs: "application",
            p: 0.11
          }, {
            transform: {
              insert: "appetizer",
              deleteLeft: 0
            },
            displayAs: "appetizer",
            p: 0.1
          }
        ]
      ];

      const tailIndex = dummied_suggestion_sequences.length - 1;

      const expected_prediction_prefix_p = dummied_suggestion_sequences
        .slice(0, dummied_suggestion_sequences.length - 1)
        .map((dist) => {
          return dist[0]
        }).reduce((accum, curr) => {
          return accum * (curr ? curr.p : Math.exp(-EDIT_DISTANCE_COST_SCALE))
        }, 1);

      const expected_prediction_prefix: Suggestion[] = [
        {
          transform: {
            insert: 'golden',
            deleteLeft: 0,
            id: transitionID
          },
          displayAs: 'golden',
          transformId: transitionID
        }, {
          transform: {
            insert: ' ',
            deleteLeft: 0,
            id: transitionID
          },
          displayAs: ' ',
          transformId: transitionID
        }
      ];

      const expected_prediction_sequences: Suggestion[][] = dummied_suggestion_sequences[tailIndex].map((p) => {
        return [...expected_prediction_prefix, p];
      });

      const expected_prediction_seq_probs: number[] = dummied_suggestion_sequences[tailIndex].map((p) => {
        return p.p * expected_prediction_prefix_p;
      });

      const model = new DummyModel({
        ...DUMMY_MODEL_CONFIG,
        futureSuggestions: dummied_suggestion_sequences
      });

      const predictions = predictFromCorrectionSequence(model, parameters);
      assert.equal(predictions.length, dummied_suggestion_sequences[dummied_suggestion_sequences.length - 1].length);

      predictions.forEach((entry) => assert.deepEqual(entry.components.map((c) => c.correction), ['golden', ' ', 'app']));
      predictions.forEach((entry) => assert.equal(entry.metadata.probabilities.correction, parameters.tokens.reduce((accum, curr) => accum * curr.correction.p, 1)));
      predictions.sort(tupleDisplayOrderSort);

      assert.sameDeepOrderedMembers(predictions.map((entry) => entry.components.map((c) => c.prediction)), expected_prediction_sequences);

      for(let i = 0; i < predictions.length; i++) {
        assert.approximately(predictions[i].metadata.probabilities.prediction, expected_prediction_seq_probs[i], 0.00001, `Expected probabilty mismatch at index ${i}`);
      }
    });
  });
});