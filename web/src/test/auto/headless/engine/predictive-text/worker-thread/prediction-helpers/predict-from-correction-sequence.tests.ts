
import { assert } from 'chai';

import { deepCopy } from "keyman/common/web-utils";
import { LexicalModelTypes } from '@keymanapp/common-types';

import { EDIT_DISTANCE_COST_SCALE, models, predictFromCorrectionSequence, tupleDisplayOrderSort } from "@keymanapp/lm-worker/test-index";

import CasingFunction = LexicalModelTypes.CasingFunction;
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import DummyModel = models.DummyModel;
import Outcome = LexicalModelTypes.Outcome;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Suggestion = LexicalModelTypes.Suggestion;
import Transform = LexicalModelTypes.Transform;

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
      const context: Context = {
        left: '',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const correctionDistribution: Distribution<Transform> = [{
          sample: {
            insert: 'Its',
            deleteLeft: 0
          },
          p: 0.6
        }
      ];

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

      const predictions = predictFromCorrectionSequence(model, correctionDistribution, context);
      predictions.forEach((entry) => assert.equal(entry.correction.sample, 'Its'));
      predictions.forEach((entry) => assert.equal(entry.correction.p, 0.6));
      predictions.sort(tupleDisplayOrderSort);

      assert.sameDeepOrderedMembers(predictions.map((entry) => entry.prediction.sample), dummied_suggestions.map((s) => {
        delete s.p;
        return s;
      }));

      assert.approximately(predictions[0].totalProb, 0.18 * 0.6, 0.00001);
      assert.approximately(predictions[1].totalProb, 0.02 * 0.6, 0.00001);
    });

    it('constructs suggestions matching multiple lexical entries directly - with transform ID', () => {
      const context: Context = {
        left: '',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const correctionDistribution: Distribution<Transform> = [{
          sample: {
            insert: 'Its',
            deleteLeft: 0,
            id: 314159
          },
          p: 0.6
        }
      ];

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

      const predictions = predictFromCorrectionSequence(model, correctionDistribution, context);
      predictions.forEach((entry) => assert.equal(entry.correction.sample, 'Its'));
      predictions.forEach((entry) => assert.equal(entry.correction.p, 0.6));
      predictions.sort(tupleDisplayOrderSort);

      assert.sameOrderedMembers(predictions.map((entry) => entry.prediction.sample.displayAs), ["it's", "its"]);
      assert.sameDeepOrderedMembers(predictions.map((entry) => entry.prediction.sample), dummied_suggestions.map((entry) => {
        entry = deepCopy(entry);
        entry.transformId = 314159;
        delete entry.p;
        return entry;
      }));

      assert.approximately(predictions[0].totalProb, 0.18 * 0.6, 0.00001);
      assert.approximately(predictions[1].totalProb, 0.02 * 0.6, 0.00001);
    });
  });

  describe('on a sequence of corrections', () => {
    it('returns results even if some correction tokens lack predictions', () => {
      const context: Context = {
        left: 'i want to eat a ',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const correctionSequence: Distribution<Transform> = [
        {
          sample: {
            insert: 'golden',
            deleteLeft: 0
          },
          p: 0.1
        }, {
          sample: {
            insert: ' ',
            deleteLeft: 0
          },
          p: 0.2
        }, {
          sample: {
            insert: 'app',
            deleteLeft: 0
          },
          p: 0.2
        }
      ];

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
          }
        ]
      ];

      const expected_prediction: ProbabilityMass<Suggestion> = {
        sample: {
          transform: {
            insert: 'golden apple',
            deleteLeft: 0
          },
          displayAs: 'golden apple'
        }, p: dummied_suggestion_sequences.map((dist) => {
          return dist[0]
        }).reduce((accum, curr) => {
          return accum * (curr ? curr.p : Math.exp(-EDIT_DISTANCE_COST_SCALE))
        }, 1)
      }

      const model = new DummyModel({
        ...DUMMY_MODEL_CONFIG,
        futureSuggestions: dummied_suggestion_sequences
      });

      const predictions = predictFromCorrectionSequence(model, correctionSequence, context);
      predictions.forEach((entry) => assert.equal(entry.correction.sample, 'golden app'));
      predictions.forEach((entry) => assert.equal(entry.correction.p, correctionSequence.reduce((accum, curr) => accum * curr.p, 1)));
      predictions.sort(tupleDisplayOrderSort);

      assert.equal(predictions[0].prediction.sample.transform.insert, 'golden apple');
      assert.sameDeepOrderedMembers(predictions.map((entry) => entry.prediction.sample), [expected_prediction.sample]);

      assert.approximately(predictions[0].prediction.p, expected_prediction.p, 0.00001);
    });

    it('returns no results if all correction tokens lack predictions', () => {
      const context: Context = {
        left: 'i want to eat a ',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const correctionSequence: Distribution<Transform> = [
        {
          sample: {
            insert: 'golden',
            deleteLeft: 0
          },
          p: 0.1
        }, {
          sample: {
            insert: ' ',
            deleteLeft: 0
          },
          p: 0.2
        }, {
          sample: {
            insert: 'app',
            deleteLeft: 0
          },
          p: 0.2
        }
      ];

      const dummied_suggestion_sequences: Outcome<Suggestion>[][] = [
        [],
        [],
        []
      ];

      const model = new DummyModel({
        ...DUMMY_MODEL_CONFIG,
        futureSuggestions: dummied_suggestion_sequences
      });

      const predictions = predictFromCorrectionSequence(model, correctionSequence, context);
      assert.deepEqual(predictions, []);
    });

    it('uses only the best suggestion for non-final corrected tokens', () => {
      const context: Context = {
        left: 'i want to eat a ',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const correctionSequence: Distribution<Transform> = [
        {
          sample: {
            insert: 'g',
            deleteLeft: 0
          },
          p: 0.1
        }, {
          sample: {
            insert: ' ',
            deleteLeft: 0
          },
          p: 0.2
        }, {
          sample: {
            insert: 'app',
            deleteLeft: 0
          },
          p: 0.2
        }
      ];

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

      const expected_prediction: ProbabilityMass<Suggestion> = {
        sample: {
          transform: {
            insert: 'golden apple',
            deleteLeft: 0
          },
          displayAs: 'golden apple'
        }, p: dummied_suggestion_sequences.map((dist) => {
          return dist[0]
        }).reduce((accum, curr) => {
          return accum * (curr ? curr.p : Math.exp(-EDIT_DISTANCE_COST_SCALE))
        }, 1)
      }

      const model = new DummyModel({
        ...DUMMY_MODEL_CONFIG,
        futureSuggestions: dummied_suggestion_sequences
      });

      const predictions = predictFromCorrectionSequence(model, correctionSequence, context);
      // There should be no variations with 'green' or 'gray' apples.
      assert.equal(predictions.length, 1);

      predictions.forEach((entry) => assert.equal(entry.correction.sample, 'g app'));
      predictions.forEach((entry) => assert.equal(entry.correction.p, correctionSequence.reduce((accum, curr) => accum * curr.p, 1)));
      predictions.sort(tupleDisplayOrderSort);

      assert.equal(predictions[0].prediction.sample.transform.insert, 'golden apple');
      assert.sameDeepOrderedMembers(predictions.map((entry) => entry.prediction.sample), [expected_prediction.sample]);

      assert.approximately(predictions[0].prediction.p, expected_prediction.p, 0.00001);
    });

    it('uses all suggestions generated from context-final correction-tokens', () => {
      const context: Context = {
        left: 'i want to eat a ',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const correctionSequence: Distribution<Transform> = [
        {
          sample: {
            insert: 'golden',
            deleteLeft: 0
          },
          p: 0.1
        }, {
          sample: {
            insert: ' ',
            deleteLeft: 0
          },
          p: 0.2
        }, {
          sample: {
            insert: 'app',
            deleteLeft: 0
          },
          p: 0.2
        }
      ];

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
      const expected_predictions: ProbabilityMass<Suggestion>[] = dummied_suggestion_sequences[tailIndex].map((p) => {
        const expectedText = `golden ${p.transform.insert}`;

        return {
          sample: {
            transform: {
              insert: expectedText,
              deleteLeft: 0
            },
            displayAs: expectedText
          }, p: dummied_suggestion_sequences.map((dist) => {
            return dist[0]
          }).reduce((accum, curr, index) => {
            if(tailIndex == index) {
              return accum * p.p;
            } else {
              return accum * (curr ? curr.p : Math.exp(-EDIT_DISTANCE_COST_SCALE))
            }
          }, 1)
        };
      });

      const model = new DummyModel({
        ...DUMMY_MODEL_CONFIG,
        futureSuggestions: dummied_suggestion_sequences
      });

      const predictions = predictFromCorrectionSequence(model, correctionSequence, context);
      assert.equal(predictions.length, dummied_suggestion_sequences[dummied_suggestion_sequences.length - 1].length);

      predictions.forEach((entry) => assert.equal(entry.correction.sample, 'golden app'));
      predictions.forEach((entry) => assert.equal(entry.correction.p, correctionSequence.reduce((accum, curr) => accum * curr.p, 1)));
      predictions.sort(tupleDisplayOrderSort);

      assert.sameOrderedMembers(
        predictions.map((t) => t.prediction.sample.transform.insert),
        ['golden apple', 'golden application', 'golden appetizer']
      );
      assert.sameDeepOrderedMembers(predictions.map((entry) => entry.prediction.sample), expected_predictions.map((p => p.sample)));

      for(let i = 0; i < predictions.length; i++) {
        assert.approximately(predictions[i].prediction.p, expected_predictions[i].p, 0.00001, `Expected probabilty mismatch at index ${i}`);
      }
    });
  });
});