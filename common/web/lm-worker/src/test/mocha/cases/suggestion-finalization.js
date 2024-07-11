import * as wordBreakers from '@keymanapp/models-wordbreakers';
import { deepCopy } from '@keymanapp/web-utils';
import { assert } from 'chai';

import { finalizeSuggestions } from "#./predict-helpers.js";
import { DummyModel } from "#./models/dummy-model.js";

/*
 * This file's tests use these parts of a lexical model:
 * - model.wordbreaker
 * - model.punctuation
 */

const testModelWithSpacing = new DummyModel({
  wordbreaker: wordBreakers.default,
  punctuation: {
    quotesForKeepSuggestion: {
      open: '<',
      close: '>'
    },
    insertAfterWord: '\u00a0' // non-breaking space
  }
});

const testModelWithoutSpacing = new DummyModel({
  wordbreaker: wordBreakers.default,
  punctuation: {
    quotesForKeepSuggestion: {
      open: '<',
      close: '>'
    },
    insertAfterWord: ''
  }
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

describe('finalizeSuggestions', () => {
  describe('with custom post-token insert', () => {
    it('with caret at end of current token and context', () => {
      /** @type {Context} */
      const context = {
        left: 'It',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      /** @type {ProbabilityMass<Transform>} */
      const transform = {
        insert: 's',
        deleteLeft: 0
      };

      const testSet = build_its_is_set();
      const analyzedSuggestions = [...Object.values(testSet).map((entry) => deepCopy(entry))];

      const finalized = finalizeSuggestions(testModelWithSpacing, analyzedSuggestions, context, transform, false);
      const expected = [...Object.values(testSet)].map((entry) => {
        return {
          ...entry.prediction.sample,
          p: entry.totalProb
        };
      });
      expected.forEach((entry) => entry.transform.insert += testModelWithSpacing.punctuation.insertAfterWord);
      assert.sameDeepOrderedMembers(finalized, expected);
    });

    it('with caret at end of current token, but mid-context', () => {
      /** @type {Context} */
      const context = {
        left: 'It',
        right: `${testModelWithSpacing.punctuation.insertAfterWord}apple`,
        startOfBuffer: true,
        endOfBuffer: true
      };

      /** @type {ProbabilityMass<Transform>} */
      const transform = {
        insert: 's',
        deleteLeft: 0
      };

      const testSet = build_its_is_set();
      const analyzedSuggestions = [...Object.values(testSet).map((entry) => deepCopy(entry))];

      const finalized = finalizeSuggestions(testModelWithSpacing, analyzedSuggestions, context, transform, false);
      const expected = [...Object.values(testSet)].map((entry) => {
        return {
          ...entry.prediction.sample,
          p: entry.totalProb
        };
      });
      // We do not add a whitespace despite not splitting a token if there's a
      // matching whitespace immediately to the caret's right.
      assert.sameDeepOrderedMembers(finalized, expected);
    });

    it('with caret at end of current token, punctuation word-break before next', () => {
      /** @type {Context} */
      const context = {
        left: 'It',
        right: `+apple`,
        startOfBuffer: true,
        endOfBuffer: true
      };

      /** @type {ProbabilityMass<Transform>} */
      const transform = {
        insert: 's',
        deleteLeft: 0
      };

      const testSet = build_its_is_set();
      const analyzedSuggestions = [...Object.values(testSet).map((entry) => deepCopy(entry))];

      const finalized = finalizeSuggestions(testModelWithSpacing, analyzedSuggestions, context, transform, false);
      const expected = [...Object.values(testSet)].map((entry) => {
        return {
          ...entry.prediction.sample,
          p: entry.totalProb
        };
      });

      // The character after the caret isn't the whitespace we'd usually insert,
      // so we don't swallow it this time.
      expected.forEach((entry) => entry.transform.insert += testModelWithSpacing.punctuation.insertAfterWord);
      assert.sameDeepOrderedMembers(finalized, expected);
    });

    it('with caret mid-token', () => {
      // Current behavior - conceptually splits the current token at the current
      // location, then acts as if it's end-token.  The whitespace added at "end
      // of token" actually enforces the split.
      /** @type {Context} */
      const context = {
        left: 'It',
        right: 's',
        startOfBuffer: true,
        endOfBuffer: true
      };

      /** @type {ProbabilityMass<Transform>} */
      const transform = {
        insert: 's',
        deleteLeft: 0
      };

      const testSet = build_its_is_set();
      const analyzedSuggestions = [...Object.values(testSet).map((entry) => deepCopy(entry))];

      const finalized = finalizeSuggestions(testModelWithSpacing, analyzedSuggestions, context, transform, false);
      const expected = [...Object.values(testSet)].map((entry) => {
        return {
          ...entry.prediction.sample,
          p: entry.totalProb
        };
      });
      expected.forEach((entry) => entry.transform.insert += testModelWithSpacing.punctuation.insertAfterWord);
      assert.sameDeepOrderedMembers(finalized, expected);
    });
  });

  describe('without post-token insert text', () => {
    it('with caret at end of current token and context', () => {
      /** @type {Context} */
      const context = {
        left: 'It',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      /** @type {ProbabilityMass<Transform>} */
      const transform = {
        insert: 's',
        deleteLeft: 0
      };

      const testSet = build_its_is_set();
      const analyzedSuggestions = [...Object.values(testSet).map((entry) => deepCopy(entry))];

      const finalized = finalizeSuggestions(testModelWithoutSpacing, analyzedSuggestions, context, transform, false);
      const expected = [...Object.values(testSet)].map((entry) => {
        return {
          ...entry.prediction.sample,
          p: entry.totalProb
        };
      });
      assert.sameDeepOrderedMembers(finalized, expected);
    });

    it('with caret at end of current token, but mid-context', () => {
      /** @type {Context} */
      const context = {
        left: 'It',
        // Including a whitespace here is the easiest way to ensure it doesn't
        // look like we're mid-token to the code.
        right: ' apple',
        startOfBuffer: true,
        endOfBuffer: true
      };

      /** @type {ProbabilityMass<Transform>} */
      const transform = {
        insert: 's',
        deleteLeft: 0
      };

      const testSet = build_its_is_set();
      const analyzedSuggestions = [...Object.values(testSet).map((entry) => deepCopy(entry))];

      const finalized = finalizeSuggestions(testModelWithoutSpacing, analyzedSuggestions, context, transform, false);
      const expected = [...Object.values(testSet)].map((entry) => {
        return {
          ...entry.prediction.sample,
          p: entry.totalProb
        };
      });
      // We do not add a whitespace despite not splitting a token if there's a
      // matching whitespace immediately to the caret's right.
      assert.sameDeepOrderedMembers(finalized, expected);
    });

    it('with caret mid-token', () => {
      // Current behavior - conceptually splits the current token at the current
      // location, then acts as if it's end-token.  The whitespace added at "end
      // of token" actually enforces the split.
      /** @type {Context} */
      const context = {
        left: 'It',
        right: 's',
        startOfBuffer: true,
        endOfBuffer: true
      };

      /** @type {ProbabilityMass<Transform>} */
      const transform = {
        insert: 's',
        deleteLeft: 0
      };

      const testSet = build_its_is_set();
      const analyzedSuggestions = [...Object.values(testSet).map((entry) => deepCopy(entry))];

      const finalized = finalizeSuggestions(testModelWithoutSpacing, analyzedSuggestions, context, transform, false);
      const expected = [...Object.values(testSet)].map((entry) => {
        return {
          ...entry.prediction.sample,
          p: entry.totalProb
        };
      });
      assert.sameDeepOrderedMembers(finalized, expected);
    });
  });

  describe('with output mode', () => {
    it('verbose', () => {
      /** @type {Context} */
      const context = {
        left: 'It',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      /** @type {ProbabilityMass<Transform>} */
      const transform = {
        insert: 's',
        deleteLeft: 0
      };

      const testSet = build_its_is_set();
      const analyzedSuggestions = [...Object.values(testSet).map((entry) => deepCopy(entry))];

      const finalized = finalizeSuggestions(testModelWithSpacing, analyzedSuggestions, context, transform, true);
      const expected = [...Object.values(testSet)].map((entry) => {
        /** @type{
            Suggestion & {
              p?: number;
              "lexical-p"?: number;
              "correction-p"?: number;
              }
            }
         */
        const mapped = {
          ...entry.prediction.sample,
          p: entry.totalProb
        };

        mapped['correction-p'] = entry.correction.p;
        mapped['lexical-p'] = entry.prediction.p;

        return mapped;
      });
      expected.forEach((entry) => entry.transform.insert += testModelWithSpacing.punctuation.insertAfterWord);
      assert.sameDeepOrderedMembers(finalized, expected);
    });

    it('standard', () => {
      /** @type {Context} */
      const context = {
        left: 'It',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      /** @type {ProbabilityMass<Transform>} */
      const transform = {
        insert: 's',
        deleteLeft: 0
      };

      const testSet = build_its_is_set();
      const analyzedSuggestions = [...Object.values(testSet).map((entry) => deepCopy(entry))];

      const finalized = finalizeSuggestions(testModelWithSpacing, analyzedSuggestions, context, transform, false);
      const expected = [...Object.values(testSet)].map((entry) => {
        return {
          ...entry.prediction.sample,
          p: entry.totalProb
        };
      });
      expected.forEach((entry) => entry.transform.insert += testModelWithSpacing.punctuation.insertAfterWord);
      assert.sameDeepOrderedMembers(finalized, expected);
    });
  });
});