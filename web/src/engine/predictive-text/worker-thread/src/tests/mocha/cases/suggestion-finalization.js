import * as wordBreakers from '@keymanapp/models-wordbreakers';
import { assert } from 'chai';

import { finalizeSuggestions } from "#./predict-helpers.js";
import { DummyModel } from "#./models/dummy-model.js";
import { deepCopy } from '@keymanapp/web-utils';

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
 * @param {'verbose'=} verbose
 * @returns
 */
const build_its_is_set = (verbose) => {
  const verboseFlag = (verbose == 'verbose' ? true : false);

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

  const baseDefinitions = {
    its,
    it_is,
    is,
    is_not
  }

  const unfinalized = [...Object.values(baseDefinitions)].map((entry) => deepCopy(entry));
  const expected = unfinalized.map((entry) => {

    const mapped = {
      ...deepCopy(entry.prediction.sample),
      p: entry.totalProb
    };

    if(verboseFlag) {
      mapped['correction-p'] = entry.correction.p;
      mapped['lexical-p'] = entry.prediction.p;
    }

    return mapped;
  });

  return { baseDefinitions, unfinalized, expected };
};

describe('finalizeSuggestions', () => {
  describe('for models with custom post-token insert', () => {
    it('adds whitespace when caret is at token + context end', () => {
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

      const { unfinalized, expected } = build_its_is_set();
      const finalized = finalizeSuggestions(testModelWithSpacing, unfinalized, context, transform, false);

      expected.forEach((entry) => entry.transform.insert += testModelWithSpacing.punctuation.insertAfterWord);
      assert.sameDeepOrderedMembers(finalized, expected);
    });

    it('does not add whitespace when caret is followed by whitespace', () => {
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

      const { unfinalized, expected } = build_its_is_set();
      const finalized = finalizeSuggestions(testModelWithSpacing, unfinalized, context, transform, false);

      // We do not add a whitespace despite not splitting a token if there's a
      // matching whitespace immediately to the caret's right.
      assert.sameDeepOrderedMembers(finalized, expected);
    });

    // Not saying this is ideal... just that this is what it's currently set to do.
    // May be possible to improve once better tokenization is ready.
    it('adds whitespace when caret is followed by word-breaking punctuation', () => {
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

      const { unfinalized, expected } = build_its_is_set();
      const finalized = finalizeSuggestions(testModelWithSpacing, unfinalized, context, transform, false);

      // The character after the caret isn't the whitespace we'd usually insert,
      // so we don't swallow it this time.
      expected.forEach((entry) => entry.transform.insert += testModelWithSpacing.punctuation.insertAfterWord);
      assert.sameDeepOrderedMembers(finalized, expected);
    });

    it('adds whitespace when caret is mid-token', () => {
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

      const { unfinalized, expected } = build_its_is_set();
      const finalized = finalizeSuggestions(testModelWithSpacing, unfinalized, context, transform, false);

      expected.forEach((entry) => entry.transform.insert += testModelWithSpacing.punctuation.insertAfterWord);
      assert.sameDeepOrderedMembers(finalized, expected);
    });
  });

  describe('for models without post-token insert text', () => {
    it('does not add whitespace when caret is at token + context end', () => {
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

      const { unfinalized, expected } = build_its_is_set();
      const finalized = finalizeSuggestions(testModelWithoutSpacing, unfinalized, context, transform, false);

      assert.sameDeepOrderedMembers(finalized, expected);
    });

    it('does not add whitespace when caret is followed by whitespace', () => {
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


      const { unfinalized, expected } = build_its_is_set();
      const finalized = finalizeSuggestions(testModelWithoutSpacing, unfinalized, context, transform, false);

      // We do not add a whitespace despite not splitting a token if there's a
      // matching whitespace immediately to the caret's right.
      assert.sameDeepOrderedMembers(finalized, expected);
    });

    it('does not add whitespace when caret is mid-token', () => {
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


      const { unfinalized, expected } = build_its_is_set();
      const finalized = finalizeSuggestions(testModelWithoutSpacing, unfinalized, context, transform, false);

      assert.sameDeepOrderedMembers(finalized, expected);
    });
  });

  describe('suggestion output-mode configuration', () => {
    it('adds extra data to suggestions when set to "verbose"', () => {
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


      const { unfinalized, expected } = build_its_is_set('verbose');
      const finalized = finalizeSuggestions(testModelWithSpacing, unfinalized, context, transform, /* verbose */ true);

      expected.forEach((entry) => entry.transform.insert += testModelWithSpacing.punctuation.insertAfterWord);
      assert.sameDeepOrderedMembers(finalized, expected);
    });

    it('does not add extra data to suggestions when not "verbose"', () => {
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


      const { unfinalized, expected } = build_its_is_set();
      const finalized = finalizeSuggestions(testModelWithSpacing, unfinalized, context, transform, false);

      expected.forEach((entry) => entry.transform.insert += testModelWithSpacing.punctuation.insertAfterWord);
      assert.sameDeepOrderedMembers(finalized, expected);
    });
  });
});