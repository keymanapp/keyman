import { QuoteBehavior } from "@keymanapp/models-templates";
import * as wordBreakers from '@keymanapp/models-wordbreakers';
import { deepCopy } from '@keymanapp/web-utils';
import { assert } from 'chai';

import { SuggestionSimilarity, processSimilarity, toAnnotatedSuggestion } from "#./predict-helpers.js";
import { DummyModel } from "#./models/dummy-model.js";

/*
 * This file's tests use these parts of a lexical model:
 * - model.wordbreaker
 * - model.toKey
 * - model.applyCasing
 * - model.punctuation
 */

/** @type { import("#./models/dummy-model.js").DummyOptions } */
const DUMMY_MODEL_CONFIG = {
  punctuation: {
    quotesForKeepSuggestion: {
      open: '<',
      close: '>'
    },
    insertAfterWord: '\u00a0' // non-breaking space
  },
  wordbreaker: wordBreakers.default
};

// See: developer/src/kmc-model/model-defaults.ts, defaultApplyCasing
const applyCasing = (casing, text) => {
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

const testModelWithoutCasing = new DummyModel({
  ...DUMMY_MODEL_CONFIG,
  toKey: (wordform) => {
    // See: developer/src/kmc-model/model-defaults.ts, defaultSearchTermToKey
    return wordform
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
  }
  // No suggestions needed here, so we don't define any.
});

const testModelWithCasing = new DummyModel({
  ...DUMMY_MODEL_CONFIG,
  applyCasing: applyCasing,
  toKey: (wordform) => {
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
  // No suggestions needed here, so we don't define any.
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

describe('processSimilarity', () => {
  it(`selects non-contraction as 'more similar' than same-keyed contraction when context is non-contraction`, () => {
    /** @type {Context} */
    const context = {
      left: 'It',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    /** @type {ProbabilityMass<Transform>} */
    const trueInput = {
      sample: {
        insert: 's',
        deleteLeft: 0
      },
      p: 1
    };

    const testSet = build_its_is_set();
    const distribution = [...Object.values(testSet)];

    /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]} */
    const expectation = [
      {
        ...testSet.its,
        matchLevel: SuggestionSimilarity.exact
      }, {
        ...testSet.it_is,
        matchLevel: SuggestionSimilarity.sameKey
      }, {
        ...testSet.is,
        matchLevel: SuggestionSimilarity.none
      }, {
        ...testSet.is_not,
        matchLevel: SuggestionSimilarity.none
      }
    ];

    const its = testSet.its;
    const original_its = deepCopy(its);
    const keep_its = toAnnotatedSuggestion(testModelWithCasing, original_its.prediction.sample, 'keep', QuoteBehavior.noQuotes);
    keep_its.matchesModel = true;

    processSimilarity(testModelWithCasing, distribution, context, trueInput);

    assert.sameDeepMembers(distribution, expectation);
    assert.equal(its.prediction.sample.tag, 'keep');
    assert.deepEqual(its.prediction.sample, keep_its);
  });

  it(`selects contraction as 'more similar' than same-keyed non-contraction when context is contraction`, () => {
    /** @type {Context} */
    const context = {
      left: 'It',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    /** @type {ProbabilityMass<Transform>} */
    const trueInput = {
      sample: {
        insert: '\'s',
        deleteLeft: 0
      },
      p: 1
    };

    const testSet = build_its_is_set();
    const distribution = [...Object.values(testSet)];

    /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]} */
    const expectation = [
      {
        ...testSet.its,
        matchLevel: SuggestionSimilarity.sameKey
      }, {
        ...testSet.it_is,
        matchLevel: SuggestionSimilarity.exact
      }, {
        ...testSet.is,
        matchLevel: SuggestionSimilarity.none
      }, {
        ...testSet.is_not,
        matchLevel: SuggestionSimilarity.none
      }
    ];

    const it_is = testSet.it_is;
    const original_it_is = deepCopy(it_is);
    const keep_it_is = toAnnotatedSuggestion(testModelWithCasing, original_it_is.prediction.sample, 'keep', QuoteBehavior.noQuotes);
    keep_it_is.matchesModel = true;

    processSimilarity(testModelWithCasing, distribution, context, trueInput);

    assert.sameDeepMembers(distribution, expectation);
    assert.equal(it_is.prediction.sample.tag, 'keep');
    assert.deepEqual(it_is.prediction.sample, keep_it_is);
  });

  it(`creates an 'exact'-match suggestion as 'keep' if no exact-match exists`, () => {
    /** @type {Context} */
    const context = {
      left: 'iphon',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    /** @type {ProbabilityMass<Transform>} */
    const trueInput = {
      sample: {
        insert: 'e',
        deleteLeft: 0
      },
      p: 1
    };

    /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple} */
    const iPhone = {
      correction: {
        sample: 'iphone',
        p: 0.8
      },
      prediction: {
        sample: {
          transform: {
            insert: 'iPhone',
            deleteLeft: 5
          },
          displayAs: 'iPhone'
        },
        p: 0.8
      },
      totalProb: 0.64
      // matchLevel does not yet exist.
    };

    /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]} */
    const distribution = [
      iPhone
    ];

    /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple} */
    const keep_iphone = {
      correction: {
        sample: 'iphone',
        p: 1
      },
      prediction: {
        sample: {
          transform: {
            insert: 'iphone',
            deleteLeft: 5
          },
          displayAs: '<iphone>',
          matchesModel: false,
          tag: 'keep',
          p: 1
        },
        p: 1
      },
      totalProb: 1,
      matchLevel: SuggestionSimilarity.exact
    };


    /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]} */
    const expectation = [
      {
        ...keep_iphone,
        matchLevel: SuggestionSimilarity.exact
      }, {
        ...iPhone,
        matchLevel: SuggestionSimilarity.sameText
      }
    ];

    processSimilarity(testModelWithCasing, distribution, context, trueInput);
    assert.sameDeepMembers(distribution, expectation);
  });

  describe('with casing', () => {
    // If we ever add a mode that can force lowercase for certain words even
    // when the context is title-cased or upper-cased, this scenario would be
    // particularly relevant.
    //
    // Could also test with lowercased context 'apple' => "Apple" (the company)
    // as an entry; this scenario can actually happen in production if the model
    // keeps a separate entry for the two versions (or only has the title-cased
    // one).
    it(`differentiates same-keyed suggestions when one only mismatches due to casing`, () => {
      /** @type {Context} */
      const context = {
        left: 'It',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      /** @type {ProbabilityMass<Transform>} */
      const trueInput = {
        sample: {
          insert: '\'s',
          deleteLeft: 0
        },
        p: 1
      };

      const testSet = build_its_is_set();

      // Have the predictions replace existing context parts with the lowercased equivalents.
      Object.values(testSet).forEach((entry) => {
        const transform = entry.prediction.sample.transform;
        transform.insert = transform.deleteLeft == 0 ? `it${transform.insert}` : `i${transform.insert}`;
        transform.deleteLeft = 2;
      });

      const distribution = [...Object.values(testSet)];

      /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]} */
      const expectation = [
        {
          ...testSet.its,
          matchLevel: SuggestionSimilarity.sameKey
        }, {
          ...testSet.it_is,
          // case mismatch, detectable because we have access to a lowercasing/uppercasing function.
          matchLevel: SuggestionSimilarity.sameText
        }, {
          ...testSet.is,
          matchLevel: SuggestionSimilarity.none
        }, {
          ...testSet.is_not,
          matchLevel: SuggestionSimilarity.none
        }
      ];

      processSimilarity(testModelWithCasing, distribution, context, trueInput);

      // Because we mucked with the casing here, a new 'keep' was generated.
      // Find it, confirm it exists and meets basic expectations, then remove it
      // for easy comparison to pre-existing entries.
      //
      // We'll be less thorough checking this 'keep', as the "creates an 'exact'..."
      // test above is thorough enough and tests the behavior already.

      const keep = distribution.find((entry) => entry.prediction.sample.tag == 'keep');
      assert.isOk(keep);
      assert.equal(keep.prediction.sample.displayAs, '<It\'s>');

      distribution.splice(distribution.indexOf(keep), 1);
      assert.sameDeepMembers(distribution, expectation);
    });
  });

  describe('without casing', () => {
    it(`does not utilize casing behaviors when checking similarity`, () => {
      /** @type {Context} */
      const context = {
        left: 'It',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      /** @type {ProbabilityMass<Transform>} */
      const trueInput = {
        sample: {
          insert: '\'s',
          deleteLeft: 0
        },
        p: 1
      };

      const testSet = build_its_is_set();

      // Have the predictions replace existing context parts with the lowercased equivalents.
      Object.values(testSet).forEach((entry) => {
        const transform = entry.prediction.sample.transform;
        transform.insert = transform.deleteLeft == 0 ? `it${transform.insert}` : `i${transform.insert}`;
        transform.deleteLeft = 2;
      });

      const distribution = [...Object.values(testSet)];

      /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]} */
      const expectation = [
        {
          ...testSet.its,
          matchLevel: SuggestionSimilarity.none
        }, {
          ...testSet.it_is,
          // case mismatch, detectable because we have access to a lowercasing/uppercasing function.
          matchLevel: SuggestionSimilarity.none
        }, {
          ...testSet.is,
          matchLevel: SuggestionSimilarity.none
        }, {
          ...testSet.is_not,
          matchLevel: SuggestionSimilarity.none
        }
      ];

      processSimilarity(testModelWithoutCasing, distribution, context, trueInput);

      // Because we mucked with the casing here, a new 'keep' was generated.
      // Find it, confirm it exists and meets basic expectations, then remove it
      // for easy comparison to pre-existing entries.
      //
      // We'll be less thorough checking this 'keep', as the "creates an 'exact'..."
      // test above is thorough enough and tests the behavior already.

      const keep = distribution.find((entry) => entry.prediction.sample.tag == 'keep');
      assert.isOk(keep);
      assert.equal(keep.prediction.sample.displayAs, '<It\'s>');

      distribution.splice(distribution.indexOf(keep), 1);
      assert.sameDeepMembers(distribution, expectation);
    });
  });
});