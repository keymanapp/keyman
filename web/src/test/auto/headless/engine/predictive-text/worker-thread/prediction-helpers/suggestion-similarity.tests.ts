import { assert } from 'chai';

import { QuoteBehavior } from "@keymanapp/models-templates";
import * as wordBreakers from '@keymanapp/models-wordbreakers';
import { deepCopy } from 'keyman/common/web-utils';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { IntermediateCompositedPrediction, models, processSimilarity, SuggestionSimilarity, toAnnotatedSuggestion } from "@keymanapp/lm-worker/test-index";

import CasingFunction = LexicalModelTypes.CasingFunction;
import Context = LexicalModelTypes.Context;
import DummyModel = models.DummyModel;
import DummyOptions = models.DummyOptions;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

/*
 * This file's tests use these parts of a lexical model:
 * - model.wordbreaker
 * - model.toKey
 * - model.applyCasing
 * - model.punctuation
 */

const DUMMY_MODEL_CONFIG: DummyOptions = {
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

describe('processSimilarity', () => {
  it(`selects non-contraction as 'more similar' than same-keyed contraction when context is non-contraction`, () => {
    const context: Context = {
      left: 'It',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const trueInput: ProbabilityMass<Transform> = {
      sample: {
        insert: 's',
        deleteLeft: 0
      },
      p: 1
    };

    const testSet = build_its_is_set();
    const distribution = [...Object.values(testSet)];

    const expectation: IntermediateCompositedPrediction[] = [...Object.values(testSet)];
    expectation[0].metadata.matchLevel = SuggestionSimilarity.exact;    // its
    expectation[1].metadata.matchLevel = SuggestionSimilarity.sameKey;  // it_is
    expectation[2].metadata.matchLevel = SuggestionSimilarity.none;     // is
    expectation[3].metadata.matchLevel = SuggestionSimilarity.none;     // is_not

    const its = testSet.its;
    const original_its = deepCopy(its);
    const keep_its = toAnnotatedSuggestion(testModelWithCasing, original_its.components.prediction, 'keep', QuoteBehavior.noQuotes);
    keep_its.matchesModel = true;

    processSimilarity(testModelWithCasing, distribution, context, trueInput);

    assert.sameDeepMembers(distribution, expectation);
    assert.equal(its.components.prediction.tag, 'keep');
    assert.deepEqual(its.components.prediction, keep_its);
  });

  it(`selects contraction as 'more similar' than same-keyed non-contraction when context is contraction`, () => {
    const context: Context = {
      left: 'It',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const trueInput: ProbabilityMass<Transform> = {
      sample: {
        insert: '\'s',
        deleteLeft: 0
      },
      p: 1
    };

    const testSet = build_its_is_set();
    const distribution = [...Object.values(testSet)];

    const expectation: IntermediateCompositedPrediction[] = [...Object.values(testSet)];
    expectation[0].metadata.matchLevel = SuggestionSimilarity.sameKey;  // its
    expectation[1].metadata.matchLevel = SuggestionSimilarity.exact;    // it_is
    expectation[2].metadata.matchLevel = SuggestionSimilarity.none;     // is
    expectation[3].metadata.matchLevel = SuggestionSimilarity.none;     // is_not

    const it_is = testSet.it_is;
    const original_it_is = deepCopy(it_is);
    const keep_it_is = toAnnotatedSuggestion(testModelWithCasing, original_it_is.components.prediction, 'keep', QuoteBehavior.noQuotes);
    keep_it_is.matchesModel = true;

    processSimilarity(testModelWithCasing, distribution, context, trueInput);

    assert.sameDeepMembers(distribution, expectation);
    assert.equal(it_is.components.prediction.tag, 'keep');
    assert.deepEqual(it_is.components.prediction, keep_it_is);
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
      const context: Context = {
        left: 'It',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const trueInput: ProbabilityMass<Transform> = {
        sample: {
          insert: '\'s',
          deleteLeft: 0
        },
        p: 1
      };

      const testSet = build_its_is_set();

      // Have the predictions replace existing context parts with the lowercased equivalents.
      Object.values(testSet).forEach((entry) => {
        const transform = entry.components.prediction.transform;
        transform.insert = transform.deleteLeft == 0 ? `it${transform.insert}` : `i${transform.insert}`;
        transform.deleteLeft = 2;
      });

      const distribution = [...Object.values(testSet)];

      const expectation: IntermediateCompositedPrediction[] = [...Object.values(testSet)];
      expectation[0].metadata.matchLevel = SuggestionSimilarity.sameKey;   // its
      expectation[1].metadata.matchLevel = SuggestionSimilarity.sameText;  // it_is
      expectation[2].metadata.matchLevel = SuggestionSimilarity.none;      // is
      expectation[3].metadata.matchLevel = SuggestionSimilarity.none;      // is_not
      processSimilarity(testModelWithCasing, distribution, context, trueInput);

      // Because we mucked with the casing here, there is no perfect 'keep' match.
      const keep = distribution.find((entry) => entry.components.prediction.tag == 'keep');
      assert.isNotOk(keep);
      assert.sameDeepMembers(distribution, expectation);
    });
  });

  describe('without casing', () => {
    it(`does not utilize casing behaviors when checking similarity`, () => {
      const context: Context = {
        left: 'It',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      };

      const trueInput: ProbabilityMass<Transform> = {
        sample: {
          insert: '\'s',
          deleteLeft: 0
        },
        p: 1
      };

      const testSet = build_its_is_set();

      // Have the predictions replace existing context parts with the lowercased equivalents.
      Object.values(testSet).forEach((entry) => {
        const transform = entry.components.prediction.transform;
        transform.insert = transform.deleteLeft == 0 ? `it${transform.insert}` : `i${transform.insert}`;
        transform.deleteLeft = 2;
      });

      const distribution = [...Object.values(testSet)];

      const expectation: IntermediateCompositedPrediction[] = [...Object.values(testSet)];

      expectation.forEach((entry) => entry.metadata.matchLevel = SuggestionSimilarity.none);
      processSimilarity(testModelWithoutCasing, distribution, context, trueInput);

      // Because we mucked with the casing here, there is no perfect 'keep' match.
      const keep = distribution.find((entry) => entry.components.prediction.tag == 'keep');
      assert.isNotOk(keep);
      assert.sameDeepMembers(distribution, expectation);
    });
  });
});