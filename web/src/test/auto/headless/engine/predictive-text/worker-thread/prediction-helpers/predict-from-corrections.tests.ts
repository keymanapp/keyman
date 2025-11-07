
import { assert } from 'chai';

import { deepCopy } from "keyman/common/web-utils";
import { LexicalModelTypes } from '@keymanapp/common-types';

import { models, predictFromCorrections, tupleDisplayOrderSort } from "@keymanapp/lm-worker/test-index";

import CasingFunction = LexicalModelTypes.CasingFunction;
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import DummyModel = models.DummyModel;
import Outcome = LexicalModelTypes.Outcome;
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

describe('predictFromCorrections', () => {
  it('handles a single correction prefixing multiple entries - no transform ID', () => {
    const context: Context = {
      left: 'It',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const correctionDistribution: Distribution<Transform> = [{
        sample: {
          insert: 's',
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

    const predictions = predictFromCorrections(model, correctionDistribution, context);
    predictions.forEach((entry) => assert.equal(entry.correction.sample, 'Its'));
    predictions.forEach((entry) => assert.equal(entry.correction.p, 0.6));
    predictions.sort(tupleDisplayOrderSort);

    assert.sameDeepOrderedMembers(predictions.map((entry) => entry.prediction.sample), dummied_suggestions);

    assert.approximately(predictions[0].totalProb, 0.18 * 0.6, 0.00001);
    assert.approximately(predictions[1].totalProb, 0.02 * 0.6, 0.00001);
  });

  it('handles a single correction prefixing multiple entries - with transform ID', () => {
    const context: Context = {
      left: 'It',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const correctionDistribution: Distribution<Transform> = [{
        sample: {
          insert: 's',
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

    const predictions = predictFromCorrections(model, correctionDistribution, context);
    predictions.forEach((entry) => assert.equal(entry.correction.sample, 'Its'));
    predictions.forEach((entry) => assert.equal(entry.correction.p, 0.6));
    predictions.sort(tupleDisplayOrderSort);

    assert.sameOrderedMembers(predictions.map((entry) => entry.prediction.sample.displayAs), ["it's", "its"]);
    assert.sameDeepOrderedMembers(predictions.map((entry) => entry.prediction.sample), dummied_suggestions.map((entry) => {
      entry = deepCopy(entry);
      entry.transformId = 314159;
      return entry;
    }));

    assert.approximately(predictions[0].totalProb, 0.18 * 0.6, 0.00001);
    assert.approximately(predictions[1].totalProb, 0.02 * 0.6, 0.00001);
  });

  it('handles multiple corrections at once', () => {
    const context: Context = {
      left: 'It',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    // Note:  each correction is used in order in a separate model.predict call.
    /** @type {Distribution<Transform>} */
    const correctionDistribution: Distribution<Transform> = [{
        // postContext:  is
        sample: {
          insert: 's',
          deleteLeft: 1
        },
        p: 0.4
      }, {
        // postContext: its
        sample: {
          insert: 's',
          deleteLeft: 0
        },
        p: 0.6
      }
    ];

    const dummied_suggestions: Outcome<Suggestion>[][] = [
      // postContext:  is
      [{
        transform: {
          insert: "is",
          deleteLeft: 2
        },
        displayAs: "is",
        p: 0.4
      }, {
        transform: {
          insert: "isn't",
          deleteLeft: 2
        },
        displayAs: "isn't",
        p: 0.2
      }],
      // postContext: its
      [{
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
      }]
    ];

    const model = new DummyModel({
      ...DUMMY_MODEL_CONFIG,
      futureSuggestions: dummied_suggestions
    });

    const predictions = predictFromCorrections(model, correctionDistribution, context);
    predictions.sort(tupleDisplayOrderSort);

    assert.sameOrderedMembers(predictions.map((entry) => entry.prediction.sample.displayAs), ["is", "it's", "isn't", "its"]);
    assert.sameDeepMembers(predictions.map((entry) => entry.prediction.sample), dummied_suggestions.flatMap((entry) => entry));

    assert.approximately(predictions[0].totalProb, 0.4 * 0.4, 0.00001);
    assert.approximately(predictions[1].totalProb, 0.18 * 0.6, 0.00001);
    assert.approximately(predictions[2].totalProb, 0.4 * 0.2, 0.00001);
    assert.approximately(predictions[3].totalProb, 0.02 * 0.6, 0.00001);
  });
});