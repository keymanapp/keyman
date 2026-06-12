/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-05-18
 *
 * This file tests the prediction helper-method responsible for preparing
 * corrections for multi-token prediction for some custom and all legacy models.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from "@keymanapp/common-types";
import * as wordBreakers from '@keymanapp/models-wordbreakers';
import { KMWString } from 'keyman/common/web-utils';

import { determineTraversallessCorrectionSequences, IntermediateTokenizedPrediction, ModelCompositor, models } from "@keymanapp/lm-worker/test-index";

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

const testModel = new DummyModel({
  ...DUMMY_MODEL_CONFIG,
  // No suggestions needed here, so we don't define any.
});

describe('determineTraversallessCorrectionSequences', () => {
  it(`processes common-case corrections correctly - on context reset with existing text`, () => {
    const context = {
      left: 'appl',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const trueInput: ProbabilityMass<Transform> = {
      sample: {
        insert: '',
        deleteLeft: 0
      },
      p: 1
    };

    const predictionRootEntries = determineTraversallessCorrectionSequences(testModel, [trueInput], context);

    assert.equal(predictionRootEntries.length, 1);
    const entry = predictionRootEntries[0];

    assert.deepEqual(
      {
        ...entry.rootContext, casingForm: entry.rootContext.casingForm ?? undefined
      }, {
        casingForm: undefined,
        left: '',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      }
    );

    assert.deepEqual(entry.tokens, [{
      correction: {
        sample: {
          insert: 'appl',
          deleteLeft: 0
        },
        p: trueInput.p
      },
      casingRoot: 'appl',
      autoSelectable: true
    }]);
  });

  it(`processes standard-case corrections correctly - text appended to existing token`, () => {
    const context: Context = {
      left: 'I want an iPhon',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const trueInput: ProbabilityMass<Transform> = {
      sample: {
        insert: 'e',
        deleteLeft: 0
      },
      p: 1
    };

    const predictionRootEntries = determineTraversallessCorrectionSequences(testModel, [trueInput], context);

    assert.equal(predictionRootEntries.length, 1);
    const entry = predictionRootEntries[0];

    assert.deepEqual(
      {
        ...entry.rootContext, casingForm: entry.rootContext.casingForm ?? undefined
      }, {
        casingForm: undefined,
        left: 'I want an ',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      }
    );

    assert.deepEqual(predictionRootEntries[0].tokens, [
      {
        correction: {
          sample: {
            insert: 'iPhone',
            deleteLeft: 0
          },
          p: trueInput.p
        },
        casingRoot: 'iPhone',
        autoSelectable: true
      }
    ]);
  });

  it(`properly analyzes common-case token-extension - adding a letter to an existing word`, () => {
    const context: Context = {
      left: 'the quick brown f',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const trueInput: ProbabilityMass<Transform> = {
      sample: {
        insert: 'o',
        deleteLeft: 0
      },
      p: .5
    };

    const predictionRootEntries = determineTraversallessCorrectionSequences(testModel, [trueInput], context);
    assert.equal(predictionRootEntries.length, 1);
    const entry = predictionRootEntries[0];

    assert.deepEqual(
      {
        ...entry.rootContext, casingForm: entry.rootContext.casingForm ?? undefined
      }, {
        casingForm: undefined,
        left: 'the quick brown ',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      }
    );

    assert.deepEqual(entry.tokens, [
      {
        correction: {
          sample: {
            insert: 'fo',
            deleteLeft: 0
          },
          p: trueInput.p
        },
        casingRoot: 'fo',
        autoSelectable: true
      }
    ]);
  });

  it(`properly analyzes common-case whitespace - ending a token and adding a new one`, () => {
    const context: Context = {
      left: 'the quick brown',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const trueInput: ProbabilityMass<Transform> = {
      sample: {
        insert: ' ',
        deleteLeft: 0
      },
      p: .5
    };

    const predictionRootEntries = determineTraversallessCorrectionSequences(testModel, [trueInput], context);
    assert.equal(predictionRootEntries.length, 1);
    const entry = predictionRootEntries[0];

    assert.deepEqual(
      {
        ...entry.rootContext, casingForm: entry.rootContext.casingForm ?? undefined
      }, {
        casingForm: undefined,
        left: 'the quick brown',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      }
    );

    assert.equal(entry.tokens.length, 1);
    assert.approximately(entry.tokens[0].correction.p, Math.pow(trueInput.p, ModelCompositor.SINGLE_CHAR_KEY_PROB_EXPONENT), Number.EPSILON*1000);

    assert.deepEqual(entry.tokens, [{
      correction: {
        sample: {
          insert: '',
          deleteLeft: 0
        },
        p: entry.tokens[0].correction.p
      },
      casingRoot: '',
      autoSelectable: false
    }]);
  });


  it(`properly analyzes common-case word-start - beginning a new token`, () => {
    const context: Context = {
      left: 'the quick brown ',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const trueInput: ProbabilityMass<Transform> = {
      sample: {
        insert: 'f',
        deleteLeft: 0
      },
      p: .5
    };

    const predictionRootEntries = determineTraversallessCorrectionSequences(testModel, [trueInput], context);
    assert.equal(predictionRootEntries.length, 1);
    const entry = predictionRootEntries[0];

    assert.deepEqual(
      {
        ...entry.rootContext, casingForm: entry.rootContext.casingForm ?? undefined
      }, {
        casingForm: undefined,
        left: 'the quick brown ',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      }
    );

    assert.equal(entry.tokens.length, 1);
    assert.approximately(entry.tokens[0].correction.p, Math.pow(trueInput.p, ModelCompositor.SINGLE_CHAR_KEY_PROB_EXPONENT), Number.EPSILON*1000);

    assert.deepEqual(entry.tokens, [{
      correction: {
        sample: {
          insert: 'f',
          deleteLeft: 0
        },
        p: entry.tokens[0].correction.p
      },
      casingRoot: 'f',
      autoSelectable: true
    }]);
  });

  it(`properly analyzes post-merge case`, () => {
    let context: Context = {
      left: 'the quick brown fox can\'',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const trueInput: ProbabilityMass<Transform> = {
      sample: {
        insert: 't',
        deleteLeft: 0
      },
      p: .5
    };

    const predictionRootEntries = determineTraversallessCorrectionSequences(testModel, [trueInput], context);
    assert.equal(predictionRootEntries.length, 1);
    const entry = predictionRootEntries[0];

    assert.deepEqual(
      {
        ...entry.rootContext, casingForm: entry.rootContext.casingForm ?? undefined
      }, {
        casingForm: undefined,
        left: 'the quick brown fox ',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      }
    );

    assert.deepEqual(entry.tokens, [{
      correction: {
        sample: {
          insert: 'can\'t',
          deleteLeft: 0
        },
        p: trueInput.p
      },
      casingRoot: 'can\'t',
      autoSelectable: true
    }]);
  });

  it(`properly analyzes post-split new-wordbreak case`, () => {
    const context: Context = {
      left: 'the quick brown fox can\'',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const trueInput: ProbabilityMass<Transform> = {
      sample: {
        insert: ' ',
        deleteLeft: 0
      },
      p: .5
    };

    const predictionRootEntries = determineTraversallessCorrectionSequences(testModel, [trueInput], context);
    assert.equal(predictionRootEntries.length, 1);
    const entry = predictionRootEntries[0];

    // assert.deepEqual(
    //   {
    //     ...entry.rootContext, casingForm: entry.rootContext.casingForm ?? undefined
    //   }, {
    //     casingForm: undefined,
    //     // Proper logic requires full multi-token awareness; predictions are currently
    //     // based on just the last token.
    //     left: 'the quick brown fox can\'',
    //     right: '',
    //     startOfBuffer: true,
    //     endOfBuffer: true
    //   }
    // );

    assert.equal(entry.tokens.length, 1);
    assert.approximately(entry.tokens[0].correction.p, Math.pow(trueInput.p, ModelCompositor.SINGLE_CHAR_KEY_PROB_EXPONENT), Number.EPSILON*1000);

    assert.deepEqual(entry.tokens, [{
      correction: {
        sample: {
          insert: '',
          deleteLeft: 0
        },
        p: entry.tokens[0].correction.p
      },
      casingRoot: '',
      autoSelectable: false
    }]);
  });

  it(`properly analyzes complex transition - multi-token replacement`, () => {
    const context: Context = {
      left: 'the quick brown f',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const trueInput: ProbabilityMass<Transform> = {
      sample: {
        insert: 'fast red d',
        deleteLeft: 'quick brown f'.length
      },
      p: .5
    };

    const predictionRootEntries = determineTraversallessCorrectionSequences(testModel, [trueInput], context);
    assert.equal(predictionRootEntries.length, 1);
    const entry = predictionRootEntries[0];

    assert.deepEqual(
      {
        ...entry.rootContext, casingForm: entry.rootContext.casingForm ?? undefined
      }, {
        casingForm: undefined,
        left: 'the ',
        right: '',
        startOfBuffer: true,
        endOfBuffer: true
      }
    );

    // Coming up next - actually providing ALL correction elements, not just the final one.
    // We're not _quite_ ready for that yet, though.
    assert.equal(entry.tokens.length, 1);
    assert.deepEqual(entry.tokens[0].correction.sample, {
      insert: 'd',
      deleteLeft: 0
    });
    assert.approximately(entry.tokens[0].correction.p, Math.pow(trueInput.p, ModelCompositor.SINGLE_CHAR_KEY_PROB_EXPONENT), Number.EPSILON*1000);

    assert.deepEqual(entry.tokens, [{
      correction: {
        sample: {
          insert: 'd',
          deleteLeft: 0
        },
        p: entry.tokens[0].correction.p
      },
      casingRoot: 'd',
      autoSelectable: true
    }]);

    const dummiedTuple: IntermediateTokenizedPrediction = {
      components: [{
        prediction: {
          transform: { insert: 'dog', deleteLeft: 0 },
          displayAs: 'dog'
        },
        correction: 'd',
        casingRoot: 'd'
      }],
      metadata: {
        probabilities: {
          prediction: .25,
          correction: trueInput.p,
          total: .25 * trueInput.p
        },
        autoSelectable: true
      }
    };

    entry.applyInPost(dummiedTuple);

    assert.deepEqual(dummiedTuple.metadata.preservationTransform, {
      insert: trueInput.sample.insert.substring(0, KMWString.length(trueInput.sample.insert) - 1), // remove the 'd'.
      deleteLeft: trueInput.sample.deleteLeft - 1
    });
  });
});