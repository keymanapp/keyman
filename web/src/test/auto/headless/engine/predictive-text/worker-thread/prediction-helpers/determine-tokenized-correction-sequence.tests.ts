/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-05-19
 *
 * This file tests the prediction helper-method responsible for preparing
 * corrections for multi-token prediction for our standard models, all of which
 * utilize LexiconTraversals and the context-tokenization-caching subsystem.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from "@keymanapp/common-types";
import * as wordBreakers from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { KMWString } from 'keyman/common/web-utils';

import { determineTokenizedCorrectionSequence, models, ContextState, ContextToken, ContextTokenization, CorrectionPredictionTuple } from "@keymanapp/lm-worker/test-index";

import Context = LexicalModelTypes.Context;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

const testModel = new TrieModel(
  jsonFixture('models/tries/english-1000'), {
    wordBreaker: wordBreakers.default,
  }
);

describe('determineTokenizedCorrectionSequence', () => {
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

    const state = new ContextState(context, testModel);
    const transition = state.analyzeTransition(context, [trueInput]);


    const results = determineTokenizedCorrectionSequence(
      transition,
      transition.final.displayTokenization, {
        matchString: 'fo',
        inputSamplingCost: -Math.log(trueInput.p),
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      },
      1
    );

    assert.deepEqual({...results.rootContext, casingForm: results.rootContext.casingForm}, {
      casingForm: undefined,
      left: 'the quick brown ',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    });

    assert.deepEqual(results.tokenizedCorrection, [
      {
        sample: {
          insert: 'fo',
          deleteLeft: 0
        },
        p: trueInput.p
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

    const state = new ContextState(context, testModel);
    const transition = state.analyzeTransition(context, [trueInput]);


    const results = determineTokenizedCorrectionSequence(
      transition,
      transition.final.displayTokenization, {
        matchString: ' ',
        inputSamplingCost: -Math.log(trueInput.p),
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      },
      1
    );

    assert.deepEqual({...results.rootContext, casingForm: results.rootContext.casingForm}, {
      casingForm: undefined,
      left: 'the quick brown',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    });

    assert.deepEqual(results.tokenizedCorrection, [
      {
        sample: {
          insert: ' ',
          deleteLeft: 0
        },
        p: trueInput.p
      }
    ]);
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

    const state = new ContextState(context, testModel);
    const transition = state.analyzeTransition(context, [trueInput]);


    const results = determineTokenizedCorrectionSequence(
      transition,
      transition.final.displayTokenization, {
        matchString: 'f',
        inputSamplingCost: -Math.log(trueInput.p),
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      },
      1
    );

    assert.deepEqual({...results.rootContext, casingForm: results.rootContext.casingForm}, {
      casingForm: undefined,
      left: 'the quick brown ',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    });

    assert.deepEqual(results.tokenizedCorrection, [
      {
        sample: {
          insert: 'f',
          deleteLeft: 0
        },
        p: trueInput.p
      }
    ]);
  });

  it(`properly analyzes post-merge case`, () => {
    let context: Context = {
      left: 'the quick brown fox ',
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

    const constructingState = new ContextState(context, testModel);
    const tokens = constructingState.displayTokenization.tokens;
    tokens.push(ContextToken.fromRawText(testModel, 'can'));
    tokens.push(ContextToken.fromRawText(testModel, '\''));

    context = models.applyTransform({insert: 'can\'', deleteLeft: 0}, context);

    const state = new ContextState(context, testModel, new ContextTokenization(tokens));
    const transition = state.analyzeTransition(context, [trueInput]);

    const results = determineTokenizedCorrectionSequence(
      transition,
      transition.final.displayTokenization, {
        matchString: 'can\'t',
        inputSamplingCost: -Math.log(trueInput.p),
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      },
      1
    );

    assert.deepEqual({...results.rootContext, casingForm: results.rootContext.casingForm}, {
      casingForm: undefined,
      left: 'the quick brown fox ',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    });

    assert.deepEqual(results.tokenizedCorrection, [
      {
        sample: {
          insert: 'can\'t',
          deleteLeft: 0
        },
        p: trueInput.p
      }
    ]);
  });

  // Will be handled far better after resolving multi-tokenization handling.
  it.skip(`properly analyzes post-split case`, () => {
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

    const state = new ContextState(context, testModel);
    assert.equal(state.displayTokenization.tail.exampleInput, 'can\'');
    const transition = state.analyzeTransition(context, [trueInput]);

    const results = determineTokenizedCorrectionSequence(
      transition,
      transition.final.displayTokenization, {
        matchString: ' ',
        inputSamplingCost: -Math.log(trueInput.p),
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      },
      1
    );

    assert.deepEqual({...results.rootContext, casingForm: results.rootContext.casingForm}, {
      casingForm: undefined,
      left: 'the quick brown fox ',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    });

    assert.deepEqual(results.tokenizedCorrection, [
      {
        sample: {
          insert: ' ',
          deleteLeft: 0
        },
        p: trueInput.p
      }
    ]);
  });

  it(`properly analyzes conplex transition - multi-token replacement`, () => {
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

    const state = new ContextState(context, testModel);
    const transition = state.analyzeTransition(context, [trueInput]);

    const results = determineTokenizedCorrectionSequence(
      transition,
      transition.final.displayTokenization, {
        matchString: 'd',
        inputSamplingCost: -Math.log(trueInput.p),
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      },
      1
    );

    // Large-scale deletions will receive enhanced handling soon.  But, for now, it's
    // deleted by the `preservationTransform`, not here.
    assert.deepEqual({...results.rootContext, casingForm: results.rootContext.casingForm}, {
      casingForm: undefined,
      left: 'the quick brown ',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    });

    assert.deepEqual(results.tokenizedCorrection, [
      {
        sample: {
          insert: 'd',
          deleteLeft: 0
        },
        p: trueInput.p
      }
    ]);

    const dummiedTuple: CorrectionPredictionTuple = {
      prediction: {
        sample: {
          transform: { insert: 'dog', deleteLeft: 0 },
          displayAs: 'dog'
        },
        p: .25
      },
      correction: {
        sample: 'd',
        p: trueInput.p
      },
      totalProb: .25 * trueInput.p
    };

    results.applyInPost(dummiedTuple);

    assert.deepEqual(dummiedTuple.preservationTransform, {
      insert: trueInput.sample.insert.substring(0, KMWString.length(trueInput.sample.insert) - 1), // remove the 'd'.
      deleteLeft: trueInput.sample.deleteLeft - 1
    });
  });
});