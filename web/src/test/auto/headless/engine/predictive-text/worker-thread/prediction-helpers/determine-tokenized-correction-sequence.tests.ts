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

import {
  determineTokenizedCorrectionSequence,
  models,
  ContextState,
  ContextToken,
  ContextTokenization,
  CompositedIntermediatePrediction,
  ModelCompositor,
  TokenizationResultMapping
} from "@keymanapp/lm-worker/test-index";

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
      transition.final.displayTokenization,
      new TokenizationResultMapping([{
        matchString: 'fo',
        inputSamplingCost: -Math.log(trueInput.p),
        inputCount: 2,
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      }], null)
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
      transition.final.displayTokenization,
      new TokenizationResultMapping([{
        matchString: ' ',
        inputSamplingCost: -Math.log(trueInput.p),
        inputCount: 1,
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      }], null)
    );

    assert.deepEqual({...results.rootContext, casingForm: results.rootContext.casingForm}, {
      casingForm: undefined,
      left: 'the quick brown',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    });

    assert.equal(results.tokenizedCorrection.length, 1);
    assert.deepEqual(results.tokenizedCorrection[0].sample, {
      insert: ' ',
      deleteLeft: 0
    });
    assert.approximately(results.tokenizedCorrection[0].p, Math.pow(trueInput.p, ModelCompositor.SINGLE_CHAR_KEY_PROB_EXPONENT), Number.EPSILON*1000);
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
      transition.final.displayTokenization,
      new TokenizationResultMapping([{
        matchString: 'f',
        inputSamplingCost: -Math.log(trueInput.p),
        inputCount: 1,
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      }], null)
    );

    assert.deepEqual({...results.rootContext, casingForm: results.rootContext.casingForm}, {
      casingForm: undefined,
      left: 'the quick brown ',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    });


    assert.equal(results.tokenizedCorrection.length, 1);
    assert.deepEqual(results.tokenizedCorrection[0].sample, {
      insert: 'f',
      deleteLeft: 0
    });
    assert.approximately(results.tokenizedCorrection[0].p, Math.pow(trueInput.p, ModelCompositor.SINGLE_CHAR_KEY_PROB_EXPONENT), Number.EPSILON*1000);
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
      transition.final.displayTokenization,
      new TokenizationResultMapping([{
        matchString: 'can\'t',
        inputSamplingCost: -Math.log(trueInput.p),
        inputCount: 5,
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      }], null)
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
  it.skip(`properly analyzes post-split new-wordbreak case`, () => {
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
      transition.final.displayTokenization,
      new TokenizationResultMapping([{
        matchString: ' ',
        inputSamplingCost: -Math.log(trueInput.p),
        inputCount: 1,
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      }], null)
    );

    assert.deepEqual({...results.rootContext, casingForm: results.rootContext.casingForm}, {
      casingForm: undefined,
      left: 'the quick brown fox ',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    });


    assert.equal(results.tokenizedCorrection.length, 1);
    assert.deepEqual(results.tokenizedCorrection[0].sample, {
      insert: ' ',
      deleteLeft: 0
    });
    assert.approximately(results.tokenizedCorrection[0].p, Math.pow(trueInput.p, ModelCompositor.SINGLE_CHAR_KEY_PROB_EXPONENT), Number.EPSILON*1000);
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

    const state = new ContextState(context, testModel);
    const transition = state.analyzeTransition(context, [trueInput]);

    const results = determineTokenizedCorrectionSequence(
      transition,
      transition.final.displayTokenization,
      new TokenizationResultMapping([{
        matchString: 'd',
        inputSamplingCost: -Math.log(trueInput.p),
        inputCount: 1,
        knownCost: 0,
        totalCost: -Math.log(trueInput.p)
      }], null)
    );

    assert.deepEqual({...results.rootContext, casingForm: results.rootContext.casingForm}, {
      casingForm: undefined,
      left: 'the ',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    });

    // Coming up next - actually providing ALL correction elements, not just the final one.
    // We're not _quite_ ready for that yet, though.
    assert.equal(results.tokenizedCorrection.length, 1);
    assert.deepEqual(results.tokenizedCorrection[0].sample, {
      insert: 'd',
      deleteLeft: 0
    });
    assert.approximately(results.tokenizedCorrection[0].p, Math.pow(trueInput.p, ModelCompositor.SINGLE_CHAR_KEY_PROB_EXPONENT), Number.EPSILON*1000);

    const dummiedTuple: CompositedIntermediatePrediction = {
      components: {
        prediction: {
          transform: { insert: 'dog', deleteLeft: 0 },
          displayAs: 'dog'
        },
        correction: 'd'
      },
      metadata: {
        probabilities: {
          prediction: .25,
          correction: trueInput.p,
          total: .25 * trueInput.p
        },
        autoSelectable: true
      }
    };

    results.applyInPost(dummiedTuple);

    assert.deepEqual(dummiedTuple.metadata.preservationTransform, {
      insert: trueInput.sample.insert.substring(0, KMWString.length(trueInput.sample.insert) - 1), // remove the 'd'.
      deleteLeft: trueInput.sample.deleteLeft - 1
    });
  });
});