
/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-07-23
 *
 * This file contains tests designed to validate the behavior of the
 * buildAndMapPredictions helper function class and its integration with the
 * lower-level predictive-text helpers.
 */


import { assert } from 'chai';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { LexicalModelTypes } from '@keymanapp/common-types';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { TrieModel } from '@keymanapp/models-templates';

import { buildAndMapPredictions, buildEdgeWindow, ContextState, ContextToken, ContextTokenization, ContextTransition, generateSubsetId, LegacyQuotientRoot, LegacyQuotientSpur, models, predictFromCorrections } from "@keymanapp/lm-worker/test-index";

import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

const plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

describe('buildAndMapPredictions', () => {
  it('adds the preservation transform to all generated predictions', () => {
    const context: Context = {
      left: 'th',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const correctionDistribution: Distribution<Transform> = [{
        sample: {
          insert: 'e',
          deleteLeft: 0
        },
        p: 0.6
      }
    ];

    const basePredictions = predictFromCorrections(plainModel, correctionDistribution, context);
    basePredictions.forEach((entry) => assert.isNotOk(entry.preservationTransform));

    // must construct the taillessTrueKeystroke appropriately.
    const tailless = { insert: 'TEST', deleteLeft: 0 };
    const tokenization = new ContextTokenization([ContextToken.fromRawText(plainModel, 'th', true)], null, tailless);
    const transition = new ContextTransition(new ContextState(context, plainModel, tokenization), 0);

    const targetTokenization = new ContextTokenization([new ContextToken(new LegacyQuotientSpur(tokenization.tail.searchModule, correctionDistribution, correctionDistribution[0]))]);
    transition.finalize(new ContextState(models.applyTransform(correctionDistribution[0].sample, context), plainModel, targetTokenization), correctionDistribution);

    const mappedPredictions = buildAndMapPredictions(
      transition,
      transition.base.displayTokenization,
      {matchString: 'the', totalCost: 0},
      1
    );

    assert.deepEqual(mappedPredictions.map((tuple) => tuple.prediction), basePredictions.map((tuple) => tuple.prediction));
    mappedPredictions.forEach((tuple) => assert.isOk(tuple.preservationTransform));
    mappedPredictions.forEach((tuple) => tuple.preservationTransform == tailless);
  });

  it('properly handles empty prediction roots from deleted same-token codepoints', () => {
    const context: Context = {
      left: 'the a',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const correctionDistribution: Distribution<Transform> = [{
        sample: {
          insert: '',
          deleteLeft: 1
        },
        p: 1
      }
    ];

    const basePredictions = predictFromCorrections(plainModel, correctionDistribution, context);

    // must construct the taillessTrueKeystroke appropriately.
    const tokenization = new ContextTokenization([
      ContextToken.fromRawText(plainModel, 'the', false),
      ContextToken.fromRawText(plainModel, ' ', false),
      ContextToken.fromRawText(plainModel, 'a', true)
    ]);
    const transition = new ContextTransition(new ContextState(context, plainModel, tokenization), 0);

    const targetTokenization = new ContextTokenization([
      tokenization.tokens[0],
      tokenization.tokens[1],
      new ContextToken(new LegacyQuotientRoot(plainModel))
    ]);
    transition.finalize(new ContextState(models.applyTransform(correctionDistribution[0].sample, context), plainModel, targetTokenization), correctionDistribution);

    const mappedPredictions = buildAndMapPredictions(
      transition,
      transition.base.displayTokenization,
      {matchString: '', totalCost: 0},
      1
    );

    assert.deepEqual(mappedPredictions.map((tuple) => tuple.prediction), basePredictions.map((tuple) => tuple.prediction));
  });

  it('properly handles empty prediction roots caused by backspacing one of multiple spaces', () => {
    const context: Context = {
      left: 'the  ',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const correctionDistribution: Distribution<Transform> = [{
        sample: {
          insert: '',
          deleteLeft: 1
        },
        p: 1
      }
    ];

    const basePredictions = predictFromCorrections(plainModel, correctionDistribution, context);

    // must construct the taillessTrueKeystroke appropriately.
    const tokenization = new ContextTokenization([
      ContextToken.fromRawText(plainModel, 'the', false),
      ContextToken.fromRawText(plainModel, '  ', false),
      ContextToken.fromRawText(plainModel, '', true)
    ]);
    const transition = new ContextTransition(new ContextState(context, plainModel, tokenization), 0);

    const targetTokenization = new ContextTokenization([
      tokenization.tokens[0],
      new ContextToken(new LegacyQuotientSpur(tokenization.tokens[1].searchModule, correctionDistribution, correctionDistribution[0])),
      new ContextToken(new LegacyQuotientRoot(plainModel))
    ]);
    transition.finalize(new ContextState(models.applyTransform(correctionDistribution[0].sample, context), plainModel, targetTokenization), correctionDistribution);

    const mappedPredictions = buildAndMapPredictions(
      transition,
      transition.base.displayTokenization,
      {matchString: '', totalCost: 0},
      1
    );

    assert.deepEqual(mappedPredictions.map((tuple) => tuple.prediction), basePredictions.map((tuple) => tuple.prediction));
  });

  it('properly handles contexts made empty by input backspace', () => {
    const context: Context = {
      left: 't',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const correctionDistribution = [{
        sample: {
          insert: '',
          deleteLeft: 1,
          deleteRight: 0
        },
        p: 1
      }
    ];

    // must construct the taillessTrueKeystroke appropriately.
    const tokenization = new ContextTokenization([
      ContextToken.fromRawText(plainModel, 't', true)
    ]);
    const transition = new ContextTransition(new ContextState(context, plainModel, tokenization), 0);

    const targetTokenization = new ContextTokenization([
      new ContextToken(new LegacyQuotientRoot(plainModel))
    ], {
      alignment: {
        merges: [],
        splits: [],
        unmappedEdits: [],
        edgeWindow: {
          ...buildEdgeWindow(tokenization.tokens, correctionDistribution[0].sample, false),
          retokenization: [''],
          retokenizationText: ''
        },
        removedTokenCount: 0
      },
      inputs: (() => {
        const val: ProbabilityMass<Map<number, Transform>>[] = [{
          sample: new Map(),
          p: correctionDistribution[0].p
        }];

        val[0].sample.set(0, correctionDistribution[0].sample);

        return val;
      })(),
      inputSubsetId: generateSubsetId()
    }, null);
    transition.finalize(new ContextState(models.applyTransform(correctionDistribution[0].sample, context), plainModel, targetTokenization), correctionDistribution);

    const mappedPredictions = buildAndMapPredictions(
      transition,
      transition.final.displayTokenization,
      {matchString: '', totalCost: 0},
      1
    );

    mappedPredictions.forEach((tuple) => assert.equal(tuple.prediction.sample.transform.deleteLeft, 1));
  });
});