/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-07-21
 *
 * This file unit tests against the `prepareTokenizationSearch`
 * prediction-helper function, which uses results from
 * `determineSuggestionRange` to build phrase-level correctors for multi-token
 * correction.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import { ContextState, ContextTokenization, ContextTransition, models, prepareTokenizationSearch, ContextToken, LegacyQuotientRoot, LegacyQuotientSpur } from "@keymanapp/lm-worker/test-index";

import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

describe.only('prepareTokenizationSearch', () => {
  it('handles simple-case, single tokenization transitions well', () => {
    const baseContext: Context = {
      left: '',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const transition = new ContextTransition(new ContextState(baseContext, testModel), 0);

    const targetContext: Context = {
      left: 'a',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const targetTokenization = new ContextTokenization([ContextToken.fromRawText(testModel, 'a')]);

    const nextState = new ContextState(targetContext, testModel, targetTokenization, [targetTokenization]);

    transition.finalize(nextState, [
      { sample: { insert: 'a', deleteLeft: 0, id: 1}, p: 1 }
    ]);

    const correctorsForSearch = prepareTokenizationSearch(transition, [targetTokenization]);
    assert.equal(correctorsForSearch.length, 1);

    const corrector = correctorsForSearch[0];
    assert.deepEqual(corrector.orderedTokens, targetTokenization.tokens);
    assert.deepEqual(corrector.correctableTokens, []);
    assert.deepEqual(corrector.uncorrectableTokens, []);
    assert.deepEqual(corrector.predictableToken, targetTokenization.tail);

    assert.equal(corrector.correctableCodepoints, 1);
    assert.isTrue(corrector.modelsCorrectables)
    assert.equal(corrector.tokenization, targetTokenization);
  });

  it('handles single-tokenization transitions from whitespace well', () => {
    const baseContext: Context = {
      left: 'space',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const transition = new ContextTransition(new ContextState(baseContext, testModel), 0);

    const targetContext: Context = {
      left: 'space ',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const targetTokenization = new ContextTokenization([
      transition.base.displayTokenization.tail,
      ContextToken.fromRawText(testModel, ' '),
      ContextToken.fromRawText(testModel, '', true)
    ]);

    const nextState = new ContextState(targetContext, testModel, targetTokenization, [targetTokenization]);

    transition.finalize(nextState, [
      { sample: { insert: ' ', deleteLeft: 0, id: 1}, p: 1 }
    ]);

    const correctorsForSearch = prepareTokenizationSearch(transition, [targetTokenization]);
    assert.equal(correctorsForSearch.length, 1);

    const corrector = correctorsForSearch[0];
    assert.deepEqual(corrector.orderedTokens, targetTokenization.tokens.slice(1));
    assert.deepEqual(corrector.correctableTokens, []);
    assert.deepEqual(corrector.uncorrectableTokens, [targetTokenization.tokens[1]]);
    assert.deepEqual(corrector.predictableToken, targetTokenization.tail);

    assert.equal(corrector.correctableCodepoints, 0);
    assert.isTrue(corrector.modelsCorrectables)
    assert.equal(corrector.tokenization, targetTokenization);
  });

  it('handles divergent transitions with possible wordbreaks', () => {
    const baseContext: Context = {
      left: 'space',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const transition = new ContextTransition(new ContextState(baseContext, testModel), 0);

    const targetContext: Context = {
      left: 'spaced',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const distribution: Distribution<Transform> = [
      { sample: { insert: 'd', deleteLeft: 0, id: 1}, p: .8 },
      { sample: { insert: ' ', deleteLeft: 0, id: 1}, p: .2 }
    ];


    const targetTokenization1 = new ContextTokenization([
      new ContextToken(new LegacyQuotientSpur(transition.base.displayTokenization.tail.searchModule, [distribution[0]], distribution[0]))
    ]);

    const targetTokenization2 = new ContextTokenization([
      transition.base.displayTokenization.tail,
      new ContextToken(new LegacyQuotientSpur(new LegacyQuotientRoot(testModel), [distribution[1]], distribution[0])),
      ContextToken.fromRawText(testModel, '', true)
    ]);

    const tokenizations = [targetTokenization1, targetTokenization2];

    const nextState = new ContextState(targetContext, testModel, targetTokenization1, tokenizations);

    transition.finalize(nextState, distribution);

    const correctorsForSearch = prepareTokenizationSearch(transition, tokenizations);
    assert.equal(correctorsForSearch.length, 2);

    const corrector1 = correctorsForSearch[0];
    assert.deepEqual(corrector1.orderedTokens, targetTokenization1.tokens);
    assert.deepEqual(corrector1.correctableTokens, []);
    assert.deepEqual(corrector1.uncorrectableTokens, []);
    assert.deepEqual(corrector1.predictableToken, targetTokenization1.tail);

    assert.equal(corrector1.correctableCodepoints, 6);
    assert.isTrue(corrector1.modelsCorrectables)
    assert.equal(corrector1.tokenization, targetTokenization1);

    const corrector2 = correctorsForSearch[1];
    assert.deepEqual(corrector2.orderedTokens, targetTokenization2.tokens);
    assert.deepEqual(corrector2.correctableTokens, []);
    assert.deepEqual(corrector2.uncorrectableTokens, targetTokenization2.tokens.slice(0, 2));
    assert.deepEqual(corrector2.predictableToken, targetTokenization2.tail);

    assert.equal(corrector2.correctableCodepoints, 0);
    assert.isTrue(corrector2.modelsCorrectables)
    assert.equal(corrector2.tokenization, targetTokenization2);
  });

  it('handles simple-case, backspace tokenization transitions well', () => {
    const baseContext: Context = {
      left: 'apples',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const transition = new ContextTransition(new ContextState(baseContext, testModel), 0);

    const targetContext: Context = {
      left: 'apple',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const targetTokenization = new ContextTokenization([ContextToken.fromRawText(testModel, 'apple')]);

    const nextState = new ContextState(targetContext, testModel, targetTokenization, [targetTokenization]);

    transition.finalize(nextState, [
      { sample: { insert: '', deleteLeft: 1, id: 1}, p: 1 }
    ]);

    const correctorsForSearch = prepareTokenizationSearch(transition, [targetTokenization]);
    assert.equal(correctorsForSearch.length, 1);

    const corrector = correctorsForSearch[0];
    assert.deepEqual(corrector.orderedTokens, targetTokenization.tokens);
    assert.deepEqual(corrector.correctableTokens, []);
    assert.deepEqual(corrector.uncorrectableTokens, []);
    assert.deepEqual(corrector.predictableToken, targetTokenization.tail);

    assert.equal(corrector.correctableCodepoints, 5);
    assert.isTrue(corrector.modelsCorrectables)
    assert.equal(corrector.tokenization, targetTokenization);
  });

  it('handles whitespace-token deletion transitions well', () => {
    const baseContext: Context = {
      left: 'apples ',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const transition = new ContextTransition(new ContextState(baseContext, testModel), 0);

    const targetContext: Context = {
      left: 'apples',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const targetTokenization = new ContextTokenization([transition.base.displayTokenization.tokens[0]]);

    const nextState = new ContextState(targetContext, testModel, targetTokenization, [targetTokenization]);

    transition.finalize(nextState, [
      { sample: { insert: '', deleteLeft: 1, id: 1}, p: 1 }
    ]);

    const correctorsForSearch = prepareTokenizationSearch(transition, [targetTokenization]);
    assert.equal(correctorsForSearch.length, 1);

    const corrector = correctorsForSearch[0];
    assert.deepEqual(corrector.orderedTokens, targetTokenization.tokens);
    assert.deepEqual(corrector.correctableTokens, []);
    assert.deepEqual(corrector.uncorrectableTokens, []);
    assert.deepEqual(corrector.predictableToken, targetTokenization.tail);

    assert.equal(corrector.correctableCodepoints, 6);
    assert.isTrue(corrector.modelsCorrectables)
    assert.equal(corrector.tokenization, targetTokenization);
  });

  // TODO:  a more complex transition set.
  it('handles complicated dictionary-style wordbreaking transitions', () => {
    const baseContext: Context = {
      left: 'myapplesandsour',
      startOfBuffer: true,
      endOfBuffer: true
    };

    // To be safe, make sure the spaces are constructed in proper left-to-right order,
    // no matter the variation.
    const my     = ContextToken.fromRawText(testModel, 'my')
    const apples = ContextToken.fromRawText(testModel, 'apples');
    const apple  = ContextToken.fromRawText(testModel, 'apple');

    const and    = ContextToken.fromRawText(testModel, 'and');
    const sand   = ContextToken.fromRawText(testModel, 'sand');
    const sands  = ContextToken.fromRawText(testModel, 'sands');

    const sour   = ContextToken.fromRawText(testModel, 'sour');
    const our    = ContextToken.fromRawText(testModel, 'our');

    const baseTokenization = new ContextTokenization(([my, apples, and, sour]));

    const baseTokenizationVariants: ContextTokenization[] = [
      new ContextTokenization(([my, apple, sand, sour])),
      new ContextTokenization(([my, apple, sands, our]))
    ]
    const transition = new ContextTransition(new ContextState(baseContext, testModel, baseTokenization, baseTokenizationVariants), 0);

    // ----
    const targetContext: Context = {
      left: 'myapplesandsourg',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const distribution: Distribution<Transform> = [
      { sample: { insert: 'g', deleteLeft: 0, id: 1}, p: 1 }
    ];

    const g      = () => new ContextToken(new LegacyQuotientSpur(new LegacyQuotientRoot(testModel), distribution, distribution[0]));
    const ourg   = () => new ContextToken(new LegacyQuotientSpur(our.searchModule, distribution, distribution[0]));
    const sourg  = () => new ContextToken(new LegacyQuotientSpur(sour.searchModule, distribution, distribution[0]));

    const targetTokenization = new ContextTokenization([my, apples, and, sour, g()]);

    const targetTokenizationVariants: ContextTokenization[] = [
      new ContextTokenization([my, apples, and, sourg()]),
      new ContextTokenization([my, apple, sand, sour, g()]),
      new ContextTokenization([my, apple, sand, sourg()]),
      new ContextTokenization([my, apple, sands, our, g()]),
      new ContextTokenization([my, apple, sands, ourg()])
    ];

    const nextState = new ContextState(targetContext, testModel, targetTokenization, [targetTokenization, ...targetTokenizationVariants]);
    transition.finalize(nextState, distribution);

    const variationStartIndex: Map<ContextTokenization, number> = new Map();
    for(let variant of nextState.tokenizations) {
      let i = 0;
      while(variant.tokens[i]?.exampleInput == baseTokenization.tokens[i]?.exampleInput) {
        i++;
      }
      variationStartIndex.set(variant, i);
    }

    const correctorsForSearch = prepareTokenizationSearch(transition, nextState.tokenizations, {
      rangeValidator: (index, rs) => index >= rs
    });
    assert.equal(correctorsForSearch.length, nextState.tokenizations.length);

    correctorsForSearch.forEach((corrector, index) => {
      const tokenization = nextState.tokenizations.find((t) => corrector.tokenization == t);
      assert.isOk(tokenization);

      assert.deepEqual(corrector.orderedTokens, tokenization.tokens.slice(1), `Error for variant at index ${index}`);
      const correctables = tokenization.tokens.slice(variationStartIndex.get(tokenization), -1)
      assert.deepEqual(corrector.correctableTokens, correctables, `Error for variant at index ${index}`);
      assert.deepEqual(corrector.uncorrectableTokens, tokenization.tokens.slice(1, variationStartIndex.get(tokenization)), `Error for variant at index ${index}`);
      assert.deepEqual(corrector.predictableToken, tokenization.tail, `Error for variant at index ${index}`);

      assert.equal(
        corrector.correctableCodepoints,
        correctables.reduce((accum, curr) => accum + curr.codepointLength, 0) + tokenization.tail.codepointLength,
        `Error for variant at index ${index}`
      );
      assert.isTrue(corrector.modelsCorrectables, `Error for variant at index ${index}`);
    });
  });
});