/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-04-13
 *
 * This file unit tests against the `determineSuggestionRange` prediction-helper function,
 * ensuring that it correctly determines its values for differerent tokenization-pattern
 * contrast cases.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import {
  ContextToken,
  ContextTokenization,
  determineSuggestionRange,
  LegacyQuotientRoot,
  LegacyQuotientSpur,
  models
} from "@keymanapp/lm-worker/test-index";

import Distribution = LexicalModelTypes.Distribution;
import TrieModel = models.TrieModel;
import Transform = LexicalModelTypes.Transform;

const plainModel = new TrieModel(
  jsonFixture('models/tries/english-1000'), {
    languageUsesCasing: true,
    wordBreaker: defaultBreaker
  }
);

function buildQuickBrownFixture() {
  const qbfText = ['the', ' ', 'quick', ' ', 'brown', ' ', 'fox', ' ', 'jumped', ' ', 'over', ' ', 'the', ' ', 'lazy', ' ', 'dog'];
  const baseTokenization = new ContextTokenization(qbfText.map((t) => ContextToken.fromRawText(plainModel, t, false)));
  const baseTokenCount = baseTokenization.tokens.length;

  const plainInsertDistrib: Distribution<Transform> = [
    { sample: { insert: 's', deleteLeft: 0, deleteRight: 0, id: 11 }, p: .45 },
    { sample: { insert: 'g', deleteLeft: 0, deleteRight: 0, id: 11 }, p: .2 }
  ];
  const plainInsertTokenization = new ContextTokenization(
    baseTokenization.tokens.slice(0, baseTokenCount-1).concat(
      new ContextToken(new LegacyQuotientSpur(baseTokenization.tail.searchModule, plainInsertDistrib, plainInsertDistrib[0]))
    ),
    null,
    null
  );

  const newTokenInsertDistrib: Distribution<Transform> = [
    { sample: { insert: '.', deleteLeft: 0, deleteRight: 0, id: 11 }, p: .1 }
  ];
  const newTokenInsertTokenization = new ContextTokenization(
    baseTokenization.tokens.slice(0, baseTokenCount).concat(
      new ContextToken(new LegacyQuotientSpur(new LegacyQuotientRoot(plainModel), newTokenInsertDistrib, newTokenInsertDistrib[0]))
    ),
    null,
    null
  );

  const charReplaceDistrib: Distribution<Transform> = [
    { sample: { insert: 't', deleteLeft: 1, deleteRight: 0, id: 11 }, p: .5 },
    { sample: { insert: 'c', deleteLeft: 1, deleteRight: 0, id: 11 }, p: .05 }
  ];
  const charReplaceTokenization = new ContextTokenization(
    baseTokenization.tokens.slice(0, baseTokenCount - 1).concat(
      new ContextToken(new LegacyQuotientSpur(baseTokenization.tail.searchModule, charReplaceDistrib, charReplaceDistrib[0]))
    ),
    null,
    null
  );

  const eraseTokenDistrib: Distribution<Transform> = [
    { sample: { insert: '', deleteLeft: baseTokenization.tail.searchModule.codepointLength, deleteRight: 0, id: 11 }, p: .05 },
  ];
  const eraseTokenTokenization = new ContextTokenization(
    baseTokenization.tokens.slice(0, baseTokenCount - 1).concat(
      new ContextToken(new LegacyQuotientRoot(plainModel))
    ),
    null,
    null
  );

  const del5Insert5Distrib: Distribution<Transform> = [
    { sample: { insert: 'iness', deleteLeft: 5, deleteRight: 0, id: 11 }, p: .05 }
  ];
  const del5Insert5Tokenization = new ContextTokenization(
    baseTokenization.tokens.slice(0, baseTokenCount - 3).concat(
      new ContextToken(new LegacyQuotientSpur(baseTokenization.tokens[baseTokenCount-2].searchModule, del5Insert5Distrib, del5Insert5Distrib[0]))
    ),
    null,
    null
  );

  const deleteToBoundDistrib: Distribution<Transform> = [
    { sample: { insert: '', deleteLeft: 4, deleteRight: 0, id: 11 }, p: .05 }
  ];
  const deleteToBoundTokenization = new ContextTokenization(
    baseTokenization.tokens.slice(0, baseTokenCount - 3).concat(
      new ContextToken(new LegacyQuotientSpur(baseTokenization.tokens[baseTokenCount-2].searchModule, deleteToBoundDistrib, deleteToBoundDistrib[0]))
    ),
    null,
    null
  );

  return {
    baseTokenization,
    variations: {
      noChange: {
        dist: [{sample: { insert: '', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 1}],
        tokenization: baseTokenization,
        range: {
          tokensToRemove: [baseTokenization.tail],
          tokensToPredict: [baseTokenization.tail]
        }
      },
      plainInsert: {
        dist: plainInsertDistrib,
        tokenization: plainInsertTokenization,
        range: {
          tokensToRemove: [baseTokenization.tail],
          tokensToPredict: [plainInsertTokenization.tail]
        }
      },
      newTokenInsert: {
        dist: newTokenInsertDistrib,
        tokenization: newTokenInsertTokenization,
        range: {
          tokensToRemove: [] as ContextToken[],
          tokensToPredict: [newTokenInsertTokenization.tail]
        }
      },
      charReplace: {
        dist: charReplaceDistrib,
        tokenization: charReplaceTokenization,
        range: {
          tokensToRemove: [baseTokenization.tail],
          tokensToPredict: [charReplaceTokenization.tail]
        }
      },
      eraseToken: {
        dist: eraseTokenDistrib,
        tokenization: eraseTokenTokenization,
        range: {
          tokensToRemove: [baseTokenization.tail],
          tokensToPredict: [eraseTokenTokenization.tail]
        }
      },
      del5Insert5: {
        dist: del5Insert5Distrib,
        tokenization: del5Insert5Tokenization,
        range: {
          tokensToRemove: baseTokenization.tokens.slice(baseTokenCount-3),
          tokensToPredict: [del5Insert5Tokenization.tail]
        }
      },
      deleteToBound: {
        dist: deleteToBoundDistrib,
        tokenization: deleteToBoundTokenization,
        range: {
          tokensToRemove: baseTokenization.tokens.slice(baseTokenCount-3),
          tokensToPredict: [deleteToBoundTokenization.tail]
        }
      }
    }
  };
}

describe('determineSuggestionRange', () => {
  it('adjusts the final token if no tokenization changes occur', () => {
    const fixture = buildQuickBrownFixture();
    const noChange = fixture.variations.noChange;

    const analysis = determineSuggestionRange(fixture.baseTokenization, noChange.tokenization);

    assert.sameOrderedMembers(analysis.tokensToRemove, noChange.range.tokensToRemove);
    assert.sameOrderedMembers(analysis.tokensToPredict, noChange.range.tokensToPredict);
  });

  it('adjusts the final token after a simple same-token insert', () => {
    const fixture = buildQuickBrownFixture();
    const plainInsert = fixture.variations.plainInsert;

    const analysis = determineSuggestionRange(fixture.baseTokenization, plainInsert.tokenization);

    assert.sameOrderedMembers(analysis.tokensToRemove, plainInsert.range.tokensToRemove);
    assert.sameOrderedMembers(analysis.tokensToPredict, plainInsert.range.tokensToPredict);
  });

  it('adjusts the final token after a simple word-breaking insert', () => {
    const fixture = buildQuickBrownFixture();
    const newTokenInsert = fixture.variations.newTokenInsert;

    const analysis = determineSuggestionRange(fixture.baseTokenization, newTokenInsert.tokenization);

    assert.sameOrderedMembers(analysis.tokensToRemove, newTokenInsert.range.tokensToRemove);
    assert.sameOrderedMembers(analysis.tokensToPredict, newTokenInsert.range.tokensToPredict);
  });

  it('adjusts the final token after a simple same-token character replacement', () => {
    const fixture = buildQuickBrownFixture();
    const charReplace = fixture.variations.charReplace;

    const analysis = determineSuggestionRange(fixture.baseTokenization, charReplace.tokenization);

    assert.sameOrderedMembers(analysis.tokensToRemove, charReplace.range.tokensToRemove);
    assert.sameOrderedMembers(analysis.tokensToPredict, charReplace.range.tokensToPredict);
  });

  it('handles deletion of two tokens + alteration of the token before', () => {
    const fixture = buildQuickBrownFixture();
    const del5Insert5 = fixture.variations.del5Insert5;

    const analysis = determineSuggestionRange(fixture.baseTokenization, del5Insert5.tokenization);

    assert.sameOrderedMembers(analysis.tokensToRemove, del5Insert5.range.tokensToRemove);
    assert.sameOrderedMembers(analysis.tokensToPredict, del5Insert5.range.tokensToPredict);
  });

  it('handles deletion of chars up to closest whitespace', () => {
    const fixture = buildQuickBrownFixture();
    const eraseToken = fixture.variations.eraseToken;

    const analysis = determineSuggestionRange(fixture.baseTokenization, eraseToken.tokenization);

    assert.sameOrderedMembers(analysis.tokensToRemove, eraseToken.range.tokensToRemove);
    assert.sameOrderedMembers(analysis.tokensToPredict, eraseToken.range.tokensToPredict);
  });

  it('handles deletion up to boundary of ancestor non-whitespace token', () => {
    const fixture = buildQuickBrownFixture();
    const deleteToBound = fixture.variations.deleteToBound;

    const analysis = determineSuggestionRange(fixture.baseTokenization, deleteToBound.tokenization);

    assert.sameOrderedMembers(analysis.tokensToRemove, deleteToBound.range.tokensToRemove);
    assert.sameOrderedMembers(analysis.tokensToPredict, deleteToBound.range.tokensToPredict);
  });

  it('handles large variation in intermediate tokens', () => {
    const originalQuickBrownTokenization = buildQuickBrownFixture().baseTokenization;
    const rawText = ['beyond', ' ', 'the', ' ', 'hungry', ' ', 'green', ' ', 'alligator'];
    // the quick brown fox jumped |
    // Final whitespace is immediately before index 10.
    const transitionSliceIndex = 10;
    const tokensToAppend = rawText.map((t) => ContextToken.fromRawText(plainModel, t, false));

    const foxVsAlligatorTokenization = new ContextTokenization(
      originalQuickBrownTokenization.tokens.slice(0, transitionSliceIndex).concat(tokensToAppend),
      null,
      null
    )

    const analysis = determineSuggestionRange(originalQuickBrownTokenization, foxVsAlligatorTokenization);

    assert.sameOrderedMembers(
      analysis.tokensToRemove,
      originalQuickBrownTokenization.tokens.slice(transitionSliceIndex)
    );
    assert.sameOrderedMembers(
      analysis.tokensToPredict,
      tokensToAppend
    );
  });

  it('handles insertion of many extra new tokens at once', () => {
    const originalQuickBrownTokenization = buildQuickBrownFixture().baseTokenization;
    const originalTokenCount = originalQuickBrownTokenization.tokens.length;
    const rawText = ['dogs', ' ', 'and', ' ', 'the', ' ', 'sleeping', ' ', 'cat'];
    const tokensToAppend = rawText.map((t) => ContextToken.fromRawText(plainModel, t, false));

    const dogsAndCatTokenization = new ContextTokenization(
      originalQuickBrownTokenization.tokens.slice(0, originalTokenCount - 1).concat(tokensToAppend),
      null,
      null
    )

    const analysis = determineSuggestionRange(originalQuickBrownTokenization, dogsAndCatTokenization);

    assert.sameOrderedMembers(
      analysis.tokensToRemove,
      [originalQuickBrownTokenization.tail]
    );
    assert.sameOrderedMembers(
      analysis.tokensToPredict,
      tokensToAppend
    );
  });
});