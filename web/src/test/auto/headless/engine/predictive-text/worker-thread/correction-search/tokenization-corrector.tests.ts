/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-04-14
 *
 * This file defines tests for the `TokenizationCorrector` class, which is used
 * to prioritize optimal multi-token corrections (and predictions) within the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import {
  ContextToken,
  ContextTokenization,
  correctionValidForAutoSelect,
  generateSubsetId,
  LegacyQuotientSpur,
  models,
  PathInputProperties,
  PathResult,
  SearchQuotientNode,
  SearchQuotientRoot,
  TokenizationCorrector,
  TokenResult
} from '@keymanapp/lm-worker/test-index';

import Distribution = LexicalModelTypes.Distribution;
import TrieModel = models.TrieModel;
import Transform = LexicalModelTypes.Transform;
import { TokenizationResultMapping } from '../../../../../../../engine/predictive-text/worker-thread/build/obj/correction/tokenization-result-mapping.js';

const plainModel = new TrieModel(
  jsonFixture('models/tries/english-1000'), {
    languageUsesCasing: true,
    wordBreaker: defaultBreaker
  }
);

function buildFixture_therefore() {
  let ID_SEED = 11;

  const distributionSrc: [string, number][][] = [
    [ ['t', 1] ],
    [ ['h', 1] ],
    [ ['e', 0.6] ],
    [ [' ', 0.8], ['r', 0.2] ],
    [ ['e', 1] ],
    [ ['f', 1] ]
  ];

  const distributions: Distribution<Required<Transform>>[] = distributionSrc.map((tupleArray) => {
    const transitionId = ID_SEED++;
    return tupleArray.map((tuple) => {
      return {
        p: tuple[1],
        sample: {
          insert: tuple[0],
          deleteLeft: 0,
          deleteRight: 0,
          id: transitionId
        }
      }
    });
  });

  // Assumes that the first entry in each distribution is the most likely.
  const inputSources: PathInputProperties[] = distributions.map((dist) => {
    return {
      subsetId: generateSubsetId(),
      segment: {start: 0, transitionId: dist[0].sample.id},
      bestProbFromSet: dist[0].p
    };
  })

  const therefTokens: ContextToken[] = []; // as in "therefore"
  const the_efTokens: ContextToken[] = []; // as in "the effect"

  // TODO:  Use SubstitutionQuotientSpur instead!
  let firstTokenNode: SearchQuotientNode = new SearchQuotientRoot(plainModel);
  for(let i=0; i < 3; i++) {
    firstTokenNode = new LegacyQuotientSpur(firstTokenNode, distributions[i], inputSources[i]);
  }

  the_efTokens.push(new ContextToken(firstTokenNode, false));

  firstTokenNode = new LegacyQuotientSpur(firstTokenNode, [distributions[3][1]], {
    ...inputSources[3],
    subsetId: generateSubsetId()
  });

  // whitespace token alternate - using the ' ' input instead.
  const whitespaceToken = new ContextToken(
    new LegacyQuotientSpur(
      new SearchQuotientRoot(plainModel),
      [distributions[3][0]],
      { ...inputSources[3], subsetId: generateSubsetId() }
    ), false
  );
  whitespaceToken.isWhitespace = true;
  the_efTokens.push(whitespaceToken);

  let secondTokenNode: SearchQuotientNode = new SearchQuotientRoot(plainModel);
  for(let i=4; i < distributions.length; i++) {
    firstTokenNode = new LegacyQuotientSpur(firstTokenNode, distributions[i], {
      ...inputSources[i],
      subsetId: generateSubsetId()
    });
    secondTokenNode = new LegacyQuotientSpur(secondTokenNode, distributions[i], {
      ...inputSources[i],
      subsetId: generateSubsetId()
    })
  }

  therefTokens.push(new ContextToken(firstTokenNode));
  the_efTokens.push(new ContextToken(secondTokenNode));

  return {
    filter: (token: ContextToken) => correctionValidForAutoSelect(token.exampleInput),
    theref: new ContextTokenization(therefTokens),
    the_ef: new ContextTokenization(the_efTokens)
  }
}

function buildFixture_terminalWhitespace() {
  let ID_SEED = 11;

  const distributionSrc: [string, number][][] = [
    [ ['s', 1] ],
    [ ['p', 1] ],
    [ ['a', 1] ],
    [ ['c', 1] ],
    [ ['e', 1] ],
    [ [' ', 1] ],
  ];

  const distributions: Distribution<Required<Transform>>[] = distributionSrc.map((tupleArray) => {
    const transitionId = ID_SEED++;
    return tupleArray.map((tuple) => {
      return {
        p: tuple[1],
        sample: {
          insert: tuple[0],
          deleteLeft: 0,
          deleteRight: 0,
          id: transitionId
        }
      }
    });
  });

  // Assumes that the first entry in each distribution is the most likely.
  const inputSources: PathInputProperties[] = distributions.map((dist) => {
    return {
      subsetId: generateSubsetId(),
      segment: {start: 0, transitionId: dist[0].sample.id},
      bestProbFromSet: dist[0].p
    };
  })

  const fullTokens: ContextToken[] = [];
  const lastToken: ContextToken[] = [];

  // TODO:  Use SubstitutionQuotientSpur instead!
  let firstTokenNode: SearchQuotientNode = new SearchQuotientRoot(plainModel);
  for(let i=0; i < 5; i++) {
    firstTokenNode = new LegacyQuotientSpur(firstTokenNode, distributions[i], inputSources[i]);
  }

  fullTokens.push(new ContextToken(firstTokenNode, false));

  // whitespace token alternate - using the ' ' input instead.
  const whitespaceToken = new ContextToken(
    new LegacyQuotientSpur(
      new SearchQuotientRoot(plainModel),
      distributions[5],
      inputSources[5],
    ), false
  );
  whitespaceToken.isWhitespace = true;
  fullTokens.push(whitespaceToken);
  lastToken.push(whitespaceToken);

  return {
    filter: (token: ContextToken) => correctionValidForAutoSelect(token.exampleInput),
    wordThenSpace: new ContextTokenization(fullTokens),
    spaceOnly: new ContextTokenization(lastToken)
  }
}

describe('TokenizationCorrector', () => {
  describe('constructor', () => {
    it('constructs correctly from a single correctable token', () => {
      const fixture = buildFixture_therefore();

      const tokenization = fixture.theref;

      const instance = new TokenizationCorrector(
        tokenization,
        1,
        fixture.filter
      );

      assert.sameOrderedMembers(instance.uncorrectableTokens.slice(), []);
      assert.sameOrderedMembers(instance.correctableTokens.slice(), []);
      assert.equal(instance.predictableToken, tokenization.tail);
    });

    it('constructs correctly from a single uncorrectable token', () => {
      const fixture = buildFixture_terminalWhitespace();

      const tokenization = fixture.spaceOnly;

      const instance = new TokenizationCorrector(
        tokenization,
        tokenization.tokens.length,
        fixture.filter
      );

      assert.sameOrderedMembers(instance.uncorrectableTokens.slice(), [tokenization.tail]);
      assert.sameOrderedMembers(instance.correctableTokens.slice(), []);
      assert.equal(instance.predictableToken, undefined);
    });

    it('constructs from multiple tokens, with the middle one uncorrectable', () => {
      const fixture = buildFixture_therefore();

      const tokenization = fixture.the_ef;
      const tokenCount = tokenization.tokens.length;

      const instance = new TokenizationCorrector(
        tokenization,
        tokenCount,
        fixture.filter
      );

      assert.sameOrderedMembers(instance.uncorrectableTokens.slice(), [tokenization.tokens[tokenCount-2]]);
      assert.sameOrderedMembers(instance.correctableTokens.slice(), [tokenization.tokens[tokenCount-3]]);
      assert.equal(instance.predictableToken, tokenization.tail);
    });

    it('constructs from multiple tokens, ignoring the first due to bounds', () => {
      const fixture = buildFixture_therefore();

      const tokenization = fixture.the_ef;
      const tokenCount = tokenization.tokens.length;

      const instance = new TokenizationCorrector(
        tokenization,
        tokenCount-1,
        fixture.filter
      );

      assert.sameOrderedMembers(instance.uncorrectableTokens.slice(), [tokenization.tokens[tokenCount-2]]);
      assert.sameOrderedMembers(instance.correctableTokens.slice(), []);
      assert.equal(instance.predictableToken, tokenization.tail);
    });

    it('constructs correctly when the final token is uncorrectable', () => {
      const fixture = buildFixture_terminalWhitespace();

      const tokenization = fixture.wordThenSpace;

      const instance = new TokenizationCorrector(
        tokenization,
        tokenization.tokens.length,
        fixture.filter
      );

      assert.sameOrderedMembers(instance.uncorrectableTokens.slice(), [tokenization.tail]);
      assert.sameOrderedMembers(instance.correctableTokens.slice(), [tokenization.tokens[0]]);
      assert.equal(instance.predictableToken, undefined);
    });
  });

  describe('handleNextNode', () => {
    it('finds corrections for a single correctable token', () => {
      const fixture = buildFixture_therefore();

      const tokenization = fixture.theref;

      const instance = new TokenizationCorrector(
        tokenization,
        1,
        fixture.filter
      );

      let searchResult: PathResult<TokenizationResultMapping>;
      do {
        searchResult = instance.handleNextNode();
      } while(searchResult.type == 'intermediate');

      assert.equal(searchResult.type, 'complete');
      if(searchResult.type == 'complete') {
        const mapping = searchResult.mapping;
        const tokenResults = mapping.matchedResult;
        assert.isNotNaN(searchResult.cost);
        assert.equal(searchResult.cost, searchResult.mapping.totalCost);
        assert.equal(tokenResults.length, 1);
        assert.sameOrderedMembers(tokenResults.map((r) => r.matchString), ['theref']);

        // Now that an entry has been found, verify the corrector's state.
        assert.isOk(instance.predictableToken); // should not become bound or locked.
        assert.isTrue(instance.lockedTokenResults.has(instance.predictableToken));
        assert.equal(instance.lockedTokenResults.get(instance.predictableToken), tokenResults[0]);
      }

      searchResult = instance.handleNextNode();
      // There should be more results that may be found.
      assert.notEqual(searchResult.type, 'none');

      do {
        searchResult = instance.handleNextNode();
      } while(searchResult.type == 'intermediate');

      assert.notEqual(searchResult.type, 'none');
    });

    it('finds corrections for a group of tokens with two correctable', () => {
      const fixture = buildFixture_therefore();

      const tokenization = fixture.the_ef;

      const instance = new TokenizationCorrector(
        tokenization,
        3,
        fixture.filter
      );

      let searchResult: PathResult<TokenizationResultMapping>;
      do {
        searchResult = instance.handleNextNode();
      } while(searchResult.type == 'intermediate');

      assert.equal(searchResult.type, 'complete');
      let firstResults: ReadonlyArray<TokenResult>;
      if(searchResult.type == 'complete') {
        const mapping = searchResult.mapping;
        const tokenResults = mapping.matchedResult;
        firstResults = tokenResults;
        assert.isNotNaN(searchResult.cost);
        assert.equal(searchResult.cost, searchResult.mapping.totalCost);
        assert.equal(tokenResults.length, 3);
        assert.sameOrderedMembers(tokenResults.map((r) => r.matchString), ['the', ' ', 'ef']);
      }

      // Now that an entry has been found, verify the corrector's state.
      assert.isOk(instance.predictableToken); // should not become bound or locked.
      assert.isTrue(instance.lockedTokenResults.has(instance.predictableToken));
      for(let i=0; i < firstResults.length; i++) {
        assert.equal(instance.lockedTokenResults.get(instance.orderedTokens[i]), firstResults[i]);
      }

      searchResult = instance.handleNextNode();
      // There should be more results that may be found.
      assert.notEqual(searchResult.type, 'none');

      do {
        searchResult = instance.handleNextNode();
        if(searchResult.type == 'complete') {
          const mapping = searchResult.mapping;
          const tokenResults = mapping.matchedResult;

          // Verify that the first (bound) token is not altered further.
          // It should receive no further correction attempts.
          assert.equal(tokenResults[0], firstResults[0]);
          assert.equal(tokenResults[1], firstResults[1]);
          assert.notEqual(tokenResults[2], firstResults[2]);
        }
      } while(searchResult.type != 'none');
    });

    it('immediately returns a single result when the only represented token is uncorrectable', () => {
      const fixture = buildFixture_terminalWhitespace();

      const tokenization = fixture.spaceOnly;

      const instance = new TokenizationCorrector(
        tokenization,
        tokenization.tokens.length,
        fixture.filter
      );

      const searchResult = instance.handleNextNode();
      assert.equal(searchResult.type, 'complete');
      if(searchResult.type == 'complete') {
        assert.equal(searchResult.mapping.matchedResult[0].matchString, ' ');
      }

      const nilResult = instance.handleNextNode();
      assert.equal(nilResult.type, 'none');
    });

    it('returns a single result when the final token is uncorrectable', () => {
      const fixture = buildFixture_terminalWhitespace();

      const tokenization = fixture.wordThenSpace;

      const instance = new TokenizationCorrector(
        tokenization,
        tokenization.tokens.length,
        fixture.filter
      );

      let searchResult: PathResult<TokenizationResultMapping>;
      do {
        searchResult = instance.handleNextNode();
      } while(searchResult.type == 'intermediate');

      assert.equal(searchResult.type, 'complete');
      if(searchResult.type == 'complete') {
        assert.equal(searchResult.mapping.matchedResult[0].matchString, 'space');
        assert.equal(searchResult.mapping.matchedResult[1].matchString, ' ');
      }

      const nilResult = instance.handleNextNode();
      assert.equal(nilResult.type, 'none');
    });
  });
});