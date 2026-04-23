/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-04-14
 *
 * This file defines tests for the `QuotientNodeFinalizer` class, which is used
 * to ensure results forward properly to all tokenization patterns and can be
 * used to enforce a correction-only state on tokens not adjacent to the caret.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import {
  EDIT_DISTANCE_COST_SCALE,
  generateSubsetId,
  models,
  PathInputProperties,
  PathResult,
  QuotientNodeFinalizer,
  SearchQuotientNode,
  SearchQuotientRoot,
  SubstitutionQuotientSpur,
  TokenResultMapping
} from '@keymanapp/lm-worker/test-index';

import Distribution = LexicalModelTypes.Distribution;
import TrieModel = models.TrieModel;
import Transform = LexicalModelTypes.Transform;

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
    [ ['r', 0.2] ],
    [ ['e', 1] ],
    [ ['f', 1] ],
    [ ['o', 1] ],
    [ ['r', 1] ],
    [ ['e', 1] ],
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
  });

  let quotientNodes: SearchQuotientNode[] = [new SearchQuotientRoot(plainModel)];
  for(let i=0; i < 9; i++) {
    quotientNodes.push(new SubstitutionQuotientSpur(quotientNodes[i], distributions[i], inputSources[i]));
  }

  return quotientNodes;
}

describe('QuotientNodeFinalizer', () => {
  describe('handleNextNode', () => {
    it('finds corrections and predictions', () => {
      const fixture = buildFixture_therefore();

      const therefo = new QuotientNodeFinalizer(fixture[7], true);

      let searchResult: PathResult<TokenResultMapping>;
      do {
        searchResult = therefo.handleNextNode();
      } while(searchResult.type == 'intermediate');

      assert.equal(searchResult.type, 'complete');
      if(searchResult.type == 'complete') {
        assert.approximately(searchResult.mapping.totalCost, -Math.log(therefo.bestExample.p), Number.EPSILON * 1000);
        assert.isNotNaN(searchResult.cost);
        assert.equal(searchResult.cost, searchResult.mapping.totalCost);
      } else {
        return;
      }

      searchResult = therefo.handleNextNode();
      // There should be more searching to perform before aborting.
      assert.notEqual(searchResult.type, 'none');

      // However, no other valid results are within correction range
      // while rooted on 6 input transforms.
      do {
        searchResult = therefo.handleNextNode();
      } while(searchResult.type == 'intermediate');

      assert.equal(searchResult.type, 'none');
    });

    it('finds only corrections when predictions are forbidden', () => {
      const fixture = buildFixture_therefore();

      const therefo = new QuotientNodeFinalizer(fixture[7], false);

      let searchResult: PathResult<TokenResultMapping>;
      do {
        searchResult = therefo.handleNextNode();
      } while(searchResult.type == 'intermediate');

      assert.equal(searchResult.type, 'complete');
      if(searchResult.type == 'complete') {
        assert.isAbove(searchResult.mapping.totalCost, -Math.log(therefo.bestExample.p));

        // There are two codepoints missing that are necessary to complete a
        // full word with the represented prefix.  Check that the penalty is set
        // appropriately, accounting for floating-point precision issues.
        assert.isAtLeast(searchResult.mapping.totalCost, -Math.log(therefo.bestExample.p) + EDIT_DISTANCE_COST_SCALE * 1.99);
        assert.isNotNaN(searchResult.cost);
        assert.equal(searchResult.cost, searchResult.mapping.totalCost);
      } else {
        return;
      }

      searchResult = therefo.handleNextNode();
      // There should be more searching to perform before aborting.
      assert.notEqual(searchResult.type, 'none');

      do {
        searchResult = therefo.handleNextNode();
      } while(searchResult.type == 'intermediate');

      // However, no other valid results are within correction range
      // while rooted on 6 input transforms.
      assert.equal(searchResult.type, 'none');
    });
  });
});