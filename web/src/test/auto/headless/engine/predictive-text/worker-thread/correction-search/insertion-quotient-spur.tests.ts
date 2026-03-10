/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-06
 *
 * This file defines tests for the InsertionQuotientSpur class of the
 * predictive-text correction-search engine's search graph.
 */

import { assert } from 'chai';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import {
  InsertionQuotientSpur,
  models,
  PathResult,
  SearchQuotientNode,
  SearchQuotientRoot,
  SubstitutionQuotientSpur
} from '@keymanapp/lm-worker/test-index';

import TrieModel = models.TrieModel;

import { buildCantLinearFixture } from '../../helpers/buildCantLinearFixture.js';
import { buildQuotientDocFixture } from '../../helpers/buildQuotientDocFixture.js';

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

function assertResultsFromQuotientNode(path: SearchQuotientNode, results: string[], shouldMatch: boolean, strictSpaceMatch?: boolean) {
  let matchCount = 0;

  let result: PathResult = path.handleNextNode();
  while(result.type != 'none') {
    if(result.type == 'complete' && (!strictSpaceMatch || result.finalNode.spaceId == path.spaceId)) {
      const resultKey = result.finalNode.resultKey;
      if(results.find((entry) => entry == resultKey)) {
        matchCount++;
      }
    }

    if(matchCount == results.length) {
      break;
    }

    result = path.handleNextNode();
  }

  if(shouldMatch) {
    assert.notEqual(result.type, 'none');
    assert.equal(matchCount, results.length);
  } else {
    assert.equal(result.type, 'none');
    assert.equal(matchCount, 0);
  }
}

describe('InsertionQuotientSpur', () => {
  describe('constructor', () => {
    it('may be extended from root path', () => {
      const rootPath = new SearchQuotientRoot(testModel);
      const extendedPath = new InsertionQuotientSpur(rootPath);

      assert.equal(extendedPath.inputCount, 0);
      assert.equal(extendedPath.codepointLength, 1);
      assert.isNumber(extendedPath.spaceId);
      assert.notEqual(extendedPath.spaceId, rootPath.spaceId);
      assert.equal(extendedPath.bestExample.text.length, 1);
      assert.equal(extendedPath.bestExample.p, 1);

      assert.deepEqual(extendedPath.parents, [rootPath]);
      assert.deepEqual(extendedPath.inputs, null);

      // Assert the root is unchanged.
      assert.equal(rootPath.inputCount, 0);

      // Should (still) have codepointLength == 0 once it's defined.
      assert.deepEqual(rootPath.bestExample, {text: '', p: 1});
      assert.deepEqual(rootPath.parents, []);
    });

    it('may be built from arbitrary prior SearchQuotientSpur', () => {
      const rootPath = new SearchQuotientRoot(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];
      const inputClone = leadEdgeDistribution.map(e => ({...e}));

      const length1Path = new SubstitutionQuotientSpur(
        rootPath,
        leadEdgeDistribution,
        leadEdgeDistribution[0]
      );

      const length2Path = new InsertionQuotientSpur(length1Path);

      // Verify that the prior distribution remains fully unaltered.
      assert.deepEqual(leadEdgeDistribution, inputClone);

      assert.equal(length2Path.inputCount, 1);
      assert.equal(length2Path.codepointLength, 2);
      assert.isNumber(length2Path.spaceId);
      assert.notEqual(length2Path.spaceId, length1Path.spaceId);
      assert.equal(length2Path.bestExample.text.length, 2);
      assert.equal(length2Path.bestExample.p, leadEdgeDistribution[0].p);
      assert.deepEqual(length2Path.parents, [length1Path]);
      assert.deepEqual(length2Path.inputs, null);
      assert.deepEqual(length2Path.inputSegments, [
        {
          transitionId: leadEdgeDistribution[0].sample.id,
          start: 0
        }
      ]);

      assert.equal(length1Path.inputCount, 1);
      assert.equal(length1Path.codepointLength, 1);
      assert.isNumber(length1Path.spaceId);
      assert.notEqual(length1Path.spaceId, rootPath.spaceId);
      assert.deepEqual(length1Path.bestExample, {text: 't', p: 0.5});
      assert.deepEqual(length1Path.parents, [rootPath]);
      assert.deepEqual(length1Path.inputs, leadEdgeDistribution);
    });
  });

  describe('.edgeKey', () => {
    it('is different for different insert locations', () => {
      const {sc1, sc2} = buildQuotientDocFixture().nodes;
      assert.notEqual(sc1.edgeKey, sc2.edgeKey);
    });

    it('is different for an insert and the spur with the most recent processed input', () => {
      const { k1c2_cd, k1c3_ins } = buildQuotientDocFixture().spurs;
      assert.notEqual(k1c2_cd.edgeKey, k1c3_ins.edgeKey);

      const k1c2_cd_ins = new InsertionQuotientSpur(k1c2_cd);
      assert.notEqual(k1c2_cd_ins.edgeKey, k1c2_cd.edgeKey);
    });
  });

  describe('handleNextNode()', () => {
    it('forwards results that match the parent node\'s conditions', () => {
      const canPath = buildCantLinearFixture().paths[3];
      const followingInsert = new InsertionQuotientSpur(canPath);

      const matchTargets = [
        'can',
        'car',
        'cen',  // 'cent' and 'center' are supported in this test model.
        // Replacement of first char
        'man',
        'far',
        // Replacement of second char
        'con',  // 'consider' and variants thereof are also supported.
        'cor',  // 'corner'
        // Replacement of third char
        'cal',  // 'call'
      ];
      assertResultsFromQuotientNode(followingInsert, matchTargets, true);
    });

    it('does not \'own\' results forwarded from the parent node', () => {
      const canPath = buildCantLinearFixture().paths[3];
      const followingInsert = new InsertionQuotientSpur(canPath);

      const matchTargets = [
        'can',
        'car',
        'cen',  // 'cent' and 'center' are supported in this test model.
        // Replacement of first char
        'man',
        'far',
        // Replacement of second char
        'con',  // 'consider' and variants thereof are also supported.
        'cor',  // 'corner'
        // Replacement of third char
        'cal',  // 'call'
      ];
      assertResultsFromQuotientNode(followingInsert, matchTargets, false, true);
    });

    it('outputs results that insert characters as needed', () => {
      const canPath = buildCantLinearFixture().paths[3];
      const followingInsert = new InsertionQuotientSpur(canPath);

      const matchTargets = [
        'cann',  // 'can' + (insert) 'n' (=> 'cannot')
        'care',  // 'car' + (insert) 'e'
        'rang',  // 'ran' + (insert) 'g' (=> 'range')
      ];
      assertResultsFromQuotientNode(followingInsert, matchTargets, true);
    });

    it('outputs results extending prior inserts if insertion spur is parent', () => {
      const canPath = buildCantLinearFixture().paths[3];
      const firstInsert = new InsertionQuotientSpur(canPath);
      const followingInsert = new InsertionQuotientSpur(firstInsert)

      const matchTargets = [
        'canno',  // 'can' + (insert) 'n' (=> 'cannot')
        'range',  // 'ran' + (insert) 'g' (=> 'range')
      ];
      assertResultsFromQuotientNode(followingInsert, matchTargets, true);
    });

    it('does not output results that delete incoming keystrokes as needed', () => {
      const canPath = buildCantLinearFixture().paths[3];
      const followingInsert = new InsertionQuotientSpur(canPath);

      const matchTargets = [
        // Delete only first
        'an',   // (delete) 'c'/'r'/'t' + 'an', for 'and' and 'any'
        'en',   // (delete) 'c'/'r'/'t' + 'en', for 'end',
        // Even delete second
        'n',    // model possesses words starting with just 'n'
        't',    // ... and 't'.
        // Delete only third
        'ca',
        'ce'
      ];
      assertResultsFromQuotientNode(followingInsert, matchTargets, false);
    });

    it.skip('does not output results when immediately following a deletion spur edit', () => {

    });
  });
});