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
  DeletionQuotientSpur,
  models,
  SearchQuotientRoot,
  SubstitutionQuotientSpur
} from '@keymanapp/lm-worker/test-index';

import TrieModel = models.TrieModel;

import { buildCantLinearFixture } from '../../helpers/buildCantLinearFixture.js';
import { buildQuotientDocFixture } from '../../helpers/buildQuotientDocFixture.js';

import { analyzeQuotientNodeResults } from '../../helpers/analyzeQuotientNodeResults.js';

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

describe('DeletionQuotientSpur', () => {
  describe('constructor', () => {
    it('may be extended from root path', () => {
      const rootPath = new SearchQuotientRoot(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const extendedPath = new DeletionQuotientSpur(rootPath, leadEdgeDistribution, leadEdgeDistribution[0]);

      assert.equal(extendedPath.inputCount, 1);
      assert.equal(extendedPath.codepointLength, 0);
      assert.isNumber(extendedPath.spaceId);
      assert.notEqual(extendedPath.spaceId, rootPath.spaceId);
      assert.deepEqual(extendedPath.bestExample, { text: '', p: 1 } );

      assert.deepEqual(extendedPath.parents, [rootPath]);
      assert.deepEqual(extendedPath.inputs, leadEdgeDistribution);

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

      const tailEdgeDistribution = [
        {sample: {insert: 'r', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'e', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'h', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new DeletionQuotientSpur(
        length1Path,
        tailEdgeDistribution,
        tailEdgeDistribution[0]
      );

      // Verify that the prior distribution remains fully unaltered.
      assert.deepEqual(leadEdgeDistribution, inputClone);

      assert.equal(length2Path.inputCount, 2);
      assert.equal(length2Path.codepointLength, 1);
      assert.isNumber(length2Path.spaceId);
      assert.notEqual(length2Path.spaceId, length1Path.spaceId);
      assert.deepEqual(length2Path.bestExample, length1Path.bestExample);
      assert.deepEqual(length2Path.parents, [length1Path]);
      assert.deepEqual(length2Path.inputs, tailEdgeDistribution);
      assert.deepEqual(length2Path.inputSegments, [
        {
          transitionId: leadEdgeDistribution[0].sample.id,
          start: 0
        }, {
          transitionId: tailEdgeDistribution[0].sample.id,
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
    it('is different for different delete locations', () => {
      const {k1c0, k2c0} = buildQuotientDocFixture().nodes;

      assert.notEqual(k2c0.edgeKey, k1c0.edgeKey);
    });

    it('is different from the parent node\'s key', () => {
      const { k1c1_ab, k2c1_del, k1c2_cd, k2c2_del } = buildQuotientDocFixture().spurs;

      assert.notEqual(k2c1_del.edgeKey, k1c1_ab.edgeKey);
      assert.notEqual(k2c2_del.edgeKey, k1c2_cd.edgeKey);
    });
  });

  describe('handleNextNode()', () => {
    it('does not output results that directly match inputs', () => {
      const caPath = buildCantLinearFixture().paths[2];
      const distrib = buildCantLinearFixture().distributions[2]; // for the third entry.
      const deletionPath = new DeletionQuotientSpur(caPath, distrib, distrib[0]);

      const matchTargets = [
        'can',
        'car',
        'cen',  // 'cent' and 'center' are supported in this test model.
      ];
      const analysis = analyzeQuotientNodeResults(deletionPath, matchTargets);

      assert.sameMembers(analysis.found, []);
      assert.sameMembers(analysis.missing, matchTargets);
      assert.isEmpty(analysis.foundWithDuplicates);
    });

    it('does not output results that substitute inputs', () => {
      const caPath = buildCantLinearFixture().paths[2];
      const distrib = buildCantLinearFixture().distributions[2]; // for the third entry.
      const deletionPath = new DeletionQuotientSpur(caPath, distrib, distrib[0]);

      const matchTargets = [
        // Replacement of first char
        'man',
        'far',
        // Replacement of second char
        'con',  // 'consider' and variants thereof are also supported.
        'cor',  // 'corner'
        // Replacement of third char
        'cal',  // 'call'
      ];
      const analysis = analyzeQuotientNodeResults(deletionPath, matchTargets);

      assert.sameMembers(analysis.found, []);
      assert.sameMembers(analysis.missing, matchTargets);
      assert.isEmpty(analysis.foundWithDuplicates);
    });

    it('does not output results that insert characters as needed', () => {
      const caPath = buildCantLinearFixture().paths[2];
      const distrib = buildCantLinearFixture().distributions[2]; // for the third entry.
      const deletionPath = new DeletionQuotientSpur(caPath, distrib, distrib[0]);

      const matchTargets = [
        'can',  // 'can' + (insert) 'n' (=> 'cannot')
        'car',  // 'car' + (insert) 'e'
        'ran',  // 'ran' + (insert) 'g' (=> 'range')
        'cann',  // 'can' + (insert) 'n' (=> 'cannot')
        'care',  // 'car' + (insert) 'e'
        'rang',  // 'ran' + (insert) 'g' (=> 'range')
      ];
      const analysis = analyzeQuotientNodeResults(deletionPath, matchTargets);

      assert.sameMembers(analysis.found, []);
      assert.sameMembers(analysis.missing, matchTargets);
      assert.isEmpty(analysis.foundWithDuplicates);
    });

    it('outputs results that delete incoming keystrokes as needed', () => {
      const caPath = buildCantLinearFixture().paths[2];
      const distrib = buildCantLinearFixture().distributions[2]; // for the third entry.
      const deletionPath = new DeletionQuotientSpur(caPath, distrib, distrib[0]);

      const matchTargets = [
        // Deletes the last char.
        'ca',
        'ce',
        're'
      ];
      const analysis = analyzeQuotientNodeResults(deletionPath, matchTargets);

      assert.sameMembers(analysis.found, matchTargets);
      assert.sameMembers(analysis.missing, []);
      assert.isEmpty(analysis.foundWithDuplicates);
    });
  });
});