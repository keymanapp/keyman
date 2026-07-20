/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-04
 *
 * This file defines tests for the SubstitutionQuotientSpur class of the
 * predictive-text correction-search engine's search graph.
 */

import { assert } from 'chai';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import {
  generateSubsetId,
  models,
  SearchQuotientRoot,
  SubstitutionQuotientSpur
} from '@keymanapp/lm-worker/test-index';

import { buildCantLinearFixture } from '../../helpers/buildCantLinearFixture.js';
import { analyzeQuotientNodeResults } from '../../helpers/analyzeQuotientNodeResults.js';

import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

describe('SubstitutionQuotientSpur', () => {
  describe('constructor', () => {
    it('may be extended from root path', () => {
      const rootPath = new SearchQuotientRoot(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const extendedPath = new SubstitutionQuotientSpur(rootPath, leadEdgeDistribution, leadEdgeDistribution[0]);

      assert.equal(extendedPath.inputCount, 1);
      assert.equal(extendedPath.codepointLength, 1);
      assert.isNumber(extendedPath.spaceId);
      assert.notEqual(extendedPath.spaceId, rootPath.spaceId);
      assert.deepEqual(extendedPath.bestExample, {text: 't', p: 0.5});
      assert.deepEqual(extendedPath.parents, [rootPath]);
      assert.deepEqual(extendedPath.inputs, leadEdgeDistribution);
      assert.deepEqual(extendedPath.inputSegments, [
        {
          transitionId: leadEdgeDistribution[0].sample.id,
          start: 0
        }
      ]);

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

      const length2Path = new SubstitutionQuotientSpur(
        length1Path,
        tailEdgeDistribution,
        tailEdgeDistribution[0]
      );

      // Verify that the prior distribution remains fully unaltered.
      assert.deepEqual(leadEdgeDistribution, inputClone);

      assert.equal(length2Path.inputCount, 2);
      assert.equal(length2Path.codepointLength, 2);
      assert.isNumber(length2Path.spaceId);
      assert.notEqual(length2Path.spaceId, length1Path.spaceId);
      assert.deepEqual(length2Path.bestExample, {text: 'tr', p: leadEdgeDistribution[0].p * tailEdgeDistribution[0].p});
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

    it('may extend with a Transform inserting multiple codepoints', () => {
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
        {sample: {insert: 'ri', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'er', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'hi', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new SubstitutionQuotientSpur(
        length1Path,
        tailEdgeDistribution,
        tailEdgeDistribution[0]
      );

      // Verify that the prior distribution remains fully unaltered.
      assert.deepEqual(leadEdgeDistribution, inputClone);

      assert.equal(length2Path.inputCount, 2);
      assert.equal(length2Path.codepointLength, 3);
      assert.isNumber(length2Path.spaceId);
      assert.notEqual(length2Path.spaceId, length1Path.spaceId);
      assert.deepEqual(length2Path.bestExample, {text: 'tri', p: leadEdgeDistribution[0].p * tailEdgeDistribution[0].p});
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
    it('changes when input source subset IDs differ', () => {
      const root = new SearchQuotientRoot(testModel);

      const {distributions} = buildCantLinearFixture();
      const inputSrc = {
        segment: {
          transitionId: distributions[0][0].sample.id,
          start: 0
        },
        subsetId: generateSubsetId(),
        bestProbFromSet: distributions[0][0].p
      };

      const spur1 = new SubstitutionQuotientSpur(root, distributions[0], {
        ...inputSrc,
        subsetId: generateSubsetId()
      });
      const spur2 = new SubstitutionQuotientSpur(root, distributions[0], {
        ...inputSrc,
        subsetId: generateSubsetId()
      });

      assert.notEqual(spur1.edgeKey, spur2.edgeKey);
    });

    it('changes when different parts of the same input source are used', () => {
      const root = new SearchQuotientRoot(testModel);

      const {distributions} = buildCantLinearFixture();
      const inputSrc = {
        segment: {
          transitionId: distributions[0][0].sample.id,
          start: 0
        },
        subsetId: generateSubsetId(),
        bestProbFromSet: distributions[0][0].p
      };

      const spur1 = new SubstitutionQuotientSpur(root, distributions[0], inputSrc);
      const spur2 = new SubstitutionQuotientSpur(root, distributions[0], {
        ...inputSrc,
        segment: {
          ...inputSrc.segment,
          end: 1
        }
      });
      const spur3 = new SubstitutionQuotientSpur(root, distributions[0], {
        ...inputSrc,
        segment: {
          ...inputSrc.segment,
          start: inputSrc.segment.start + 1
        }
      });

      assert.notEqual(spur1.edgeKey, spur2.edgeKey);
      assert.notEqual(spur2.edgeKey, spur3.edgeKey);
      assert.notEqual(spur3.edgeKey, spur1.edgeKey);
    });
  });

  describe('handleNextNode()', () => {
    it('outputs results that directly match inputs', () => {
      const canPath = buildCantLinearFixture().paths[3];

      const matchTargets = [
        'can',
        'car',
        'cen',  // 'cent' and 'center' are supported in this test model.
      ];
      const analysis = analyzeQuotientNodeResults(canPath, matchTargets);

      assert.sameMembers(analysis.found, matchTargets);
      assert.isEmpty(analysis.foundWithDuplicates);
    });

    it('outputs results that substitute inputs', () => {
      const canPath = buildCantLinearFixture().paths[3];

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
      const analysis = analyzeQuotientNodeResults(canPath, matchTargets);

      assert.sameMembers(analysis.found, matchTargets);
      assert.isEmpty(analysis.foundWithDuplicates);
    });

    it('does not output results that insert characters as needed', () => {
      const canPath = buildCantLinearFixture().paths[3];

      const matchTargets = [
        'char',  // 'c' + (insert) 'h' + 'ar'
        'than',  // 't' + (insert) 'h' + 'an'
      ];
      const analysis = analyzeQuotientNodeResults(canPath, matchTargets);

      assert.sameMembers(analysis.found, []);
      assert.sameMembers(analysis.missing, matchTargets);
      assert.isEmpty(analysis.foundWithDuplicates);
    });

    it('does not output results that delete incoming keystrokes as needed', () => {
      const canPath = buildCantLinearFixture().paths[3];

      const matchTargets = [
        // Delete only first
        'an',   // (delete) 'c'/'r'/'t' + 'an', for 'and' and 'any'
        'en',   // (delete) 'c'/'r'/'t' + 'en', for 'end',
        // Even delete second
        'n',    // model possesses words starting with just 'n'
        't'     // ... and 't'.
      ];
      const analysis = analyzeQuotientNodeResults(canPath, matchTargets);

      assert.sameMembers(analysis.found, []);
      assert.sameMembers(analysis.missing, matchTargets);
      assert.isEmpty(analysis.foundWithDuplicates);
    });
  });
});