/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-29
 *
 * This file defines tests for the SearchSpace class of the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { LegacyQuotientSpur, models, LegacyQuotientRoot, quotientPathHasInputs } from '@keymanapp/lm-worker/test-index';

import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

export function buildSimplePathSplitFixture() {
  const rootPath = new LegacyQuotientRoot(testModel);

  const distrib1 = [
    { sample: {insert: 'c', deleteLeft: 0, id: 11}, p: 0.5 },
    { sample: {insert: 'r', deleteLeft: 0, id: 11}, p: 0.4 },
    { sample: {insert: 't', deleteLeft: 0, id: 11}, p: 0.1 }
  ];
  const path1 = new LegacyQuotientSpur(rootPath, distrib1, distrib1[0]);

  const distrib2 = [
    { sample: {insert: 'a', deleteLeft: 0, id: 12}, p: 0.7 },
    { sample: {insert: 'e', deleteLeft: 0, id: 12}, p: 0.3 }
  ];
  const path2 = new LegacyQuotientSpur(path1, distrib2, distrib2[0]);

  const distrib3 = [
    { sample: {insert: 'n', deleteLeft: 0, id: 13}, p: 0.8 },
    { sample: {insert: 'r', deleteLeft: 0, id: 13}, p: 0.2 }
  ];
  const path3 = new LegacyQuotientSpur(path2, distrib3, distrib3[0]);

  const distrib4 = [
    { sample: {insert: 't', deleteLeft: 0, id: 14}, p: 1 }
  ];
  const path4 = new LegacyQuotientSpur(path3, distrib4, distrib4[0]);

  return {
    paths: [rootPath, path1, path2, path3, path4],
    distributions: [distrib1, distrib2, distrib3, distrib4]
  };
}

describe('SearchQuotientSpur', () => {
  describe('constructor', () => {
    it('initializes from a lexical model', () => {
      const path = new LegacyQuotientRoot(testModel);
      assert.equal(path.inputCount, 0);
      assert.equal(path.codepointLength, 0);
      assert.isNumber(path.spaceId);
      assert.deepEqual(path.bestExample, {text: '', p: 1});
      assert.deepEqual(path.parents, []);
    });

    it('may be extended from root path', () => {
      const rootPath = new LegacyQuotientRoot(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const extendedPath = new LegacyQuotientSpur(rootPath, leadEdgeDistribution, leadEdgeDistribution[0]);

      assert.equal(extendedPath.inputCount, 1);
      assert.equal(extendedPath.codepointLength, 1);
      assert.isNumber(extendedPath.spaceId);
      assert.notEqual(extendedPath.spaceId, rootPath.spaceId);
      assert.deepEqual(extendedPath.bestExample, {text: 't', p: 0.5});
      assert.deepEqual(extendedPath.parents, [rootPath]);
      assert.deepEqual(extendedPath.inputs, leadEdgeDistribution);
      assert.deepEqual(extendedPath.inputSegments, [
        {
          segment: {
            trueTransform: leadEdgeDistribution[0].sample,
            transitionId: leadEdgeDistribution[0].sample.id,
            start: 0
          },
          bestProbFromSet: leadEdgeDistribution[0].p,
          // Just write in the variable-value entry; the rest should match perfectly.
          subsetId: extendedPath.inputSegments[0].subsetId
        }
      ]);

      // Assert the root is unchanged.
      assert.equal(rootPath.inputCount, 0);
      // Should (still) have codepointLength == 0 once it's defined.
      assert.deepEqual(rootPath.bestExample, {text: '', p: 1});
      assert.deepEqual(rootPath.parents, []);
    });

    it('may be built from arbitrary prior SearchQuotientSpur', () => {
      const rootPath = new LegacyQuotientRoot(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];
      const inputClone = leadEdgeDistribution.map(e => ({...e}));

      const length1Path = new LegacyQuotientSpur(
        rootPath,
        leadEdgeDistribution,
        leadEdgeDistribution[0]
      );

      const tailEdgeDistribution = [
        {sample: {insert: 'r', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'e', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'h', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new LegacyQuotientSpur(
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
          segment: {
            trueTransform: leadEdgeDistribution[0].sample,
            transitionId: leadEdgeDistribution[0].sample.id,
            start: 0
          },
          bestProbFromSet: leadEdgeDistribution[0].p,
          // Just write in the variable-value entry; the rest should match perfectly.
          subsetId: length2Path.inputSegments[0].subsetId
        }, {
          segment: {
            trueTransform: tailEdgeDistribution[0].sample,
            transitionId: tailEdgeDistribution[0].sample.id,
            start: 0
          },
          bestProbFromSet: tailEdgeDistribution[0].p,
          // Just write in the variable-value entry; the rest should match perfectly.
          subsetId: length2Path.inputSegments[1].subsetId
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

    it('throws if input and input-source transition IDs mismatch', () => {
      const rootPath = new LegacyQuotientRoot(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      assert.throws(() => new LegacyQuotientSpur(rootPath, leadEdgeDistribution, {
        ...leadEdgeDistribution[0],
        sample: {...leadEdgeDistribution[0].sample, id: 15}
      }));
    });

    it('may extend with a Transform inserting multiple codepoints', () => {
      const rootPath = new LegacyQuotientRoot(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];
      const inputClone = leadEdgeDistribution.map(e => ({...e}));

      const length1Path = new LegacyQuotientSpur(
        rootPath,
        leadEdgeDistribution,
        leadEdgeDistribution[0]
      );

      const tailEdgeDistribution = [
        {sample: {insert: 'ri', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'er', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'hi', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new LegacyQuotientSpur(
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
          segment: {
            trueTransform: leadEdgeDistribution[0].sample,
            transitionId: leadEdgeDistribution[0].sample.id,
            start: 0
          },
          bestProbFromSet: leadEdgeDistribution[0].p,
          // Just write in the variable-value entry; the rest should match perfectly.
          subsetId: length2Path.inputSegments[0].subsetId
        }, {
          segment: {
            trueTransform: tailEdgeDistribution[0].sample,
            transitionId: tailEdgeDistribution[0].sample.id,
            start: 0
          },
          bestProbFromSet: tailEdgeDistribution[0].p,
          // Just write in the variable-value entry; the rest should match perfectly.
          subsetId: length2Path.inputSegments[1].subsetId
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

  describe('fixture construction', () => {
    it('setup: buildSimplePathSplitFixture() constructs paths properly', () => {
      const { paths, distributions } = buildSimplePathSplitFixture();
      const pathToSplit = paths[4];

      assert.equal(pathToSplit.inputCount, 4);
      assert.equal(distributions.length, pathToSplit.inputCount);
      assert.equal(pathToSplit.codepointLength, 4); // one char per input, no deletions anywhere
      // Per assertions documented in the setup above.
      assert.deepEqual(pathToSplit.bestExample, distributions.reduce(
        (constructing, current) => ({text: constructing.text + current[0].sample.insert, p: constructing.p * current[0].p}),
        {text: '', p: 1})
      );
      assert.deepEqual(pathToSplit.parents[0].bestExample, distributions.slice(0, pathToSplit.inputCount-1).reduce(
        (constructing, current) => ({text: constructing.text + current[0].sample.insert, p: constructing.p * current[0].p}),
        {text: '', p: 1})
      );
      assert.isTrue(quotientPathHasInputs(pathToSplit, distributions));
    });
  });
});