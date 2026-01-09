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
import { models, SearchQuotientSpur } from '@keymanapp/lm-worker/test-index';

import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

export function buildSimplePathSplitFixture() {
  const rootPath = new SearchQuotientSpur(testModel);

  const distrib1 = [
    { sample: {insert: 'c', deleteLeft: 0, id: 11}, p: 0.5 },
    { sample: {insert: 'r', deleteLeft: 0, id: 11}, p: 0.4 },
    { sample: {insert: 't', deleteLeft: 0, id: 11}, p: 0.1 }
  ];
  const path1 = new SearchQuotientSpur(rootPath, distrib1, distrib1[0].p);

  const distrib2 = [
    { sample: {insert: 'a', deleteLeft: 0, id: 12}, p: 0.7 },
    { sample: {insert: 'e', deleteLeft: 0, id: 12}, p: 0.3 }
  ];
  const path2 = new SearchQuotientSpur(path1, distrib2, distrib2[0].p);

  const distrib3 = [
    { sample: {insert: 'n', deleteLeft: 0, id: 13}, p: 0.8 },
    { sample: {insert: 'r', deleteLeft: 0, id: 13}, p: 0.2 }
  ];
  const path3 = new SearchQuotientSpur(path2, distrib3, distrib3[0].p);

  const distrib4 = [
    { sample: {insert: 't', deleteLeft: 0, id: 14}, p: 1 }
  ];
  const path4 = new SearchQuotientSpur(path3, distrib4, distrib4[0].p);

  return {
    paths: [rootPath, path1, path2, path3, path4],
    distributions: [distrib1, distrib2, distrib3, distrib4]
  };
}

describe('SearchPath', () => {
  describe('constructor', () => {
    it('initializes from a lexical model', () => {
      const path = new SearchQuotientSpur(testModel);
      assert.equal(path.inputCount, 0);
      assert.isNumber(path.spaceId);
      assert.deepEqual(path.bestExample, {text: '', p: 1});
      assert.deepEqual(path.parents, []);
      assert.isNotOk(path.inputs);
    });

    it('may be extended from root path', () => {
      const rootPath = new SearchQuotientSpur(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const extendedPath = new SearchQuotientSpur(rootPath, leadEdgeDistribution, leadEdgeDistribution[0].p);

      assert.equal(extendedPath.inputCount, 1);
      assert.isNumber(extendedPath.spaceId);
      assert.notEqual(extendedPath.spaceId, rootPath.spaceId);
      assert.deepEqual(extendedPath.bestExample, {text: 't', p: 0.5});
      assert.deepEqual(extendedPath.parents, [rootPath]);
      assert.deepEqual(extendedPath.inputs, leadEdgeDistribution);

      // Assert the root is unchanged.
      assert.equal(rootPath.inputCount, 0);
      // Should (still) have codepointLength == 0 once it's defined.
      assert.deepEqual(rootPath.bestExample, {text: '', p: 1});
      assert.deepEqual(rootPath.parents, []);
      assert.isNotOk(rootPath.inputs);
    });

    it('may be built from arbitrary prior SearchPath', () => {
      const rootPath = new SearchQuotientSpur(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];
      const inputClone = leadEdgeDistribution.map(e => ({...e}));

      const length1Path = new SearchQuotientSpur(
        rootPath,
        leadEdgeDistribution,
        leadEdgeDistribution[0].p
      );

      const tailEdgeDistribution = [
        {sample: {insert: 'r', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'e', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'h', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new SearchQuotientSpur(
        length1Path,
        tailEdgeDistribution,
        tailEdgeDistribution[0].p
      );

      // Verify that the prior distribution remains fully unaltered.
      assert.deepEqual(leadEdgeDistribution, inputClone);

      assert.equal(length2Path.inputCount, 2);
      assert.isNumber(length2Path.spaceId);
      assert.notEqual(length2Path.spaceId, length1Path.spaceId);
      assert.deepEqual(length2Path.bestExample, {text: 'tr', p: leadEdgeDistribution[0].p * tailEdgeDistribution[0].p});
      assert.deepEqual(length2Path.parents, [length1Path]);
      assert.deepEqual(length2Path.inputs, tailEdgeDistribution);

      assert.equal(length1Path.inputCount, 1);
      assert.isNumber(length1Path.spaceId);
      assert.notEqual(length1Path.spaceId, rootPath.spaceId);
      assert.deepEqual(length1Path.bestExample, {text: 't', p: 0.5});
      assert.deepEqual(length1Path.parents, [rootPath]);
      assert.deepEqual(length1Path.inputs, leadEdgeDistribution);
    });

    it('may extend with a Transform inserting multiple codepoints', () => {
      const rootPath = new SearchQuotientSpur(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];
      const inputClone = leadEdgeDistribution.map(e => ({...e}));

      const length1Path = new SearchQuotientSpur(
        rootPath,
        leadEdgeDistribution,
        leadEdgeDistribution[0].p
      );

      const tailEdgeDistribution = [
        {sample: {insert: 'ri', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'er', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'hi', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new SearchQuotientSpur(
        length1Path,
        tailEdgeDistribution,
        tailEdgeDistribution[0].p
      );

      // Verify that the prior distribution remains fully unaltered.
      assert.deepEqual(leadEdgeDistribution, inputClone);

      assert.equal(length2Path.inputCount, 2);
      assert.isNumber(length2Path.spaceId);
      assert.notEqual(length2Path.spaceId, length1Path.spaceId);
      assert.deepEqual(length2Path.bestExample, {text: 'tri', p: leadEdgeDistribution[0].p * tailEdgeDistribution[0].p});
      assert.deepEqual(length2Path.parents, [length1Path]);
      assert.deepEqual(length2Path.inputs, tailEdgeDistribution);

      assert.equal(length1Path.inputCount, 1);
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
      // Per assertions documented in the setup above.
      assert.deepEqual(pathToSplit.bestExample, distributions.reduce(
        (constructing, current) => ({text: constructing.text + current[0].sample.insert, p: constructing.p * current[0].p}),
        {text: '', p: 1})
      );
      assert.deepEqual(pathToSplit.parents[0].bestExample, distributions.slice(0, pathToSplit.inputCount-1).reduce(
        (constructing, current) => ({text: constructing.text + current[0].sample.insert, p: constructing.p * current[0].p}),
        {text: '', p: 1})
      );
      assert.isTrue(pathToSplit.hasInputs(distributions));
    });
  });

  describe('hasInputs()', () => {
    it('matches an empty array on root SearchPaths', () => {
      assert.isTrue(new SearchQuotientSpur(testModel).hasInputs([]));
    });

    it('matches all path inputs when provided in proper order', () => {
      const { paths, distributions } = buildSimplePathSplitFixture();
      assert.isTrue(paths[4].hasInputs(distributions));
    });

    it('does not match when any path input component is missing', () => {
      const { paths, distributions } = buildSimplePathSplitFixture();
      assert.isFalse(paths[4].hasInputs(distributions.slice(1)));
      assert.isFalse(paths[4].hasInputs(distributions.slice(2)));
      assert.isFalse(paths[4].hasInputs(distributions.slice(3)));
      assert.isFalse(paths[4].hasInputs(distributions.slice(0, 3)));
      assert.isFalse(paths[4].hasInputs(distributions.slice(0, 1).concat(distributions.slice(2))));
    });

    it('does not match when path inputs are not in proper order', () => {
      const { paths, distributions } = buildSimplePathSplitFixture();
      assert.isFalse(paths[4].hasInputs(distributions.slice().reverse()));

      // Random shuffle.
      let shuffled: typeof distributions;
      let isShuffled: boolean;
      do {
        shuffled = distributions.slice().sort(() => Math.random() * 2 - 1);
        // Validate that we actually shuffled - that we didn't land on the original order!
        isShuffled = false;
        for(let i = 0; i < distributions.length; i++) {
          if(distributions[i] != shuffled[i]) {
            isShuffled = true;
            break;
          }
        }
      } while(!isShuffled);
      assert.isFalse(paths[4].hasInputs(shuffled));
    });
  });
});