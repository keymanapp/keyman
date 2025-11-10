/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-29
 *
 * This file defines tests for the SearchSpace class of the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { models, SearchCluster, SearchPath } from '@keymanapp/lm-worker/test-index';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;
import { KMWString } from '@keymanapp/web-utils';
const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

export const buildAlphabeticClusterFixtures = () => {
  const rootPath = new SearchPath(testModel);

  // consonant-cluster 1, insert 1, delete 0
  const distrib_c1_i1d0: Distribution<Transform> = [
    { sample: { insert: 'b', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.3 }, // most likely for id 11
    { sample: { insert: 'c', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.2 },
    { sample: { insert: 'd', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.1 },
  ];

  // consonant-cluster 1, insert 2, delete 0
  const distrib_c1_i2d0: Distribution<Transform> = [
    { sample: { insert: 'fg', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.16 },
    { sample: { insert: 'hj', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.14 },
    { sample: { insert: 'kl', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.1 },
  ];

  // keystrokes 1, codepoints 1, total inserts 1, delete 0
  const path_k1c1_i1d0 = new SearchPath(rootPath, distrib_c1_i1d0, distrib_c1_i1d0[0]);
  // keystrokes 1, codepoints 2, total inserts 2, delete 0
  const path_k1c2_i2d0 = new SearchPath(rootPath, distrib_c1_i2d0, distrib_c1_i1d0[0]);

  // Second input

  const distrib_v1_i1d0: Distribution<Transform> = [
    { sample: { insert: 'e', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.4 }, // most likely for id 12
    { sample: { insert: 'a', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.3 },
    { sample: { insert: 'i', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.1 },
    { sample: { insert: 'o', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.1 },
    { sample: { insert: 'u', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.1 },
  ];

  const path_k2c2_i2d0 = new SearchPath(path_k1c1_i1d0, distrib_v1_i1d0, distrib_v1_i1d0[0]);
  const path_k2c3_i3d0 = new SearchPath(path_k1c2_i2d0, distrib_v1_i1d0, distrib_v1_i1d0[0]);

  // Third input
  const distrib_v2_i1d0: Distribution<Transform> = [
    { sample: { insert: 'e', deleteLeft: 0, deleteRight: 0, id: 13 }, p: 0.15 }, // most likely for id 13
    { sample: { insert: 'a', deleteLeft: 0, deleteRight: 0, id: 13 }, p: 0.13 },
    { sample: { insert: 'i', deleteLeft: 0, deleteRight: 0, id: 13 }, p: 0.12 },
    { sample: { insert: 'o', deleteLeft: 0, deleteRight: 0, id: 13 }, p: 0.11 },
    { sample: { insert: 'u', deleteLeft: 0, deleteRight: 0, id: 13 }, p: 0.09 },
  ]; // 0.60 total

  const distrib_v2_i1d1: Distribution<Transform> = [
    { sample: { insert: 'á', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.05 },
    { sample: { insert: 'é', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.06 },
    { sample: { insert: 'í', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.04 },
    { sample: { insert: 'ó', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.03 },
    { sample: { insert: 'ú', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.02 },
  ]; // 0.2 total

  const distrib_v2_i2d1: Distribution<Transform> = [
    { sample: { insert: 'áá', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.05 },
    { sample: { insert: 'éé', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.06 },
    { sample: { insert: 'íí', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.04 },
    { sample: { insert: 'óó', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.03 },
    { sample: { insert: 'úú', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.02 },
  ]; // 0.2 total

  const path_k3c2_i3d1 = new SearchPath(path_k2c2_i2d0, distrib_v2_i1d1, distrib_v2_i1d0[0]);

  const path_k3c3_i3d0  = new SearchPath(path_k2c2_i2d0, distrib_v2_i1d0, distrib_v2_i1d0[0]);
  const path_k3c3_i4d1a = new SearchPath(path_k2c2_i2d0, distrib_v2_i2d1, distrib_v2_i1d0[0]);
  const path_k3c3_i4d1b = new SearchPath(path_k2c3_i3d0, distrib_v2_i1d1, distrib_v2_i1d0[0]);

  // both are built on path k1c2 (splits at index 1)
  const path_k3c4_i4d0 = new SearchPath(path_k2c3_i3d0, distrib_v2_i1d0, distrib_v2_i1d0[0]);
  const path_k3c4_i5d1 = new SearchPath(path_k2c3_i3d0, distrib_v2_i2d1, distrib_v2_i1d0[0]);

  const cluster_k3c3 = new SearchCluster([path_k3c3_i3d0, path_k3c3_i4d1a, path_k3c3_i4d1b]);
  // both are built on path k1c2.
  const cluster_k3c4 = new SearchCluster([path_k3c4_i4d0, path_k3c4_i5d1]);

  // Input 4
  const distrib_c2_i1d0: Distribution<Transform> = [
    { sample: { insert: 'n', deleteLeft: 0, deleteRight: 0, id: 14 }, p: 0.12 },
    { sample: { insert: 'p', deleteLeft: 0, deleteRight: 0, id: 14 }, p: 0.08 },
  ];

  const distrib_c2_i2d0: Distribution<Transform> = [
    { sample: { insert: 'qr', deleteLeft: 0, deleteRight: 0, id: 14 }, p: 0.3 }, // most likely for id 14
    { sample: { insert: 'st', deleteLeft: 0, deleteRight: 0, id: 14 }, p: 0.2 },
    { sample: { insert: 'vw', deleteLeft: 0, deleteRight: 0, id: 14 }, p: 0.1 }
  ];

  const path_k4c4_i2 = new SearchPath(path_k3c2_i3d1, distrib_c2_i2d0, distrib_c2_i2d0[0]);
  const path_k4c4_i1 = new SearchPath(cluster_k3c3, distrib_c2_i1d0, distrib_c2_i2d0[0]);

  const path_k4c5_i2 = new SearchPath(cluster_k3c3, distrib_c2_i2d0, distrib_c2_i2d0[0]);
  const path_k4c5_i1 = new SearchPath(cluster_k3c4, distrib_c2_i1d0, distrib_c2_i2d0[0]);

  const path_k4c6 = new SearchPath(cluster_k3c4, distrib_c2_i2d0, distrib_c2_i2d0[0]);

  const cluster_k4c4 = new SearchCluster([path_k4c4_i2, path_k4c4_i1]);
  // former path is entirely based on cluster_k3c3, which has multiple paths
  // with a clean split at index 1.
  //
  // latter path is entirely based on cluster_k3c4, which itself is based
  // entirely on path_k1c2, which must be split at index 1.
  const cluster_k4c5 = new SearchCluster([path_k4c5_i2, path_k4c5_i1]);

  // Input 5 (currently used only for merge tests)
  const distrib_c3_i2d0: Distribution<Transform> = [
    { sample: { insert: 'xy', deleteLeft: 0, deleteRight: 0, id: 15 }, p: 0.6 } // most likely for id 15
  ];
  const distrib_c3_i1d0: Distribution<Transform> = [
    { sample: { insert: 'z', deleteLeft: 0, deleteRight: 0, id: 15 }, p: 0.4 }
  ];

  const path_k5c6_a = new SearchPath(cluster_k4c4, distrib_c3_i2d0, distrib_c3_i2d0[0]);
  const path_k5c6_b = new SearchPath(cluster_k4c5, distrib_c3_i1d0, distrib_c3_i2d0[0]);

  const cluster_k5c6 = new SearchCluster([path_k5c6_a, path_k5c6_b]);

  return {
    distributions: {
      1: {
        distrib_c1_i1d0,
        distrib_c1_i2d0
      },
      2: {
        distrib_v1_i1d0
      },
      3: {
        distrib_v2_i1d0,
        distrib_v2_i1d1,
        distrib_v2_i2d1
      },
      4: {
        distrib_c2_i1d0,
        distrib_c2_i2d0
      },
      5: {
        distrib_c3_i1d0,
        distrib_c3_i2d0
      }
    },
    paths: {
      0: {
        rootPath
      },
      1: {
        path_k1c1_i1d0,
        path_k1c2_i2d0,
      },
      2: {
        path_k2c2_i2d0,
        path_k2c3_i3d0,
      },
      3: {
        path_k3c2_i3d1,
        path_k3c3_i3d0,
        path_k3c3_i4d1a,
        path_k3c3_i4d1b,
        path_k3c4_i4d0,
        path_k3c4_i5d1,
      },
      4: {
        path_k4c4_i2,
        path_k4c4_i1,
        path_k4c5_i2,
        path_k4c5_i1,
        path_k4c6
      },
      5: {
        path_k5c6_a,
        path_k5c6_b
      }
    },
    clusters: {
      cluster_k3c3,
      cluster_k3c4,
      cluster_k4c4,
      cluster_k4c5,
      cluster_k5c6
    }
  }
}

describe('SearchCluster', () => {
  describe('constructor()', () => {
    it('initializes from root SearchPath', () => {
      const path = new SearchPath(testModel);
      const cluster = new SearchCluster([path]);
      assert.equal(cluster.inputCount, 0);
      assert.equal(cluster.codepointLength, 0);
      assert.isNumber(cluster.spaceId);
      assert.deepEqual(cluster.bestExample, {text: '', p: 1});
      assert.deepEqual(cluster.parents, [path]);
    });

    it('initializes from arbitrary SearchPath', () => {
      const rootPath = new SearchPath(testModel);

      const leadEdgeDistribution: Distribution<Transform> = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const length1Path = new SearchPath(
        rootPath,
        leadEdgeDistribution,
        leadEdgeDistribution[0]
      );

      const tailEdgeDistribution = [
        {sample: {insert: 'r', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'e', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'h', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new SearchPath(
        length1Path,
        tailEdgeDistribution,
        tailEdgeDistribution[0]
      );

      const cluster = new SearchCluster([length2Path]);

      assert.equal(cluster.inputCount, 2);
      assert.equal(cluster.codepointLength, 2);
      assert.isNumber(cluster.spaceId);
      assert.notEqual(cluster.spaceId, length1Path.spaceId);
      assert.deepEqual(cluster.bestExample, {text: 'tr', p: leadEdgeDistribution[0].p * tailEdgeDistribution[0].p});
      assert.deepEqual(cluster.parents, [length2Path]);
      assert.deepEqual(cluster.inputSegments, [
        {
          transitionId: leadEdgeDistribution[0].sample.id,
          start: 0
        }, {
          transitionId: tailEdgeDistribution[0].sample.id,
          start: 0
        }
      ]);
    });

    it('throws an error when SearchPath array is empty', () => {
      assert.throws(() => new SearchCluster([]));
    });

    it('throws an error if SearchPath .inputCount values don\'t match', () => {
      const rootPath = new SearchPath(testModel);

      const leadEdgeDistribution: Distribution<Transform> = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const length1Path = new SearchPath(
        rootPath,
        leadEdgeDistribution,
        leadEdgeDistribution[0]
      );

      const tailEdgeDistribution = [
        {sample: {insert: 'r', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'e', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'h', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new SearchPath(
        length1Path,
        tailEdgeDistribution,
        tailEdgeDistribution[0]
      );

      const altDistribution = [
        {sample: {insert: 'tr', deleteLeft: 0, id: 13 }, p: 0.6},
        {sample: {insert: 'te', deleteLeft: 0, id: 13 }, p: 0.25},
        {sample: {insert: 'th', deleteLeft: 0, id: 13 }, p: 0.15}
      ];
      const singleInputPath = new SearchPath(rootPath, altDistribution, altDistribution[0]);

      assert.throws(() => new SearchCluster([length2Path, singleInputPath]));
    });

    it('throws an error if SearchPath .sourceIdentifier values don\'t match', () => {
      const rootPath = new SearchPath(testModel);

      const distribution1: Distribution<Transform> = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const path1 = new SearchPath(
        rootPath,
        distribution1,
        distribution1[0]
      );

      const distribution2 = [
        {sample: {insert: 'r', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'e', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'h', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const path2 = new SearchPath(
        rootPath,
        distribution2,
        distribution2[0]
      );

      assert.throws(() => new SearchCluster([path1, path2]));
    });

    it('throws an error if SearchPath .codepointLength values don\'t match', () => {
      const rootPath = new SearchPath(testModel);

      const dist1: Distribution<Transform> = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const path1 = new SearchPath(
        rootPath,
        dist1,
        dist1[0]
      );

      const dist2 = [
        {sample: {insert: 'tr', deleteLeft: 0, id: 13 }, p: 0.6},
        {sample: {insert: 'te', deleteLeft: 0, id: 13 }, p: 0.25},
        {sample: {insert: 'th', deleteLeft: 0, id: 13 }, p: 0.15}
      ];
      const path2 = new SearchPath(rootPath, dist2, dist2[0]);

      assert.throws(() => new SearchCluster([path1, path2]));
    });
  });

  it('constructs a SearchPath + SearchCluster fixture properly', () => {
    // Finishing construction of the fixture without errors is itself an
    // implicit test.
    buildAlphabeticClusterFixtures();
  });

  // As it's used to validate other SearchCluster unit tests, it's wise to test
  // this early.
  describe('constituentPaths()', () => {
    it('enumerates clusters built only from paths', () => {
      const { paths, clusters } = buildAlphabeticClusterFixtures();

      const threeCharCluster = clusters.cluster_k3c3;
      assert.equal(threeCharCluster.constituentPaths.length, 3);
      // Root path counts for this.
      threeCharCluster.constituentPaths.forEach(sequence => assert.equal(sequence.length, 4));

      assert.includeDeepMembers(threeCharCluster.constituentPaths, [
        [
          paths[0].rootPath,
          paths[1].path_k1c1_i1d0,
          paths[2].path_k2c2_i2d0,
          paths[3].path_k3c3_i3d0
        ], [
          paths[0].rootPath,
          paths[1].path_k1c1_i1d0,
          paths[2].path_k2c2_i2d0,
          paths[3].path_k3c3_i4d1a
        ], [
          paths[0].rootPath,
          paths[1].path_k1c2_i2d0,
          paths[2].path_k2c3_i3d0,
          paths[3].path_k3c3_i4d1b
        ]
      ]);

      const fourCharCluster = clusters.cluster_k3c4;
      assert.equal(fourCharCluster.constituentPaths.length, 2);
      // Root path counts for this.
      fourCharCluster.constituentPaths.forEach(sequence => assert.equal(sequence.length, 4));

      assert.includeDeepMembers(fourCharCluster.constituentPaths, [
        [
          paths[0].rootPath,
          paths[1].path_k1c2_i2d0,
          paths[2].path_k2c3_i3d0,
          paths[3].path_k3c4_i4d0
        ], [
          paths[0].rootPath,
          paths[1].path_k1c2_i2d0,
          paths[2].path_k2c3_i3d0,
          paths[3].path_k3c4_i5d1
        ]
      ]);
    });

    it('enumerates clusters built from a mix of parent paths and clusters', () => {
      const { paths, clusters } = buildAlphabeticClusterFixtures();

      const fourCharCluster = clusters.cluster_k4c4;
      assert.equal(fourCharCluster.constituentPaths.length, 4);
      // Root path counts for this.
      fourCharCluster.constituentPaths.forEach(sequence => assert.equal(sequence.length, 5));

      assert.notIncludeDeepOrderedMembers(fourCharCluster.constituentPaths, [
        [
          paths[0].rootPath,
          paths[1].path_k1c1_i1d0,
          paths[2].path_k2c2_i2d0,
          paths[3].path_k3c3_i3d0,
          paths[4].path_k4c4_i2 // last component writes the third char
        ]
      ]);

      assert.includeDeepMembers(fourCharCluster.constituentPaths,
        // Should have all paths enumerable from cluster_k3c3 as a prefix.
        clusters.cluster_k3c3.constituentPaths.map((seq) => {
          seq.push(paths[4].path_k4c4_i1);
          return seq;
        })
      );

      assert.includeDeepMembers(fourCharCluster.constituentPaths, [
        [
          paths[0].rootPath,
          paths[1].path_k1c1_i1d0,
          paths[2].path_k2c2_i2d0,
          paths[3].path_k3c2_i3d1,
          paths[4].path_k4c4_i2
        ]
      ]);

      const fiveCharCluster = clusters.cluster_k4c5;
      assert.equal(fiveCharCluster.constituentPaths.length, 5);
      // Root path counts for this.
      fiveCharCluster.constituentPaths.forEach(sequence => assert.equal(sequence.length, 5));

      assert.includeDeepMembers(fiveCharCluster.constituentPaths,
        // Should have all paths enumerable from cluster_k3c3 as a prefix.
        clusters.cluster_k3c3.constituentPaths.map((seq) => {
          seq.push(paths[4].path_k4c5_i2);
          return seq;
        })
      );

      assert.includeDeepMembers(fiveCharCluster.constituentPaths,
        // Should have all paths enumerable from cluster_k3c4 as a prefix.
        clusters.cluster_k3c4.constituentPaths.map((seq) => {
          seq.push(paths[4].path_k4c5_i1);
          return seq;
        })
      );
    });
  });

  // Another unit-test utility method.  We could be more thorough, but the SearchPath
  // tests, + this, should probably be "enough".
  describe('hasInputs', () => {
    it('is able to match inputs against constituent input paths', () => {
      const { distributions, clusters } = buildAlphabeticClusterFixtures();

      const fourCharCluster = clusters.cluster_k4c4;
      const fiveCharCluster = clusters.cluster_k4c5;

      assert.isTrue(fourCharCluster.hasInputs([
        distributions[1].distrib_c1_i1d0,
        distributions[2].distrib_v1_i1d0,
        distributions[3].distrib_v2_i1d0,
        distributions[4].distrib_c2_i1d0
      ]));
      assert.isFalse(fiveCharCluster.hasInputs([
        distributions[1].distrib_c1_i1d0,
        distributions[2].distrib_v1_i1d0,
        distributions[3].distrib_v2_i1d0,
        distributions[4].distrib_c2_i1d0
      ]));

      assert.isFalse(fourCharCluster.hasInputs([
        distributions[1].distrib_c1_i1d0,
        distributions[2].distrib_v1_i1d0,
        distributions[3].distrib_v2_i1d0,
        distributions[4].distrib_c2_i2d0
      ]));
      assert.isTrue(fiveCharCluster.hasInputs([
        distributions[1].distrib_c1_i1d0,
        distributions[2].distrib_v1_i1d0,
        distributions[3].distrib_v2_i1d0,
        distributions[4].distrib_c2_i2d0
      ]));
    });
  });

  // Self-note:  reuse the 'alphabetic' fixture and split that.
  describe('split()', () => {
    // We'll use the 'five-char cluster' for these.
    it('properly splits the cluster at index 0', () => {
      const fixture = buildAlphabeticClusterFixtures();
      const fiveCharCluster = fixture.clusters.cluster_k4c5;

      const splitResults = fiveCharCluster.split(0);
      assert.equal(splitResults.length, 1);
      const splitResult = splitResults[0];

      assert.equal(splitResult[0].inputCount, 0);
      assert.equal(splitResult[1].inputCount, fiveCharCluster.inputCount);

      assert.isTrue(splitResult[0] instanceof SearchPath);
      assert.deepEqual(splitResult[0].constituentPaths, [
        [fixture.paths[0].rootPath]
      ]);

      // Also add assertions for the right-hand side.
      assert.isTrue(splitResult[1] instanceof SearchCluster);
      // Note that the path structures themselves, however, will be rebuilt - and with new spaceIDs.
      const pathSequenceToInputs = (sequence: SearchPath[]) => sequence.map(p => p.inputs);
      assert.deepEqual(splitResult[1].constituentPaths.length, fiveCharCluster.constituentPaths.length);
      // The input distributions should re-appear in the constituent paths, in full.
      assert.sameDeepMembers(
        splitResult[1].constituentPaths.map(pathSequenceToInputs),
        fiveCharCluster.constituentPaths.map(pathSequenceToInputs)
      );
    });

    it('properly splits the cluster at index 1', () => {
      const fixture = buildAlphabeticClusterFixtures();
      const fiveCharCluster = fixture.clusters.cluster_k4c5;

      const splitResults = fiveCharCluster.split(1);
      // One split array is "clean", while the other splits a keystroke in half.
      assert.equal(splitResults.length, 2);
      assert.sameMembers(splitResults.map((r) => r[0].inputCount + r[1].inputCount), [4, 5]);

      const cleanSplit = splitResults.find((r) => r[0].inputCount + r[1].inputCount == 4);

      assert.equal(cleanSplit[0].inputCount, 1);
      assert.equal(cleanSplit[1].inputCount, fiveCharCluster.inputCount - 1);

      assert.equal(cleanSplit[0], fixture.paths[1].path_k1c1_i1d0);

      // There is only one valid path with the clean split.  There is a cluster
      // in its history, but it reconverges before the end.
      assert.isTrue(cleanSplit[1] instanceof SearchPath);
      // Note that the path structures themselves, however, will be rebuilt - and with new spaceIDs.
      const pathSequenceToInputs = (sequence: SearchPath[]) => sequence.map(p => p.inputs);
      // One of the paths leading into cluster_k3c3 passes through the split-point.
      assert.deepEqual(cleanSplit[1].constituentPaths.length, fixture.clusters.cluster_k3c3.constituentPaths.length - 1);
      // The input distributions should re-appear in the constituent paths, in full.
      assert.includeDeepMembers( // not deepEqual.
        // superset
        fiveCharCluster.constituentPaths.map((sequence) => {
          return [sequence[0], ...sequence.slice(2)];
        }).map(pathSequenceToInputs),
        // subset
        cleanSplit[1].constituentPaths.map(pathSequenceToInputs)
      );

      const dirtySplit = splitResults.find((r) => r[0].inputCount + r[1].inputCount == 5);

      assert.equal(dirtySplit[0].inputCount, 1);
      assert.equal(dirtySplit[1].inputCount, fiveCharCluster.inputCount /* +1 - 1 */);

      assert.isTrue(dirtySplit[0] instanceof SearchPath);
      assert.deepEqual(dirtySplit[0].inputSegments[0], {
        ... fixture.paths[1].path_k1c2_i2d0.inputSource.segment,
        start: 0,
        end: 1
      });

      assert.isTrue(dirtySplit[1] instanceof SearchCluster);
      assert.equal(dirtySplit[1].constituentPaths.length, fixture.paths[4].path_k4c5_i1.constituentPaths.length + 1)
      assert.deepEqual(dirtySplit[1].inputSegments[0], {
        ... fixture.paths[1].path_k1c2_i2d0.inputSource.segment,
        start: 1,
      });

      assert.isOk(dirtySplit[1].constituentPaths.find((sequence) => {
        // One constituent path sequence should contain this path, which is part of cluster_k3c3.
        return sequence.find((path) => path.isSameSpace(fixture.paths[3].path_k3c3_i4d1b))
      }));

      assert.isOk(dirtySplit[1].constituentPaths.find((sequence) => {
        // One constituent path sequence should contain this path, which is part of cluster_k3c4.
        return sequence.find((path) => path.isSameSpace(fixture.paths[3].path_k3c4_i4d0))
      }));

      assert.isOk(dirtySplit[1].constituentPaths.find((sequence) => {
        // One constituent path sequence should contain this path, which is part of cluster_k3c4.
        return sequence.find((path) => path.isSameSpace(fixture.paths[3].path_k3c4_i5d1))
      }));

      // Deep-equal doesn't work; the paths aren't the same instances.
      // We COULD do a subsetId check, though.
      // assert.deepEqual(dirtySplit[1].constituentPaths.map((seq) => seq[4]), [
      //   fixture.paths[3].path_k3c3_i4d1b, fixture.paths[3].path_k3c4_i4d0, fixture.paths[3].path_k3c4_i5d1
      // ])
    });

    it('properly splits the cluster at index 2', () => {
      const fixture = buildAlphabeticClusterFixtures();
      const fiveCharCluster = fixture.clusters.cluster_k4c5;

      const splitResults = fiveCharCluster.split(2);
      // Two split arrays are "clean" (but differ by contributing keystroke
      // count), while the other splits a keystroke in half.
      assert.equal(splitResults.length, 3);
      assert.sameMembers(splitResults.map((r) => r[0].inputCount + r[1].inputCount), [4, 4, 5]);

      // Splits cleanly after 1 keystroke.
      const cleanSplit1 = splitResults.find((r) => r[0].inputCount == 1 && + r[1].inputCount == 3);
      assert.isTrue(cleanSplit1[0] instanceof SearchPath);

      assert.equal(cleanSplit1[0].inputCount, 1);
      assert.equal(cleanSplit1[1].inputCount, fiveCharCluster.inputCount - 1);

      assert.equal(cleanSplit1[0], fixture.paths[1].path_k1c2_i2d0);

      assert.isTrue(cleanSplit1[1] instanceof SearchCluster); // passes through both final paths

      // Note that the path structures themselves, however, will be rebuilt - and with new spaceIDs.
      const pathSequenceToInputs = (sequence: SearchPath[]) => sequence.map(p => p.inputs);
      // One of the paths leading into cluster_k3c3 passes through the split-point.
      assert.deepEqual(cleanSplit1[1].constituentPaths.length, 3);
      assert.isOk(cleanSplit1[1].constituentPaths.find((sequence) => {
        // One constituent path sequence should contain this path, which is part of cluster_k3c3.
        return sequence.find((path) => path.isSameSpace(fixture.paths[3].path_k3c3_i4d1b))
      }));

      assert.isOk(cleanSplit1[1].constituentPaths.find((sequence) => {
        // One constituent path sequence should contain this path, which is part of cluster_k3c4.
        return sequence.find((path) => path.isSameSpace(fixture.paths[3].path_k3c4_i4d0))
      }));

      assert.isOk(cleanSplit1[1].constituentPaths.find((sequence) => {
        // One constituent path sequence should contain this path, which is part of cluster_k3c4.
        return sequence.find((path) => path.isSameSpace(fixture.paths[3].path_k3c4_i5d1))
      }));

      // The input distributions should re-appear in the constituent paths, in full.
      assert.includeDeepMembers( // not deepEqual.
        // superset
        fiveCharCluster.constituentPaths.map((sequence) => {
          return [sequence[0], ...sequence.slice(2)];
        }).map(pathSequenceToInputs),
        // subset
        cleanSplit1[1].constituentPaths.map(pathSequenceToInputs)
      );

      // Splits cleanly after 2 keystrokes.
      const cleanSplit2 = splitResults.find((r) => r[0].inputCount == 2 && + r[1].inputCount == 2);
      assert.isTrue(cleanSplit2[0] instanceof SearchPath);

      assert.equal(cleanSplit2[0].inputCount, 2);
      assert.equal(cleanSplit2[1].inputCount, fiveCharCluster.inputCount - 2);

      assert.equal(cleanSplit2[0], fixture.paths[2].path_k2c2_i2d0);
      assert.isTrue(cleanSplit2[1] instanceof SearchPath);

      assert.deepEqual(cleanSplit2[1].inputSegments, [
        fixture.paths[3].path_k3c3_i3d0.inputSource.segment, fixture.paths[4].path_k4c5_i2.inputSource.segment
      ]);

      // splits on the double-accented version (that also does a deleteLeft)
      //
      // Splits after THREE keystrokes - first keystroke emitted one char, then
      // the third = the one that deletes left 1, then emits an accented vowel
      // pair.
      const dirtySplit = splitResults.find((r) => r[0].inputCount + r[1].inputCount == 5);
      assert.isTrue(dirtySplit[0] instanceof SearchPath);

      assert.equal(dirtySplit[0].inputCount, 3);
      assert.equal(dirtySplit[1].inputCount, fiveCharCluster.inputCount - 2);

      assert.deepEqual(dirtySplit[0].inputSegments[2], {
        ... fixture.paths[3].path_k3c3_i4d1a.inputSource.segment,
        start: 0,
        end: 1
      });

      assert.isTrue(dirtySplit[1] instanceof SearchPath);
      assert.deepEqual(dirtySplit[1].inputSegments[0], {
        ... fixture.paths[3].path_k3c3_i4d1a.inputSource.segment,
        start: 1
      });
      assert.deepEqual(dirtySplit[1].inputSegments[1], fixture.paths[4].path_k4c5_i2.inputSource.segment);
    });

    it('properly splits the cluster at index 3', () => {
      const fixture = buildAlphabeticClusterFixtures();
      const fiveCharCluster = fixture.clusters.cluster_k4c5;

      const splitResults = fiveCharCluster.split(3);
      // Two split arrays are "clean" (but differ by contributing keystroke
      // count), while the other splits a keystroke in half.
      assert.equal(splitResults.length, 3);
      assert.sameMembers(splitResults.map((r) => r[0].inputCount + r[1].inputCount), [4, 4, 5]);

      // Splits cleanly after 2 keystrokes.
      const cleanSplit1 = splitResults.find((r) => r[0].inputCount == 2 && + r[1].inputCount == 2);
      assert.isTrue(cleanSplit1[0] instanceof SearchPath);

      assert.equal(cleanSplit1[0].inputCount, 2);
      assert.equal(cleanSplit1[1].inputCount, fiveCharCluster.inputCount - 2);

      assert.equal(cleanSplit1[0], fixture.paths[2].path_k2c3_i3d0);
      assert.isTrue(cleanSplit1[1] instanceof SearchPath); // passes through both final paths

      // Note that the path structures themselves, however, will be rebuilt - and with new spaceIDs.
      const pathSequenceToInputs = (sequence: SearchPath[]) => sequence.map(p => p.inputs);
      // One of the paths leading into cluster_k3c3 passes through the split-point.
      assert.deepEqual(cleanSplit1[1].constituentPaths.length, 1);

      // The input distributions should re-appear in the constituent paths, in full.
      assert.includeDeepMembers( // not deepEqual.
        // superset
        fiveCharCluster.constituentPaths.map((sequence) => {
          return [sequence[0], ...sequence.slice(3)];
        }).map(pathSequenceToInputs),
        // subset
        cleanSplit1[1].constituentPaths.map(pathSequenceToInputs)
      );

      // Splits cleanly after 3 keystrokes.
      const cleanSplit2 = splitResults.find((r) => r[0].inputCount == 3 && + r[1].inputCount == 1);
      assert.isTrue(cleanSplit2[0] instanceof SearchCluster);

      assert.equal(cleanSplit2[0].inputCount, 3);
      assert.equal(cleanSplit2[1].inputCount, fiveCharCluster.inputCount - 3);

      assert.equal(cleanSplit2[0], fixture.clusters.cluster_k3c3);

      assert.isTrue(cleanSplit2[1] instanceof SearchPath);

      assert.deepEqual(cleanSplit2[1].inputSegments, [
        fixture.paths[4].path_k4c5_i2.inputSource.segment
      ]);

      // splits on the double-accented version (that also does a deleteLeft)
      //
      // Splits after THREE keystrokes - lead keystroke had two chars, emitted
      // one, then emitted the one that deletes left 1, then emits an accented
      // vowel pair.
      const dirtySplit = splitResults.find((r) => r[0].inputCount + r[1].inputCount == 5);
      assert.isTrue(dirtySplit[0] instanceof SearchPath);

      assert.equal(dirtySplit[0].inputCount, 3);
      assert.equal(dirtySplit[1].inputCount, fiveCharCluster.inputCount - 2);

      assert.deepEqual(dirtySplit[0].inputSegments[2], {
        ... fixture.paths[3].path_k3c4_i5d1.inputSource.segment,
        start: 0,
        end: 1
      });

      assert.isTrue(dirtySplit[1] instanceof SearchPath);
      assert.deepEqual(dirtySplit[1].inputSegments[0], {
        ... fixture.paths[3].path_k3c4_i5d1.inputSource.segment,
        start: 1
      });
      assert.deepEqual(dirtySplit[1].inputSegments[1], fixture.paths[4].path_k4c5_i1.inputSource.segment);
    });

    it('properly splits the cluster at index 4', () => {
      const fixture = buildAlphabeticClusterFixtures();
      const fiveCharCluster = fixture.clusters.cluster_k4c5;

      const splitResults = fiveCharCluster.split(4);
      // Two split arrays are "clean" (but differ by contributing keystroke
      // count), while the other splits a keystroke in half.
      assert.equal(splitResults.length, 2);
      assert.sameMembers(splitResults.map((r) => r[0].inputCount + r[1].inputCount), [4, 5]);

      // Splits cleanly after 3 keystrokes.
      const cleanSplit = splitResults.find((r) => r[0].inputCount == 3 && + r[1].inputCount == 1);
      assert.equal(cleanSplit[0], fixture.clusters.cluster_k3c4);

      assert.equal(cleanSplit[1].inputCount, fiveCharCluster.inputCount - 3);
      assert.isTrue(cleanSplit[1] instanceof SearchPath);

      const pathSequenceToInputs = (sequence: SearchPath[]) => sequence.map(p => p.inputs);
      assert.deepEqual(cleanSplit[1].constituentPaths.length, 1);

      // The input distributions should re-appear in the constituent paths, in full.
      assert.includeDeepMembers( // not deepEqual.
        // superset
        fiveCharCluster.constituentPaths.map((sequence) => {
          return [sequence[0], ...sequence.slice(4)];
        }).map(pathSequenceToInputs),
        // subset
        cleanSplit[1].constituentPaths.map(pathSequenceToInputs)
      );

      // splits on the fourth keystroke variant that outputs two chars.
      const dirtySplit = splitResults.find((r) => r[0].inputCount + r[1].inputCount == 5);
      assert.isTrue(dirtySplit[0] instanceof SearchPath);

      assert.equal(dirtySplit[0].inputCount, 4);
      assert.equal(dirtySplit[1].inputCount, 1);

      assert.deepEqual(dirtySplit[0].inputSegments[3], {
        ... fixture.paths[4].path_k4c5_i2.inputSource.segment,
        start: 0,
        end: 1
      });

      assert.isTrue(dirtySplit[1] instanceof SearchPath);
      assert.deepEqual(dirtySplit[1].inputSegments[0], {
        ... fixture.paths[4].path_k4c5_i2.inputSource.segment,
        start: 1
      });
    });

    it('properly splits the cluster at index 5', () => {
      const fixture = buildAlphabeticClusterFixtures();
      const fiveCharCluster = fixture.clusters.cluster_k4c5;

      const splitResults = fiveCharCluster.split(5);
      // Two split arrays are "clean" (but differ by contributing keystroke
      // count), while the other splits a keystroke in half.
      assert.equal(splitResults.length, 1);

      assert.equal(splitResults[0][0], fiveCharCluster);
      assert.equal(splitResults[0][1].inputCount, 0);
    });
  });

  // Self-note:  reuse the 'alphabetic' fixture and manually make mocked paths
  // that match split results; then do merges to match 'alphabetic' results.
  describe('merge()', () => {
    it('merges in a simple SearchPath with a single, unsplit input', () => {
      const fixture = buildAlphabeticClusterFixtures();

      // From the test fixture:  we have two paths that were constructed as followups to cluster_k3c3.
      const baseCluster = fixture.clusters.cluster_k3c3;

      const mergeResult = baseCluster.merge(new SearchPath(
        // Is (and mocks) the head result from `path_k4c4_i1.split(3)`.
        fixture.paths[0].rootPath,
        // Mocks the tail result from `path_k4c4_i1.split(3)`.
        fixture.distributions[4].distrib_c2_i1d0, {
          segment: {
            transitionId: fixture.distributions[4].distrib_c2_i1d0[0].sample.id,
            start: 0
          },
          bestProbFromSet: fixture.distributions[4].distrib_c2_i1d0[0].p,
          subsetId: fixture.paths[4].path_k4c4_i1.inputSource.subsetId
        })
      );

      assert.isTrue(mergeResult.isSameSpace(fixture.paths[4].path_k4c4_i1));
    });

    it('merges in a SearchCluster with unsplit initial inputs', () => {
      const fixture = buildAlphabeticClusterFixtures();

      // This is the cluster we wish to merge new content into.
      const cluster_k3c3 = fixture.clusters.cluster_k3c3;

      // Now, we manually construct the right-hand side that we wish to merge in.
      const rightRoot = new SearchPath(testModel);

      const pathA1 = new SearchPath(rightRoot, fixture.distributions[4].distrib_c2_i1d0, fixture.paths[4].path_k4c4_i1.inputSource);
      const pathB1 = new SearchPath(rightRoot, fixture.distributions[4].distrib_c2_i2d0, fixture.paths[4].path_k4c5_i2.inputSource);

      const pathA2 = new SearchPath(pathA1, fixture.distributions[5].distrib_c3_i2d0, fixture.paths[5].path_k5c6_a.inputSource);
      const pathB2 = new SearchPath(pathB1, fixture.distributions[5].distrib_c3_i1d0, fixture.paths[5].path_k5c6_b.inputSource);

      const rightHandCluster = new SearchCluster([pathA2, pathB2]);

      const merged = cluster_k3c3.merge(rightHandCluster);

      assert.isTrue(merged.isSameSpace(fixture.clusters.cluster_k5c6));
    });

    it('merges in a SearchPath with multiple inputs & an ancestor SearchCluster', () => {
      const fixture = buildAlphabeticClusterFixtures();

      // This is the cluster we wish to merge new content into.
      const cluster_k3c3 = fixture.clusters.cluster_k3c3;

      // Now, we manually construct the right-hand side that we wish to merge in.
      const rightRoot = new SearchPath(testModel);

      const pathA1 = new SearchPath(rightRoot, fixture.distributions[4].distrib_c2_i1d0, fixture.paths[4].path_k4c4_i1.inputSource);
      const pathB1 = new SearchPath(rightRoot, fixture.distributions[4].distrib_c2_i2d0, fixture.paths[4].path_k4c5_i2.inputSource);

      const pathA2 = new SearchPath(pathA1, fixture.distributions[5].distrib_c3_i2d0, fixture.paths[5].path_k5c6_a.inputSource);
      const pathB2 = new SearchPath(pathB1, fixture.distributions[5].distrib_c3_i1d0, fixture.paths[5].path_k5c6_b.inputSource);

      const rightHandCluster = new SearchCluster([pathA2, pathB2]); // endpoint for prior test.

      // Append an extra path step.
      const final_dist: Distribution<Transform> = [
        { sample: { insert: '0', deleteLeft: 0, deleteRight: 0, id: 16 }, p: 0.6 }, // most likely for id 16
        { sample: { insert: '1', deleteLeft: 0, deleteRight: 0, id: 16 }, p: 0.4 }
      ];

      const targetFinalPath = new SearchPath(fixture.clusters.cluster_k5c6, final_dist, final_dist[0]);
      const rightHandFinalPath = new SearchPath(rightHandCluster, final_dist, targetFinalPath.inputSource);

      const merged = cluster_k3c3.merge(rightHandFinalPath);
      assert.isTrue(merged.isSameSpace(targetFinalPath));
    });

    it('remerges the components of a previous .split() result that split input keystrokes', () => {
      // First:  creating a SearchSpace matching the desired merge results for this test.
      const rootPath = new SearchPath(testModel);

      // consonant-cluster 1, insert 1, delete 0
      const distrib_k1_i2: Distribution<Transform> = [
        { sample: { insert: 'ab', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.3 }, // most likely for id 11
        { sample: { insert: 'cd', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.2 },
        { sample: { insert: 'ef', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.1 },
      ];

      // consonant-cluster 1, insert 2, delete 0
      const distrib_k1_i3: Distribution<Transform> = [
        { sample: { insert: 'ghi', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.16 },
        { sample: { insert: 'jkl', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.14 },
        { sample: { insert: 'mno', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.1 },
      ];

      const path_k1c2_i2 = new SearchPath(rootPath, distrib_k1_i2, distrib_k1_i2[0]);
      const path_k1c3_i3 = new SearchPath(rootPath, distrib_k1_i3, distrib_k1_i2[0]);

      const distrib_k2_i1: Distribution<Transform> = [
        { sample: { insert: 'p', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.25 }, // most likely for id 12
        { sample: { insert: 'q', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.15 },
        { sample: { insert: 'r', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.1 },
      ];

      const distrib_k2_i2: Distribution<Transform> = [
        { sample: { insert: 'st', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.14 },
        { sample: { insert: 'uv', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.13 },
        { sample: { insert: 'wx', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.12 },
        { sample: { insert: 'yz', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.11 },
      ];

      const path_k2c4_1 = new SearchPath(path_k1c2_i2, distrib_k2_i2, distrib_k2_i1[0]);
      const path_k2c4_2 = new SearchPath(path_k1c3_i3, distrib_k2_i1, distrib_k2_i1[0]);

      const targetCluster = new SearchCluster([path_k2c4_1, path_k2c4_2]);

      const splitDistribution = (dist: Distribution<Transform>) => {
        const splitEntries = dist.map((entry) => ([{
          p: entry.p,
          sample: {
            ...entry.sample,
            insert: KMWString.substring(entry.sample.insert, 0, 1)
          }
        }, {
          p: entry.p,
          sample: {
            ...entry.sample,
            insert: KMWString.substring(entry.sample.insert, 1)
          }
        }]));

        return { head: splitEntries.map(t => t[0]), tail: splitEntries.map(t => t[1])};
      }

      // Now:  constructing the left-hand components.
      const path_k1c2_i2_head = new SearchPath(rootPath,
        splitDistribution(distrib_k1_i2).head, {
          ...path_k1c2_i2.inputSource,
          segment: {
            ...path_k1c2_i2.inputSource.segment,
            end: 1
          }
        }
      );
      const path_k1c3_i3_head = new SearchPath(rootPath,
        splitDistribution(distrib_k1_i3).head, {
          ...path_k1c3_i3.inputSource,
          segment: {
            ...path_k1c3_i3.inputSource.segment,
            end: 1
          }
        }
      );

      const splitHeadCluster = new SearchCluster([path_k1c2_i2_head, path_k1c3_i3_head]);

      // And now the right-hand side of the split.

      const tailRoot = new SearchPath(testModel);

      // Now:  constructing the left-hand components.
      const path_k1c2_i2_tail = new SearchPath(tailRoot,
        splitDistribution(distrib_k1_i2).tail, {
          ...path_k1c2_i2.inputSource,
          segment: {
            ...path_k1c2_i2.inputSource.segment,
            start: 1
          }
        }
      );
      const path_k1c3_i3_tail = new SearchPath(tailRoot,
        splitDistribution(distrib_k1_i3).tail, {
          ...path_k1c3_i3.inputSource,
          segment: {
            ...path_k1c3_i3.inputSource.segment,
            start: 1
          }
        }
      );

      const path_k2c4_1_tail = new SearchPath(path_k1c2_i2_tail, distrib_k2_i2, {
        ...path_k2c4_1.inputSource
      });
      const path_k2c4_2_tail = new SearchPath(path_k1c3_i3_tail, distrib_k2_i1, {
        ...path_k2c4_1.inputSource
      });

      const splitTailCluster = new SearchCluster([path_k2c4_1_tail, path_k2c4_2_tail]);
      const remergedCluster = splitHeadCluster.merge(splitTailCluster);

      assert.isTrue(remergedCluster.isSameSpace(targetCluster));
      assert.equal(remergedCluster.inputCount, targetCluster.inputCount);
      assert.deepEqual(remergedCluster.inputSegments, targetCluster.inputSegments);
      assert.isTrue(remergedCluster.hasInputs([
        distrib_k1_i3, distrib_k2_i1
      ]));
      assert.isTrue(remergedCluster.hasInputs([
        distrib_k1_i2, distrib_k2_i2
      ]));
    });
  });
});