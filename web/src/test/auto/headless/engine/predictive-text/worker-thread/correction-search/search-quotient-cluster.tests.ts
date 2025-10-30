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
import { LegacyQuotientRoot, LegacyQuotientSpur, models, SearchQuotientCluster, unitTestEndpoints } from '@keymanapp/lm-worker/test-index';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

const constituentPaths = unitTestEndpoints.constituentPaths;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

export const buildAlphabeticClusterFixtures = () => {
  const rootPath = new LegacyQuotientRoot(testModel);

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
  const path_k1c1_i1d0 = new LegacyQuotientSpur(rootPath, distrib_c1_i1d0, distrib_c1_i1d0[0]);
  // keystrokes 1, codepoints 2, total inserts 2, delete 0
  const path_k1c2_i2d0 = new LegacyQuotientSpur(rootPath, distrib_c1_i2d0, distrib_c1_i1d0[0]);

  // Second input

  const distrib_v1_i1d0: Distribution<Transform> = [
    { sample: { insert: 'e', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.4 }, // most likely for id 12
    { sample: { insert: 'a', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.3 },
    { sample: { insert: 'i', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.1 },
    { sample: { insert: 'o', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.1 },
    { sample: { insert: 'u', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.1 },
  ];

  const path_k2c2_i2d0 = new LegacyQuotientSpur(path_k1c1_i1d0, distrib_v1_i1d0, distrib_v1_i1d0[0]);
  const path_k2c3_i3d0 = new LegacyQuotientSpur(path_k1c2_i2d0, distrib_v1_i1d0, distrib_v1_i1d0[0]);

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

  const path_k3c2_i3d1 = new LegacyQuotientSpur(path_k2c2_i2d0, distrib_v2_i1d1, distrib_v2_i1d0[0]);

  const path_k3c3_i3d0  = new LegacyQuotientSpur(path_k2c2_i2d0, distrib_v2_i1d0, distrib_v2_i1d0[0]);
  const path_k3c3_i4d1a = new LegacyQuotientSpur(path_k2c2_i2d0, distrib_v2_i2d1, distrib_v2_i1d0[0]);
  const path_k3c3_i4d1b = new LegacyQuotientSpur(path_k2c3_i3d0, distrib_v2_i1d1, distrib_v2_i1d0[0]);

  const path_k3c4_i4d0 = new LegacyQuotientSpur(path_k2c3_i3d0, distrib_v2_i1d0, distrib_v2_i1d0[0]);
  const path_k3c4_i5d1 = new LegacyQuotientSpur(path_k2c3_i3d0, distrib_v2_i2d1, distrib_v2_i1d0[0]);

  const cluster_k3c3 = new SearchQuotientCluster([path_k3c3_i3d0, path_k3c3_i4d1a, path_k3c3_i4d1b]);
  const cluster_k3c4 = new SearchQuotientCluster([path_k3c4_i4d0, path_k3c4_i5d1]);

  // Input 4
  const distrib_c2_i1d0: Distribution<Transform> = [ // most likely for id 11
    { sample: { insert: 'n', deleteLeft: 0, deleteRight: 0, id: 14 }, p: 0.12 },
    { sample: { insert: 'p', deleteLeft: 0, deleteRight: 0, id: 14 }, p: 0.08 },
  ];

  const distrib_c2_i2d0: Distribution<Transform> = [
    { sample: { insert: 'qr', deleteLeft: 0, deleteRight: 0, id: 14 }, p: 0.3 }, // most likely for id 14
    { sample: { insert: 'st', deleteLeft: 0, deleteRight: 0, id: 14 }, p: 0.2 },
    { sample: { insert: 'vw', deleteLeft: 0, deleteRight: 0, id: 14 }, p: 0.1 }
  ];

  const path_k4c4_i2 = new LegacyQuotientSpur(path_k3c2_i3d1, distrib_c2_i2d0, distrib_c2_i2d0[0]);
  const path_k4c4_i1 = new LegacyQuotientSpur(cluster_k3c3, distrib_c2_i1d0, distrib_c2_i2d0[0]);

  const path_k4c5_i2 = new LegacyQuotientSpur(cluster_k3c3, distrib_c2_i2d0, distrib_c2_i2d0[0]);
  const path_k4c5_i1 = new LegacyQuotientSpur(cluster_k3c4, distrib_c2_i1d0, distrib_c2_i2d0[0]);

  const path_k4c6 = new LegacyQuotientSpur(cluster_k3c4, distrib_c2_i2d0, distrib_c2_i2d0[0]);

  const cluster_k4c4 = new SearchQuotientCluster([path_k4c4_i2, path_k4c4_i1]);
  const cluster_k4c5 = new SearchQuotientCluster([path_k4c5_i2, path_k4c5_i1]);

  return {
    distributions: {
      0: {
        distrib_c1_i1d0,
        distrib_c1_i2d0
      },
      1: {
        distrib_v1_i1d0
      },
      2: {
        distrib_v2_i1d0,
        distrib_v2_i1d1,
        distrib_v2_i2d1
      },
      3: {
        distrib_c2_i1d0,
        distrib_c2_i2d0
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
      }
    },
    clusters: {
      cluster_k3c3,
      cluster_k3c4,
      cluster_k4c4,
      cluster_k4c5
    }
  }
}

describe('SearchCluster', () => {
  describe('constructor()', () => {
    it('initializes from root SearchPath', () => {
      const path = new LegacyQuotientRoot(testModel);
      const cluster = new SearchQuotientCluster([path]);
      assert.equal(cluster.inputCount, 0);
      assert.equal(cluster.codepointLength, 0);
      assert.isNumber(cluster.spaceId);
      assert.deepEqual(cluster.bestExample, {text: '', p: 1});
      assert.deepEqual(cluster.parents, [path]);
    });

    it('initializes from arbitrary SearchPath', () => {
      const rootPath = new LegacyQuotientRoot(testModel);

      const leadEdgeDistribution: Distribution<Transform> = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

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

      const cluster = new SearchQuotientCluster([length2Path]);

      assert.equal(cluster.inputCount, 2);
      assert.equal(cluster.codepointLength, 2);
      assert.isNumber(cluster.spaceId);
      assert.notEqual(cluster.spaceId, length1Path.spaceId);
      assert.deepEqual(cluster.bestExample, {text: 'tr', p: leadEdgeDistribution[0].p * tailEdgeDistribution[0].p});
      assert.deepEqual(cluster.parents, [length2Path]);
      assert.deepEqual(cluster.inputSegments, [
        {
          start: 0,
          transitionId: leadEdgeDistribution[0].sample.id
        }, {
          start: 0,
          transitionId: tailEdgeDistribution[0].sample.id
        }
      ]);
    });

    it('throws an error when SearchPath array is empty', () => {
      assert.throws(() => new SearchQuotientCluster([]));
    });

    it('throws an error if SearchPath .inputCount values don\'t match', () => {
      const rootPath = new LegacyQuotientRoot(testModel);

      const leadEdgeDistribution: Distribution<Transform> = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

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

      const altDistribution = [
        {sample: {insert: 'tr', deleteLeft: 0, id: 13 }, p: 0.6},
        {sample: {insert: 'te', deleteLeft: 0, id: 13 }, p: 0.25},
        {sample: {insert: 'th', deleteLeft: 0, id: 13 }, p: 0.15}
      ];
      const singleInputPath = new LegacyQuotientSpur(rootPath, altDistribution, altDistribution[0]);

      assert.throws(() => new SearchQuotientCluster([length2Path, singleInputPath]));
    });

    it('throws an error if SearchPath .sourceIdentifier values don\'t match', () => {
      const rootPath = new LegacyQuotientRoot(testModel);

      const distribution1: Distribution<Transform> = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const path1 = new LegacyQuotientSpur(
        rootPath,
        distribution1,
        distribution1[0]
      );

      const distribution2 = [
        {sample: {insert: 'r', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'e', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'h', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const path2 = new LegacyQuotientSpur(
        rootPath,
        distribution2,
        distribution2[0]
      );

      assert.throws(() => new SearchQuotientCluster([path1, path2]));
    });

    it('throws an error if SearchPath .codepointLength values don\'t match', () => {
      const rootPath = new LegacyQuotientRoot(testModel);

      const dist1: Distribution<Transform> = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const path1 = new LegacyQuotientSpur(
        rootPath,
        dist1,
        dist1[0]
      );

      const dist2 = [
        {sample: {insert: 'tr', deleteLeft: 0, id: 13 }, p: 0.6},
        {sample: {insert: 'te', deleteLeft: 0, id: 13 }, p: 0.25},
        {sample: {insert: 'th', deleteLeft: 0, id: 13 }, p: 0.15}
      ];
      const path2 = new LegacyQuotientSpur(rootPath, dist2, dist2[0]);

      assert.throws(() => new SearchQuotientCluster([path1, path2]));
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
      assert.equal(constituentPaths(threeCharCluster).length, 3);
      // Root path counts for this.
      constituentPaths(threeCharCluster).forEach(sequence => assert.equal(sequence.length, 3));

      assert.includeDeepMembers(constituentPaths(threeCharCluster), [
        [
          paths[1].path_k1c1_i1d0,
          paths[2].path_k2c2_i2d0,
          paths[3].path_k3c3_i3d0
        ], [
          paths[1].path_k1c1_i1d0,
          paths[2].path_k2c2_i2d0,
          paths[3].path_k3c3_i4d1a
        ], [
          paths[1].path_k1c2_i2d0,
          paths[2].path_k2c3_i3d0,
          paths[3].path_k3c3_i4d1b
        ]
      ]);

      const fourCharCluster = clusters.cluster_k3c4;
      assert.equal(constituentPaths(fourCharCluster).length, 2);
      // Root path counts for this.
      constituentPaths(fourCharCluster).forEach(sequence => assert.equal(sequence.length, 3));

      assert.includeDeepMembers(constituentPaths(fourCharCluster), [
        [
          paths[1].path_k1c2_i2d0,
          paths[2].path_k2c3_i3d0,
          paths[3].path_k3c4_i4d0
        ], [
          paths[1].path_k1c2_i2d0,
          paths[2].path_k2c3_i3d0,
          paths[3].path_k3c4_i5d1
        ]
      ]);
    });

    it('enumerates clusters built from a mix of parent paths and clusters', () => {
      const { paths, clusters } = buildAlphabeticClusterFixtures();

      const fourCharCluster = clusters.cluster_k4c4;
      assert.equal(constituentPaths(fourCharCluster).length, 4);
      // Root path counts for this.
      constituentPaths(fourCharCluster).forEach(sequence => assert.equal(sequence.length, 4));

      assert.notIncludeDeepOrderedMembers(constituentPaths(fourCharCluster), [
        [
          paths[1].path_k1c1_i1d0,
          paths[2].path_k2c2_i2d0,
          paths[3].path_k3c3_i3d0,
          paths[4].path_k4c4_i2 // last component writes the third char
        ]
      ]);

      assert.includeDeepMembers(constituentPaths(fourCharCluster),
        // Should have all paths enumerable from cluster_k3c3 as a prefix.
        constituentPaths(clusters.cluster_k3c3).map((seq) => {
          seq.push(paths[4].path_k4c4_i1);
          return seq;
        })
      );

      assert.includeDeepMembers(constituentPaths(fourCharCluster), [
        [
          paths[1].path_k1c1_i1d0,
          paths[2].path_k2c2_i2d0,
          paths[3].path_k3c2_i3d1,
          paths[4].path_k4c4_i2
        ]
      ]);

      const fiveCharCluster = clusters.cluster_k4c5;
      assert.equal(constituentPaths(fiveCharCluster).length, 5);
      // Root path counts for this.
      constituentPaths(fiveCharCluster).forEach(sequence => assert.equal(sequence.length, 4));

      assert.includeDeepMembers(constituentPaths(fiveCharCluster),
        // Should have all paths enumerable from cluster_k3c3 as a prefix.
        constituentPaths(clusters.cluster_k3c3).map((seq) => {
          seq.push(paths[4].path_k4c5_i2);
          return seq;
        })
      );

      assert.includeDeepMembers(constituentPaths(fiveCharCluster),
        // Should have all paths enumerable from cluster_k3c4 as a prefix.
        constituentPaths(clusters.cluster_k3c4).map((seq) => {
          seq.push(paths[4].path_k4c5_i1);
          return seq;
        })
      );
    });
  });
});