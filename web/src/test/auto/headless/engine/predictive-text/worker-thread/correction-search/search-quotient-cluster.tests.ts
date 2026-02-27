/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-29
 *
 * This file defines tests for the SearchQuotientCluster class of the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { KMWString } from '@keymanapp/web-utils';
import { generateSubsetId, LegacyQuotientRoot, LegacyQuotientSpur, models, SearchQuotientCluster, SearchQuotientNode, SearchQuotientRoot, SearchQuotientSpur } from '@keymanapp/lm-worker/test-index';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

import { constituentPaths, quotientPathHasInputs } from '#test-resources/searchQuotientUtils.js';

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

  // both are built on path k1c2 (splits at index 1)
  const path_k3c4_i4d0 = new LegacyQuotientSpur(path_k2c3_i3d0, distrib_v2_i1d0, distrib_v2_i1d0[0]);
  const path_k3c4_i5d1 = new LegacyQuotientSpur(path_k2c3_i3d0, distrib_v2_i2d1, distrib_v2_i1d0[0]);

  const cluster_k3c3 = new SearchQuotientCluster([path_k3c3_i3d0, path_k3c3_i4d1a, path_k3c3_i4d1b]);
  // both are built on path k1c2.
  const cluster_k3c4 = new SearchQuotientCluster([path_k3c4_i4d0, path_k3c4_i5d1]);

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

  const path_k4c4_i2 = new LegacyQuotientSpur(path_k3c2_i3d1, distrib_c2_i2d0, distrib_c2_i2d0[0]);
  const path_k4c4_i1 = new LegacyQuotientSpur(cluster_k3c3, distrib_c2_i1d0, distrib_c2_i2d0[0]);

  const path_k4c5_i2 = new LegacyQuotientSpur(cluster_k3c3, distrib_c2_i2d0, distrib_c2_i2d0[0]);
  const path_k4c5_i1 = new LegacyQuotientSpur(cluster_k3c4, distrib_c2_i1d0, distrib_c2_i2d0[0]);

  const path_k4c6 = new LegacyQuotientSpur(cluster_k3c4, distrib_c2_i2d0, distrib_c2_i2d0[0]);

  const cluster_k4c4 = new SearchQuotientCluster([path_k4c4_i2, path_k4c4_i1]);
  const cluster_k4c5 = new SearchQuotientCluster([path_k4c5_i2, path_k4c5_i1]);

  // Input 5 (currently used only for merge tests)
  const distrib_c3_i2d0: Distribution<Transform> = [
    { sample: { insert: 'xy', deleteLeft: 0, deleteRight: 0, id: 15 }, p: 0.6 } // most likely for id 15
  ];
  const distrib_c3_i1d0: Distribution<Transform> = [
    { sample: { insert: 'z', deleteLeft: 0, deleteRight: 0, id: 15 }, p: 0.4 }
  ];

  const path_k5c6_a = new LegacyQuotientSpur(cluster_k4c4, distrib_c3_i2d0, distrib_c3_i2d0[0]);
  const path_k5c6_b = new LegacyQuotientSpur(cluster_k4c5, distrib_c3_i1d0, distrib_c3_i2d0[0]);

  const cluster_k5c6 = new SearchQuotientCluster([path_k5c6_a, path_k5c6_b]);

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

const splitDistribution = (dist: Distribution<Transform>, index: number) => {
  const splitEntries = dist.map((entry) => ([{
    p: entry.p,
    sample: {
      ...entry.sample,
      insert: KMWString.substring(entry.sample.insert, 0, index),
      deleteRight: 0
    }
  }, {
    p: entry.p,
    sample: {
      ...entry.sample,
      insert: KMWString.substring(entry.sample.insert, index),
      deleteLeft: 0
    }
  }]));

  return { head: splitEntries.map(t => t[0]), tail: splitEntries.map(t => t[1])};
}

const determineTargetSplitSequences = (constituentPaths: SearchQuotientSpur[][], splitLocation: number) => {
  const cleanPathSequences: [SearchQuotientSpur[], SearchQuotientSpur[]][] = [];
  const dirtyPathSequences: [SearchQuotientSpur[], SearchQuotientSpur[]][] = [];
  constituentPaths.forEach(seq => {
    let splitIndex = seq.length - 1;
    for(; splitIndex >= 0; splitIndex--) {
      const spur = seq[splitIndex];
      const codeLen = spur.codepointLength;

      if(codeLen == splitLocation) {
        cleanPathSequences.push([seq.slice(0, splitIndex+1),seq.slice(splitIndex+1)]);
        break;
      }

      const spurStartIndex = spur.codepointLength - spur.insertLength + spur.leftDeleteLength;

      if(spurStartIndex > splitLocation) {
        continue;
      } else if(spurStartIndex == splitLocation && spur.leftDeleteLength == 0) {
        continue;
      }

      /* else split occurs within currently-examined entry */
      const cleanHead = seq.slice(0, splitIndex);
      const cleanTail = seq.slice(splitIndex+1);

      const splitSegment = seq[splitIndex];
      const segmentSplitIndex = splitLocation - (splitSegment.codepointLength - splitSegment.insertLength);
      const splitDistrib = splitDistribution(splitSegment.inputs, segmentSplitIndex);
      const dirtyStart = new LegacyQuotientSpur(
        seq[splitIndex-1] ?? new LegacyQuotientRoot(splitSegment.model),
        splitDistrib.head, {
          ...splitSegment.inputSource,
          segment: {
            ...splitSegment.inputSource.segment,
            end: segmentSplitIndex
          }
        }
      );
      const dirtyTail: SearchQuotientSpur = new LegacyQuotientSpur(
        dirtyStart,
        splitDistrib.tail, {
          ...splitSegment.inputSource,
          segment: {
            ...splitSegment.inputSource.segment,
            start: segmentSplitIndex
          }
        }
      );

      dirtyPathSequences.push([cleanHead.concat([dirtyStart]), [dirtyTail].concat(cleanTail)]);

      break;
    }

    if(splitIndex == -1) {
      cleanPathSequences.push([[], seq]);
    }
  });

  return { cleanPathSequences, dirtyPathSequences };
};

describe('SearchQuotientCluster', () => {
  describe('constructor()', () => {
    it('initializes from LegacySearchRoot', () => {
      const path = new LegacyQuotientRoot(testModel);
      const cluster = new SearchQuotientCluster([path]);
      assert.equal(cluster.inputCount, 0);
      assert.equal(cluster.codepointLength, 0);
      assert.isNumber(cluster.spaceId);
      assert.deepEqual(cluster.bestExample, {text: '', p: 1});
      assert.deepEqual(cluster.parents, [path]);
    });

    it('initializes from arbitrary SearchQuotientSpur', () => {
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

    it('throws an error when constructor array parameter is empty', () => {
      assert.throws(() => new SearchQuotientCluster([]));
    });

    it('throws an error if parent .inputCount values don\'t match', () => {
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

  it('constructs a SearchQuotientSpur + SearchQuotientCluster fixture properly', () => {
    // Finishing construction of the fixture without errors is itself an
    // implicit test.
    buildAlphabeticClusterFixtures();
  });

  // As it's used to validate other SearchQuotientCluster unit tests, it's wise to test
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

      assert.isTrue(splitResult[0] instanceof SearchQuotientRoot);
      assert.deepEqual(constituentPaths(splitResult[0]), []);

      // Also add assertions for the right-hand side.
      assert.isTrue(splitResult[1] instanceof SearchQuotientCluster);
      // Note that the path structures themselves, however, will be rebuilt - and with new spaceIDs.
      const pathSequenceToInputs = (sequence: SearchQuotientSpur[]) => sequence.map(p => p.inputs);
      assert.deepEqual(constituentPaths(splitResult[1]).length, constituentPaths(fiveCharCluster).length);
      // The input distributions should re-appear in the constituent paths, in full.
      assert.sameDeepMembers(
        constituentPaths(splitResult[1]).map(pathSequenceToInputs),
        constituentPaths(fiveCharCluster).map(pathSequenceToInputs)
      );

      assert.equal(constituentPaths(splitResult[1]).length, constituentPaths(fiveCharCluster).length);
    });

    it('properly splits the cluster at index 1', () => {
      const fixture = buildAlphabeticClusterFixtures();
      const fiveCharCluster = fixture.clusters.cluster_k4c5;

      const splitPoint = 1;
      const splitResults = fiveCharCluster.split(splitPoint);

      const { cleanPathSequences, dirtyPathSequences } = determineTargetSplitSequences(constituentPaths(fiveCharCluster), splitPoint);

      // One split array is "clean", while the other splits a keystroke in half.
      assert.equal(splitResults.length, 2);
      assert.sameMembers(splitResults.map((r) => r[0].inputCount + r[1].inputCount), [4, 5]);

      const cleanSplit = splitResults.find((r) => r[0].inputCount + r[1].inputCount == 4);

      assert.equal(cleanSplit[0].inputCount, 1);
      assert.equal(cleanSplit[1].inputCount, fiveCharCluster.inputCount - 1);

      assert.equal(cleanSplit[0], fixture.paths[1].path_k1c1_i1d0);

      // There is only one valid path with the clean split.  There is a cluster
      // in its history, but it reconverges before the end.
      assert.isTrue(cleanSplit[1] instanceof SearchQuotientSpur);
      // Note that the path structures themselves, however, will be rebuilt -
      // and with new spaceIDs.
      const pathSequenceToInputs = (sequence: SearchQuotientSpur[]) => sequence.map(p => p.inputs);
      // One of the paths leading into cluster_k3c3 passes through the
      // split-point.
      assert.deepEqual(constituentPaths(cleanSplit[1]).length, constituentPaths(fixture.clusters.cluster_k3c3).length - 1);
      // The input distributions should re-appear in the constituent paths, in
      // full.
      assert.includeDeepMembers( // not deepEqual.
        // superset
        constituentPaths(fiveCharCluster).map((sequence) => {
          return sequence.slice(1);
        }).map(pathSequenceToInputs),
        // subset
        constituentPaths(cleanSplit[1]).map(pathSequenceToInputs)
      );

      assert.includeDeepMembers(
        cleanPathSequences.map(t => t[0]).map(s => s.map(n => n.edgeKey)),
        constituentPaths(cleanSplit[0]).map(s => s.map(n => n.edgeKey))
      );
      assert.includeDeepMembers(
        cleanPathSequences.map(t => t[1]).map(s => s.map(n => n.edgeKey)),
        constituentPaths(cleanSplit[1]).map(s => s.map(n => n.edgeKey))
      );

      const dirtySplit = splitResults.find((r) => r[0].inputCount + r[1].inputCount == 5);

      assert.equal(dirtySplit[0].inputCount, 1);
      assert.equal(dirtySplit[1].inputCount, fiveCharCluster.inputCount /* +1 - 1 */);

      assert.isTrue(dirtySplit[0] instanceof SearchQuotientSpur);
      assert.deepEqual(dirtySplit[0].inputSegments[0], {
        ... fixture.paths[1].path_k1c2_i2d0.inputSource.segment,
        start: 0,
        end: 1
      });

      assert.isTrue(dirtySplit[1] instanceof SearchQuotientCluster);
      assert.equal(constituentPaths(dirtySplit[1]).length, constituentPaths(fixture.paths[4].path_k4c5_i1).length + 1)
      assert.deepEqual(dirtySplit[1].inputSegments[0], {
        ... fixture.paths[1].path_k1c2_i2d0.inputSource.segment,
        start: 1,
      });

      // The unsplit input distributions should re-appear in the constituent
      // paths, in full.
      assert.includeDeepMembers( // not deepEqual.
        // superset
        constituentPaths(fiveCharCluster).map((sequence) => {
          return sequence.slice(1);
        }).map(pathSequenceToInputs),
        // subset
        constituentPaths(cleanSplit[1]).slice(1).map(pathSequenceToInputs)
      );

      assert.includeDeepMembers(
        dirtyPathSequences.map(t => t[0]).map(s => s.map(n => n.edgeKey)),
        constituentPaths(dirtySplit[0]).map(s => s.map(n => n.edgeKey))
      );
      assert.includeDeepMembers(
        dirtyPathSequences.map(t => t[1]).map(s => s.map(n => n.edgeKey)),
        constituentPaths(dirtySplit[1]).map(s => s.map(n => n.edgeKey))
      );
    });

    it('properly splits the cluster at index 2', () => {
      const fixture = buildAlphabeticClusterFixtures();
      const fiveCharCluster = fixture.clusters.cluster_k4c5;

      const splitPoint = 2;
      const splitResults = fiveCharCluster.split(splitPoint);
      // Two split arrays are "clean" (but differ by contributing keystroke
      // count), while the other splits a keystroke in half.
      assert.equal(splitResults.length, 3);

      const { cleanPathSequences, dirtyPathSequences } = determineTargetSplitSequences(constituentPaths(fiveCharCluster), splitPoint);

      const cleanPath1Sequences = cleanPathSequences.filter(seqTuple => seqTuple[0].length == 1);
      const cleanPath2Sequences = cleanPathSequences.filter(seqTuple => seqTuple[0].length == 2);

      assert.sameMembers(splitResults.map((r) => r[0].inputCount + r[1].inputCount), [4, 4, 5]);

      // Splits cleanly after 1 keystroke.
      const cleanSplit1 = splitResults.find((r) => r[0].inputCount == 1 && + r[1].inputCount == 3);
      assert.isTrue(cleanSplit1[0] instanceof SearchQuotientSpur);

      assert.equal(cleanSplit1[0].inputCount, 1);
      assert.equal(cleanSplit1[1].inputCount, fiveCharCluster.inputCount - 1);

      assert.equal(cleanSplit1[0], fixture.paths[1].path_k1c2_i2d0);

      assert.isTrue(cleanSplit1[1] instanceof SearchQuotientCluster); // passes through both final paths

      // Note that the path structures themselves, however, will be rebuilt - and with new spaceIDs.
      const pathSequenceToInputs = (sequence: SearchQuotientSpur[]) => sequence.map(p => p.inputs);
      // One of the paths leading into cluster_k3c3 passes through the split-point.
      assert.deepEqual(constituentPaths(cleanSplit1[1]).length, 3);

      assert.includeDeepMembers(
        cleanPath1Sequences.map(t => t[0]).map(s => s.map(n => n.edgeKey)),
        constituentPaths(cleanSplit1[0]).map(s => s.map(n => n.edgeKey))
      );
      assert.includeDeepMembers(
        cleanPath1Sequences.map(t => t[1]).map(s => s.map(n => n.edgeKey)),
        constituentPaths(cleanSplit1[1]).map(s => s.map(n => n.edgeKey))
      );

      // The input distributions should re-appear in the constituent paths, in full.
      assert.includeDeepMembers( // not deepEqual.
        // superset
        constituentPaths(fiveCharCluster).map((sequence) => {
          return sequence.slice(1);
        }).map(pathSequenceToInputs),
        // subset
        constituentPaths(cleanSplit1[1]).map(pathSequenceToInputs)
      );

      // Splits cleanly after 2 keystrokes.
      const cleanSplit2 = splitResults.find((r) => r[0].inputCount == 2 && + r[1].inputCount == 2);
      assert.isTrue(cleanSplit2[0] instanceof SearchQuotientSpur);

      assert.equal(cleanSplit2[0].inputCount, 2);
      assert.equal(cleanSplit2[1].inputCount, fiveCharCluster.inputCount - 2);

      assert.equal(cleanSplit2[0], fixture.paths[2].path_k2c2_i2d0);
      assert.isTrue(cleanSplit2[1] instanceof SearchQuotientSpur);

      assert.deepEqual(cleanSplit2[1].inputSegments, [
        fixture.paths[3].path_k3c3_i3d0.inputSource.segment, fixture.paths[4].path_k4c5_i2.inputSource.segment
      ]);

        assert.includeDeepMembers(
        cleanPath2Sequences.map(t => t[0]).map(s => s.map(n => n.edgeKey)),
        constituentPaths(cleanSplit2[0]).map(s => s.map(n => n.edgeKey))
      );
      assert.includeDeepMembers(
        cleanPath2Sequences.map(t => t[1]).map(s => s.map(n => n.edgeKey)),
        constituentPaths(cleanSplit2[1]).map(s => s.map(n => n.edgeKey))
      );

      // splits on the double-accented version (that also does a deleteLeft)
      //
      // Splits after THREE keystrokes - first keystroke emitted one char, then
      // the third = the one that deletes left 1, then emits an accented vowel
      // pair.
      const dirtySplit = splitResults.find((r) => r[0].inputCount + r[1].inputCount == 5);
      assert.isTrue(dirtySplit[0] instanceof SearchQuotientSpur);

      assert.equal(dirtySplit[0].inputCount, 3);
      assert.equal(dirtySplit[1].inputCount, fiveCharCluster.inputCount - 2);

      assert.deepEqual(dirtySplit[0].inputSegments[2], {
        ... fixture.paths[3].path_k3c3_i4d1a.inputSource.segment,
        start: 0,
        end: 1
      });

      assert.isTrue(dirtySplit[1] instanceof SearchQuotientSpur);
      assert.deepEqual(dirtySplit[1].inputSegments[0], {
        ... fixture.paths[3].path_k3c3_i4d1a.inputSource.segment,
        start: 1
      });
      assert.deepEqual(dirtySplit[1].inputSegments[1], fixture.paths[4].path_k4c5_i2.inputSource.segment);

      assert.includeDeepMembers(
        dirtyPathSequences.map(t => t[0]).map(s => s.map(n => n.edgeKey)),
        constituentPaths(dirtySplit[0]).map(s => s.map(n => n.edgeKey))
      );
      assert.includeDeepMembers(
        dirtyPathSequences.map(t => t[1]).map(s => s.map(n => n.edgeKey)),
        constituentPaths(dirtySplit[1]).map(s => s.map(n => n.edgeKey))
      );
    });

    //
    it('properly splits the cluster at index 3 - 4 char cluster', () => {
      const fixture = buildAlphabeticClusterFixtures();
      const fourCharCluster = fixture.clusters.cluster_k4c4;

      assert.equal(constituentPaths(fourCharCluster).length, 4);

      const splitResults = fourCharCluster.split(3);

      // Two split arrays are "clean" (but differ by contributing keystroke
      // count), while the other splits a keystroke in half.
      assert.equal(splitResults.length, 2);
      assert.sameMembers(splitResults.map((r) => r[0].inputCount + r[1].inputCount), [4, 5]);

      // Splits cleanly after 2 keystrokes.
      const cleanSplit = splitResults.find((r) => r[0].inputCount == 3 && + r[1].inputCount == 1);
      assert.isTrue(cleanSplit[0] instanceof SearchQuotientCluster);

      assert.equal(cleanSplit[0].inputCount, 3);
      assert.equal(cleanSplit[1].inputCount, fourCharCluster.inputCount - 3);

      assert.equal(cleanSplit[0], fixture.clusters.cluster_k3c3);
      assert.isTrue(cleanSplit[1] instanceof SearchQuotientSpur); // passes through both final paths

      assert.equal(constituentPaths(cleanSplit[0]).length, 3);
      assert.equal(constituentPaths(cleanSplit[1]).length, 1);

      const dirtySplit = splitResults.find((r) => r[0].inputCount + r[1].inputCount == 5);
      assert.isTrue(dirtySplit[0] instanceof SearchQuotientSpur);

      assert.equal(dirtySplit[0].inputCount, 4);
      assert.equal(dirtySplit[1].inputCount, fourCharCluster.inputCount - 4 + 1);

      assert.deepEqual(dirtySplit[0].inputSegments[3], {
        ... fixture.paths[4].path_k4c4_i2.inputSource.segment,
        start: 0,
        end: 1
      });

      assert.isTrue(dirtySplit[1] instanceof SearchQuotientSpur);
      assert.deepEqual(dirtySplit[1].inputSegments[0], {
        ... fixture.paths[4].path_k4c4_i2.inputSource.segment,
        start: 1
      });
      assert.notOk(dirtySplit[1].inputSegments[1]);

      assert.equal(constituentPaths(dirtySplit[0]).length, 1);
      assert.equal(constituentPaths(dirtySplit[1]).length, 1);
    });

    //
    it('properly splits the cluster at index 3 - 5 char cluster', () => {
      const fixture = buildAlphabeticClusterFixtures();
      const fiveCharCluster = fixture.clusters.cluster_k4c5;

      assert.equal(constituentPaths(fiveCharCluster).length, 5);

      const splitResults = fiveCharCluster.split(3);
      // Two split arrays are "clean" (but differ by contributing keystroke
      // count), while the other splits a keystroke in half.
      assert.equal(splitResults.length, 3);
      assert.sameMembers(splitResults.map((r) => r[0].inputCount + r[1].inputCount), [4, 4, 5]);

      // Splits cleanly after 2 keystrokes.
      const cleanSplit1 = splitResults.find((r) => r[0].inputCount == 2 && + r[1].inputCount == 2);
      assert.isTrue(cleanSplit1[0] instanceof SearchQuotientSpur);

      assert.equal(cleanSplit1[0].inputCount, 2);
      assert.equal(cleanSplit1[1].inputCount, fiveCharCluster.inputCount - 2);

      assert.equal(cleanSplit1[0], fixture.paths[2].path_k2c3_i3d0);
      assert.isTrue(cleanSplit1[1] instanceof SearchQuotientSpur); // passes through both final paths

      // Note that the path structures themselves, however, will be rebuilt - and with new spaceIDs.
      const pathSequenceToInputs = (sequence: SearchQuotientSpur[]) => sequence.map(p => p.inputs);
      // One of the paths leading into cluster_k3c3 passes through the split-point.
      assert.equal(constituentPaths(cleanSplit1[0]).length, 1);
      assert.equal(constituentPaths(cleanSplit1[1]).length, 1);

      // The input distributions should re-appear in the constituent paths, in full.
      assert.includeDeepMembers( // not deepEqual.
        // superset
        constituentPaths(fiveCharCluster).map((sequence) => {
          return sequence.slice(2);
        }).map(pathSequenceToInputs),
        // subset
        constituentPaths(cleanSplit1[1]).map(pathSequenceToInputs)
      );

      // Splits cleanly after 3 keystrokes.
      const cleanSplit2 = splitResults.find((r) => r[0].inputCount == 3 && + r[1].inputCount == 1);
      assert.isTrue(cleanSplit2[0] instanceof SearchQuotientCluster);

      assert.equal(cleanSplit2[0].inputCount, 3);
      assert.equal(cleanSplit2[1].inputCount, fiveCharCluster.inputCount - 3);

      assert.equal(cleanSplit2[0], fixture.clusters.cluster_k3c3);

      assert.isTrue(cleanSplit2[1] instanceof SearchQuotientSpur);

      assert.deepEqual(cleanSplit2[1].inputSegments, [
        fixture.paths[4].path_k4c5_i2.inputSource.segment
      ]);

      assert.equal(constituentPaths(cleanSplit2[0]).length, 3);
      assert.equal(constituentPaths(cleanSplit2[1]).length, 1);

      // splits on the double-accented version (that also does a deleteLeft)
      //
      // Splits after THREE keystrokes - lead keystroke had two chars, emitted
      // one, then emitted the one that deletes left 1, then emits an accented
      // vowel pair.
      const dirtySplit = splitResults.find((r) => r[0].inputCount + r[1].inputCount == 5);
      assert.isTrue(dirtySplit[0] instanceof SearchQuotientSpur);

      assert.equal(dirtySplit[0].inputCount, 3);
      assert.equal(dirtySplit[1].inputCount, fiveCharCluster.inputCount - 2);

      assert.deepEqual(dirtySplit[0].inputSegments[2], {
        ... fixture.paths[3].path_k3c4_i5d1.inputSource.segment,
        start: 0,
        end: 1
      });

      assert.isTrue(dirtySplit[1] instanceof SearchQuotientSpur);
      assert.deepEqual(dirtySplit[1].inputSegments[0], {
        ... fixture.paths[3].path_k3c4_i5d1.inputSource.segment,
        start: 1
      });
      assert.deepEqual(dirtySplit[1].inputSegments[1], fixture.paths[4].path_k4c5_i1.inputSource.segment);

      assert.equal(constituentPaths(dirtySplit[0]).length, 1);
      assert.equal(constituentPaths(dirtySplit[1]).length, 1);
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
      assert.isTrue(cleanSplit[1] instanceof SearchQuotientSpur);

      const pathSequenceToInputs = (sequence: SearchQuotientSpur[]) => sequence.map(p => p.inputs);
      assert.deepEqual(constituentPaths(cleanSplit[1]).length, 1);

      // The input distributions should re-appear in the constituent paths, in full.
      assert.includeDeepMembers( // not deepEqual.
        // superset
        constituentPaths(fiveCharCluster).map((sequence) => {
          return sequence.slice(3);
        }).map(pathSequenceToInputs),
        // subset
        constituentPaths(cleanSplit[1]).map(pathSequenceToInputs)
      );

      // splits on the fourth keystroke variant that outputs two chars.
      const dirtySplit = splitResults.find((r) => r[0].inputCount + r[1].inputCount == 5);
      assert.isTrue(dirtySplit[0] instanceof SearchQuotientSpur);

      assert.equal(dirtySplit[0].inputCount, 4);
      assert.equal(dirtySplit[1].inputCount, 1);

      assert.deepEqual(dirtySplit[0].inputSegments[3], {
        ... fixture.paths[4].path_k4c5_i2.inputSource.segment,
        start: 0,
        end: 1
      });

      assert.isTrue(dirtySplit[1] instanceof SearchQuotientSpur);
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

      const mergeResult = baseCluster.merge(new LegacyQuotientSpur(
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

      assert.isTrue(mergeResult.isSameNode(fixture.paths[4].path_k4c4_i1));
    });

    it('merges in a SearchCluster with unsplit initial inputs', () => {
      const fixture = buildAlphabeticClusterFixtures();

      // This is the cluster we wish to merge new content into.
      const cluster_k3c3 = fixture.clusters.cluster_k3c3;

      // Now, we manually construct the right-hand side that we wish to merge in.
      const rightRoot = new LegacyQuotientRoot(testModel);

      const buildRightHandSide = (root: SearchQuotientNode) => {
        const pathA1 = new LegacyQuotientSpur(root, fixture.distributions[4].distrib_c2_i1d0, fixture.paths[4].path_k4c4_i1.inputSource);
        const pathB1 = new LegacyQuotientSpur(root, fixture.distributions[4].distrib_c2_i2d0, fixture.paths[4].path_k4c5_i2.inputSource);

        const pathA2 = new LegacyQuotientSpur(pathA1, fixture.distributions[5].distrib_c3_i2d0, fixture.paths[5].path_k5c6_a.inputSource);
        const pathB2 = new LegacyQuotientSpur(pathB1, fixture.distributions[5].distrib_c3_i1d0, fixture.paths[5].path_k5c6_b.inputSource);

        const rightHandCluster = new SearchQuotientCluster([pathA2, pathB2]);
        return rightHandCluster;
      }

      const rightHandCluster = buildRightHandSide(rightRoot);
      const targetCluster = buildRightHandSide(cluster_k3c3);
      const merged = cluster_k3c3.merge(rightHandCluster);

      assert.isTrue(merged.isSameNode(targetCluster));
    });

    it('merges in a SearchPath with multiple inputs & an ancestor SearchCluster', () => {
      const fixture = buildAlphabeticClusterFixtures();

      // This is the cluster we wish to merge new content into.
      const cluster_k3c3 = fixture.clusters.cluster_k3c3;

      // Now, we manually construct the right-hand side that we wish to merge in.
      const rightRoot = new LegacyQuotientRoot(testModel);

      const finalInputSubsetId = generateSubsetId();

      const buildRightHandSide = (root: SearchQuotientNode) => {
        const pathA1 = new LegacyQuotientSpur(root, fixture.distributions[4].distrib_c2_i1d0, fixture.paths[4].path_k4c4_i1.inputSource);
        const pathB1 = new LegacyQuotientSpur(root, fixture.distributions[4].distrib_c2_i2d0, fixture.paths[4].path_k4c5_i2.inputSource);

        const pathA2 = new LegacyQuotientSpur(pathA1, fixture.distributions[5].distrib_c3_i2d0, fixture.paths[5].path_k5c6_a.inputSource);
        const pathB2 = new LegacyQuotientSpur(pathB1, fixture.distributions[5].distrib_c3_i1d0, fixture.paths[5].path_k5c6_b.inputSource);

        const rightHandCluster = new SearchQuotientCluster([pathA2, pathB2]); // endpoint for prior test.

        // Append an extra path step.
        const final_dist: Distribution<Transform> = [
          { sample: { insert: '0', deleteLeft: 0, deleteRight: 0, id: 16 }, p: 0.6 }, // most likely for id 16
          { sample: { insert: '1', deleteLeft: 0, deleteRight: 0, id: 16 }, p: 0.4 }
        ];

        const finalPath = new LegacyQuotientSpur(rightHandCluster, final_dist, {
          segment: {
            transitionId: 16,
            start: 0
          },
          bestProbFromSet: final_dist[0].p,
          subsetId: finalInputSubsetId
        });
        return finalPath;
      }

      const merged = cluster_k3c3.merge(buildRightHandSide(rightRoot));
      assert.isTrue(merged.isSameNode(buildRightHandSide(cluster_k3c3)));
    });

    it('remerges the components of a previous .split() result that split input keystrokes', () => {
      // First:  creating a SearchSpace matching the desired merge results for this test.
      const rootPath = new LegacyQuotientRoot(testModel);

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

      const path_k1c2_i2 = new LegacyQuotientSpur(rootPath, distrib_k1_i2, distrib_k1_i2[0]);
      const path_k1c3_i3 = new LegacyQuotientSpur(rootPath, distrib_k1_i3, distrib_k1_i2[0]);

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

      const path_k2c4_1 = new LegacyQuotientSpur(path_k1c2_i2, distrib_k2_i2, distrib_k2_i1[0]);
      const path_k2c4_2 = new LegacyQuotientSpur(path_k1c3_i3, distrib_k2_i1, distrib_k2_i1[0]);

      const targetCluster = new SearchQuotientCluster([path_k2c4_1, path_k2c4_2]);

      // Now:  constructing the left-hand components.
      const path_k1c2_i2_head = new LegacyQuotientSpur(rootPath,
        splitDistribution(distrib_k1_i2, 1).head, {
          ...path_k1c2_i2.inputSource,
          segment: {
            ...path_k1c2_i2.inputSource.segment,
            end: 1
          }
        }
      );
      const path_k1c3_i3_head = new LegacyQuotientSpur(rootPath,
        splitDistribution(distrib_k1_i3, 1).head, {
          ...path_k1c3_i3.inputSource,
          segment: {
            ...path_k1c3_i3.inputSource.segment,
            end: 1
          }
        }
      );

      const splitHeadCluster = new SearchQuotientCluster([path_k1c2_i2_head, path_k1c3_i3_head]);

      // And now the right-hand side of the split.

      const tailRoot = new LegacyQuotientRoot(testModel);

      // Now:  constructing the left-hand components.
      const path_k1c2_i2_tail = new LegacyQuotientSpur(tailRoot,
        splitDistribution(distrib_k1_i2, 1).tail, {
          ...path_k1c2_i2.inputSource,
          segment: {
            ...path_k1c2_i2.inputSource.segment,
            start: 1
          }
        }
      );
      const path_k1c3_i3_tail = new LegacyQuotientSpur(tailRoot,
        splitDistribution(distrib_k1_i3, 1).tail, {
          ...path_k1c3_i3.inputSource,
          segment: {
            ...path_k1c3_i3.inputSource.segment,
            start: 1
          }
        }
      );

      const path_k2c4_1_tail = new LegacyQuotientSpur(path_k1c2_i2_tail, distrib_k2_i2, {
        ...path_k2c4_1.inputSource
      });
      const path_k2c4_2_tail = new LegacyQuotientSpur(path_k1c3_i3_tail, distrib_k2_i1, {
        ...path_k2c4_2.inputSource
      });

      const splitTailCluster = new SearchQuotientCluster([path_k2c4_1_tail, path_k2c4_2_tail]);
      const remergedCluster = splitHeadCluster.merge(splitTailCluster);

      assert.isTrue(remergedCluster.isSameNode(targetCluster));
      assert.equal(remergedCluster.inputCount, targetCluster.inputCount);
      assert.deepEqual(remergedCluster.inputSegments, targetCluster.inputSegments);
      assert.isTrue(quotientPathHasInputs(
        remergedCluster, [
        distrib_k1_i3, distrib_k2_i1
      ]));
      assert.isTrue(quotientPathHasInputs(
        remergedCluster, [
        distrib_k1_i2, distrib_k2_i2
      ]));
    });
  });
});