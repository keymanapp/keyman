/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-09
 *
 * This file defines a unit-text fixture designed for testing
 * behaviors related to the clustering of search quotient spurs
 * based on just substitution/match edges.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import {
  models,
  SearchQuotientCluster,
  SearchQuotientRoot,
  SubstitutionQuotientSpur
} from '@keymanapp/lm-worker/test-index';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

/**
 * Builds a fixture for use in unit-testing against diverging and re-converging
 * routes through the modeled correction-search graph and its SearchQuotientNode
 * representation.
 * @returns
 */
export const buildAlphabeticClusterFixtures = () => {
  const rootPath = new SearchQuotientRoot(testModel);

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
  const path_k1c1_i1d0 = new SubstitutionQuotientSpur(rootPath, distrib_c1_i1d0, distrib_c1_i1d0[0]);
  // keystrokes 1, codepoints 2, total inserts 2, delete 0
  const path_k1c2_i2d0 = new SubstitutionQuotientSpur(rootPath, distrib_c1_i2d0, distrib_c1_i1d0[0]);

  // Second input

  const distrib_v1_i1d0: Distribution<Transform> = [
    { sample: { insert: 'e', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.4 }, // most likely for id 12
    { sample: { insert: 'a', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.3 },
    { sample: { insert: 'i', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.1 },
    { sample: { insert: 'o', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.1 },
    { sample: { insert: 'u', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.1 },
  ];

  const path_k2c2_i2d0 = new SubstitutionQuotientSpur(path_k1c1_i1d0, distrib_v1_i1d0, distrib_v1_i1d0[0]);
  const path_k2c3_i3d0 = new SubstitutionQuotientSpur(path_k1c2_i2d0, distrib_v1_i1d0, distrib_v1_i1d0[0]);

  // Third input
  const distrib_v2_i1d0: Distribution<Transform> = [
    { sample: { insert: 'e', deleteLeft: 0, deleteRight: 0, id: 13 }, p: 0.15 }, // most likely for id 13
    { sample: { insert: 'a', deleteLeft: 0, deleteRight: 0, id: 13 }, p: 0.13 },
    { sample: { insert: 'i', deleteLeft: 0, deleteRight: 0, id: 13 }, p: 0.12 },
    { sample: { insert: 'o', deleteLeft: 0, deleteRight: 0, id: 13 }, p: 0.11 },
    { sample: { insert: 'u', deleteLeft: 0, deleteRight: 0, id: 13 }, p: 0.09 },
  ]; // 0.60 total

  const distrib_v2_i1d1: Distribution<Transform> = [
    { sample: { insert: 'é', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.06 },
    { sample: { insert: 'á', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.05 },
    { sample: { insert: 'í', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.04 },
    { sample: { insert: 'ó', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.03 },
    { sample: { insert: 'ú', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.02 },
  ]; // 0.2 total

  const distrib_v2_i2d1: Distribution<Transform> = [
    { sample: { insert: 'éé', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.06 },
    { sample: { insert: 'áá', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.05 },
    { sample: { insert: 'íí', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.04 },
    { sample: { insert: 'óó', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.03 },
    { sample: { insert: 'úú', deleteLeft: 1, deleteRight: 0, id: 13 }, p: 0.02 },
  ]; // 0.2 total

  const path_k3c2_i3d1 = new SubstitutionQuotientSpur(path_k2c2_i2d0, distrib_v2_i1d1, distrib_v2_i1d0[0]);

  const path_k3c3_i3d0  = new SubstitutionQuotientSpur(path_k2c2_i2d0, distrib_v2_i1d0, distrib_v2_i1d0[0]);
  const path_k3c3_i4d1a = new SubstitutionQuotientSpur(path_k2c2_i2d0, distrib_v2_i2d1, distrib_v2_i1d0[0]);
  const path_k3c3_i4d1b = new SubstitutionQuotientSpur(path_k2c3_i3d0, distrib_v2_i1d1, distrib_v2_i1d0[0]);

  // both are built on path k1c2 (splits at index 1)
  const path_k3c4_i4d0 = new SubstitutionQuotientSpur(path_k2c3_i3d0, distrib_v2_i1d0, distrib_v2_i1d0[0]);
  const path_k3c4_i5d1 = new SubstitutionQuotientSpur(path_k2c3_i3d0, distrib_v2_i2d1, distrib_v2_i1d0[0]);

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

  const path_k4c4_i2 = new SubstitutionQuotientSpur(path_k3c2_i3d1, distrib_c2_i2d0, distrib_c2_i2d0[0]);
  const path_k4c4_i1 = new SubstitutionQuotientSpur(cluster_k3c3, distrib_c2_i1d0, distrib_c2_i2d0[0]);

  const path_k4c5_i2 = new SubstitutionQuotientSpur(cluster_k3c3, distrib_c2_i2d0, distrib_c2_i2d0[0]);
  const path_k4c5_i1 = new SubstitutionQuotientSpur(cluster_k3c4, distrib_c2_i1d0, distrib_c2_i2d0[0]);

  const path_k4c6 = new SubstitutionQuotientSpur(cluster_k3c4, distrib_c2_i2d0, distrib_c2_i2d0[0]);

  const cluster_k4c4 = new SearchQuotientCluster([path_k4c4_i2, path_k4c4_i1]);
  const cluster_k4c5 = new SearchQuotientCluster([path_k4c5_i2, path_k4c5_i1]);

  // Input 5 (currently used only for merge tests)
  const distrib_c3_i2d0: Distribution<Transform> = [
    { sample: { insert: 'xy', deleteLeft: 0, deleteRight: 0, id: 15 }, p: 0.6 } // most likely for id 15
  ];
  const distrib_c3_i1d0: Distribution<Transform> = [
    { sample: { insert: 'z', deleteLeft: 0, deleteRight: 0, id: 15 }, p: 0.4 }
  ];

  const path_k5c6_a = new SubstitutionQuotientSpur(cluster_k4c4, distrib_c3_i2d0, distrib_c3_i2d0[0]);
  const path_k5c6_b = new SubstitutionQuotientSpur(cluster_k4c5, distrib_c3_i1d0, distrib_c3_i2d0[0]);

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
    root: rootPath,
    paths: {
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