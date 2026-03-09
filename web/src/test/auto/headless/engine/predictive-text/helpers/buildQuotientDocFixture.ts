/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-09
 *
 * This file defines a unit-text fixture designed for testing
 * the internal mechanisms of a search quotient graph built from
 * quotient-spurs specialized for each of the three main edit-distance
 * operation types.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import {
  generateSubsetId,
  InsertionQuotientSpur,
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
 * Builds a text fixture matching the [final quotient-graph example](
 * ../../../../../../../engine/predictive-text/worker-thread/docs/correction-search-graph.md)
 * documenting the internal SearchQuotientNode design.
 * @returns
 */
export function buildQuotientDocFixture() {
  const searchRoot = new SearchQuotientRoot(testModel);
  let idSeed = 0;

  const key1Id = idSeed++;
  const abDistrib: Distribution<Transform> = [
    { sample: { insert: 'a', deleteLeft: 0, id: key1Id }, p: .45 },
    { sample: { insert: 'b', deleteLeft: 0, id: key1Id }, p: .35 }
  ];

  const cdDistrib: Distribution<Transform> = [
    { sample: { insert: 'cd', deleteLeft: 0, id: key1Id }, p: .2 }
  ];

  const sc1 = new InsertionQuotientSpur(searchRoot);
  const sc2 = new InsertionQuotientSpur(sc1);

  // // K1C0
  // const k1c0 = new DeletionQuotientSpur(searchRoot, abDistrib.concat(cdDistrib), {
  //   segment: {
  //     transitionId: key1Id,
  //     start: 0
  //   },
  //   // Deletions always get their own unique subset ID.
  //   subsetId: generateSubsetId(),
  //   bestProbFromSet: abDistrib[0].p
  // });

  // // K1C1
  // const k1c1_del = new DeletionQuotientSpur(sc1, abDistrib.concat(cdDistrib), {
  //   segment: {
  //     transitionId: key1Id,
  //     start: 0
  //   },
  //   // Deletions always get their own unique subset ID.
  //   subsetId: generateSubsetId(),
  //   bestProbFromSet: abDistrib[0].p
  // });
  const k1c1_ab = new SubstitutionQuotientSpur(searchRoot, abDistrib, {
    segment: {
      transitionId: key1Id,
      start: 0
    },
    // Deletions always get their own unique subset ID.
    subsetId: generateSubsetId(),
    bestProbFromSet: abDistrib[0].p
  });
  // const k1c1_ins = new InsertionQuotientSpur(k1c0);
  const k1c1 = new SearchQuotientCluster([/*k1c1_del,*/ k1c1_ab, /*k1c1_ins*/]);

  // const k1c2_del = new DeletionQuotientSpur(sc2, abDistrib.concat(cdDistrib), {
  //   segment: {
  //     transitionId: key1Id,
  //     start: 0
  //   },
  //   // Deletions always get their own unique subset ID.
  //   subsetId: generateSubsetId(),
  //   bestProbFromSet: abDistrib[0].p
  // });
  const k1c2_ab = new SubstitutionQuotientSpur(sc1, abDistrib, {
    segment: {
      transitionId: key1Id,
      start: 0
    },
    // Deletions always get their own unique subset ID.
    subsetId: generateSubsetId(),
    bestProbFromSet: abDistrib[0].p
  });
  const k1c2_cd = new SubstitutionQuotientSpur(searchRoot, cdDistrib, {
    segment: {
      transitionId: key1Id,
      start: 0
    },
    // Deletions always get their own unique subset ID.
    subsetId: generateSubsetId(),
    bestProbFromSet: abDistrib[0].p
  });
  const k1c2_ins = new InsertionQuotientSpur(k1c1);
  const k1c2 = new SearchQuotientCluster([/*k1c2_del, */ k1c2_ab, k1c2_cd, k1c2_ins]);

  const k1c3_ab = new SubstitutionQuotientSpur(sc2, abDistrib, {
    segment: {
      transitionId: key1Id,
      start: 0
    },
    // Deletions always get their own unique subset ID.
    subsetId: generateSubsetId(),
    bestProbFromSet: abDistrib[0].p
  });
  const k1c3_cd = new SubstitutionQuotientSpur(sc1, cdDistrib, {
    segment: {
      transitionId: key1Id,
      start: 0
    },
    // Deletions always get their own unique subset ID.
    subsetId: generateSubsetId(),
    bestProbFromSet: abDistrib[0].p
  });
  const k1c3_ins = new InsertionQuotientSpur(k1c2);
  const k1c3 = new SearchQuotientCluster([k1c3_ab, k1c3_cd, k1c3_ins]);

  // Onto keystroke 2.

  const key2Id = idSeed++;
  const efDistrib: Distribution<Transform> = [
    { sample: { insert: 'e', deleteLeft: 0, id: key2Id }, p: .4 },
    { sample: { insert: 'f', deleteLeft: 0, id: key2Id }, p: .3 }
  ];

  const ghDistrib: Distribution<Transform> = [
    { sample: { insert: 'gh', deleteLeft: 0, id: key2Id }, p: .3 }
  ];

  // const k2c0 = new DeletionQuotientSpur(k1c0, efDistrib.concat(ghDistrib), {
  //   segment: {
  //     transitionId: key2Id,
  //     start: 0
  //   },
  //   // Deletions always get their own unique subset ID.
  //   subsetId: generateSubsetId(),
  //   bestProbFromSet: efDistrib[0].p
  // });

  // const k2c1_del = new DeletionQuotientSpur(k1c1, efDistrib.concat(ghDistrib), {
  //   segment: {
  //     transitionId: key2Id,
  //     start: 0
  //   },
  //   // Deletions always get their own unique subset ID.
  //   subsetId: generateSubsetId(),
  //   bestProbFromSet: efDistrib[0].p
  // });
  // const k2c1_ef = new SubstitutionQuotientSpur(k1c0, efDistrib, {
  //   segment: {
  //     transitionId: key2Id,
  //     start: 0
  //   },
  //   // Deletions always get their own unique subset ID.
  //   subsetId: generateSubsetId(),
  //   bestProbFromSet: efDistrib[0].p
  // });
  // const k2c1_ins = new InsertionQuotientSpur(k2c0);
  // const k2c1 = new SearchQuotientCluster([k2c1_del, k2c1_ef, k2c1_ins]);

  // const k2c2_del = new DeletionQuotientSpur(k1c2, efDistrib.concat(ghDistrib), {
  //   segment: {
  //     transitionId: key2Id,
  //     start: 0
  //   },
  //   // Deletions always get their own unique subset ID.
  //   subsetId: generateSubsetId(),
  //   bestProbFromSet: efDistrib[0].p
  // });
  const k2c2_ef = new SubstitutionQuotientSpur(k1c1, efDistrib, {
    segment: {
      transitionId: key2Id,
      start: 0
    },
    // Deletions always get their own unique subset ID.
    subsetId: generateSubsetId(),
    bestProbFromSet: efDistrib[0].p
  });
  // const k2c2_gh = new SubstitutionQuotientSpur(k1c0, ghDistrib, {
  //   segment: {
  //     transitionId: key2Id,
  //     start: 0
  //   },
  //   // Deletions always get their own unique subset ID.
  //   subsetId: generateSubsetId(),
  //   bestProbFromSet: efDistrib[0].p
  // });
  // const k2c2_ins = new InsertionQuotientSpur(k2c1);
  const k2c2 = new SearchQuotientCluster([/*k2c2_del,*/ k2c2_ef /*, k2c2_gh, k2c2_ins*/]);

  // const k2c3_del = new DeletionQuotientSpur(k1c3, efDistrib.concat(ghDistrib), {
  //   segment: {
  //     transitionId: key2Id,
  //     start: 0
  //   },
  //   // Deletions always get their own unique subset ID.
  //   subsetId: generateSubsetId(),
  //   bestProbFromSet: efDistrib[0].p
  // });
  const k2c3_ef = new SubstitutionQuotientSpur(k1c2, efDistrib, {
    segment: {
      transitionId: key2Id,
      start: 0
    },
    // Deletions always get their own unique subset ID.
    subsetId: generateSubsetId(),
    bestProbFromSet: efDistrib[0].p
  });
  const k2c3_gh = new SubstitutionQuotientSpur(k1c1, ghDistrib, {
    segment: {
      transitionId: key2Id,
      start: 0
    },
    // Deletions always get their own unique subset ID.
    subsetId: generateSubsetId(),
    bestProbFromSet: efDistrib[0].p
  });
  const k2c3_ins = new InsertionQuotientSpur(k2c2);
  const k2c3 = new SearchQuotientCluster([/*k2c3_del, */ k2c3_ef, k2c3_gh, k2c3_ins]);

  return {
    searchRoot,
    spurs: {sc1, sc2, k1c1_ab, k1c2_ab, k1c2_cd, k1c2_ins, k1c3_ab, k1c3_cd, k1c3_ins, k2c2_ef, k2c3_ef, k2c3_gh, k2c3_ins},
    nodes: {sc1, sc2, /* k1c0, */ k1c1, k1c2, k1c3, /* k2c0, k2c1, */ k2c2, k2c3}
  };
}
