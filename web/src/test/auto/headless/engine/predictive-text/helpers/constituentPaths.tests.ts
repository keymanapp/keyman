/**
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-10
 *
 * This file adds unit tests for unit-test helper functions validating
 * the correction-search modules of the Keyman predictive-text engine.
 */

import { assert } from 'chai';

import { DeletionQuotientSpur, InsertionQuotientSpur } from '@keymanapp/lm-worker/test-index';

import { constituentPaths } from "./constituentPaths.js";
import { toSpurTypeSequence } from './toSpurTypeSequence.js';
import { buildCantLinearFixture } from './buildCantLinearFixture.js';
import { buildAlphabeticClusterFixtures } from './buildAlphabeticClusteredFixture.js';
import { buildQuotientDocFixture } from './buildQuotientDocFixture.js';

describe('constituentPaths', () => {
  it('includes a single entry array when all parents are SearchQuotientSpurs', () => {
    const { paths } = buildCantLinearFixture();
    const finalPath = paths[4];

    assert.equal(constituentPaths(finalPath).length, 1);

    const pathSequence = constituentPaths(finalPath)[0];
    assert.equal(pathSequence.length, 4); // 4 inputs; does not include root node

    assert.sameOrderedMembers(pathSequence, paths.slice(1));
  });

  it('properly enumerates child paths when encountering SearchCluster ancestors', () => {
    const fixture = buildAlphabeticClusterFixtures();
    const finalPath = fixture.paths[4].path_k4c6;

    // The longest SearchPath at the end of that fixture's set is based on a
    // lead-in cluster; all variants of that should be included.
    assert.equal(constituentPaths(finalPath).length, constituentPaths(fixture.clusters.cluster_k3c4).length);

    // That cluster holds the different potential penultimate paths;
    // finalPath's inputs are added directly after any variation that may be
    // output from the cluster.
    assert.sameDeepMembers(constituentPaths(finalPath), constituentPaths(fixture.clusters.cluster_k3c4).map((p) => {
      p.push(finalPath);
      return p;
    }));
  });

  describe('for the final quotient-graph doc example', () => {
    it('handles insertion-only quotient-graph paths', () => {
      const { sc2 } = buildQuotientDocFixture().nodes;

      const sc2Constituents = constituentPaths(sc2);
      assert.equal(sc2Constituents.length, 1);
      sc2Constituents.forEach(s => s.forEach(p => assert.isTrue(p instanceof InsertionQuotientSpur)));
    });

    it('handles deletion-only quotient-graph paths', () => {
      const { k2c0 } = buildQuotientDocFixture().nodes;

      const k2c0Constituents = constituentPaths(k2c0);
      assert.equal(k2c0Constituents.length, 1);
      k2c0Constituents.forEach(s => s.forEach(p => assert.isTrue(p instanceof DeletionQuotientSpur)));
    });

    it('does not emit sequences with inserts immediately following deletes', () => {
      const { k2c3 } = buildQuotientDocFixture().nodes;

      const k2c3Constituents = constituentPaths(k2c3);

      const shouldNotOccur = k2c3Constituents.find((seq) => {
        const typeSeq = toSpurTypeSequence(seq);
        return typeSeq.find((type, index) => {
          return type == 'delete' && typeSeq[index+1] == 'insert';
        });
      });
      assert.isNotOk(shouldNotOccur);
    });

    it('does emit sequences with deletes immediately following inserts', () => {
      const { k2c3 } = buildQuotientDocFixture().nodes;

      const k2c3Constituents = constituentPaths(k2c3);

      const shouldOccur = k2c3Constituents.find((seq) => {
        const typeSeq = toSpurTypeSequence(seq);
        return typeSeq.find((type, index) => {
          return type == 'insert' && typeSeq[index+1] == 'delete';
        });
      });
      assert.isNotOk(shouldOccur);
    });
  });
});