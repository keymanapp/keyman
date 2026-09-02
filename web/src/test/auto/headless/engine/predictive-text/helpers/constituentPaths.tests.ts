/**
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-10
 *
 * This file adds unit tests for unit-test helper functions validating
 * the correction-search modules of the Keyman predictive-text engine.
 */

import { assert } from 'chai';

import { constituentPaths } from "./constituentPaths.js";
import { buildCantLinearFixture } from './buildCantLinearFixture.js';
import { buildAlphabeticClusterFixtures } from './buildAlphabeticClusteredFixture.js';

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
});