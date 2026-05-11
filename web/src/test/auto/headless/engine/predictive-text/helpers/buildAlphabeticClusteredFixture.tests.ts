/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-09
 *
 * This file tests the construction of the helper test-fixture function in
 * buildAlphabeticClusteredFixture.ts.
 */

import { assert } from "chai";

import { SearchQuotientNode } from "@keymanapp/lm-worker/test-index";

import { constituentPaths } from "./constituentPaths.js";
import { quotientPathHasInputs } from "./quotientPathHasInputs.js";
import { buildAlphabeticClusterFixtures } from "./buildAlphabeticClusteredFixture.js";

describe('buildAlphabeticClusteredFixture() fixture', () => {
  it('constructs paths properly', () => {
    const { clusters, paths, distributions } = buildAlphabeticClusterFixtures();
    assert.equal(clusters.cluster_k5c6.inputCount, 5);

    const allPaths = Object.values(paths).map(set => Object.values(set)).flat() as SearchQuotientNode[];
    const allDists = Object.values(distributions).map(set => Object.values(set)).flat();
    const finalClusterPaths = constituentPaths(clusters.cluster_k5c6) as SearchQuotientNode[][];

    allPaths.forEach((spur) => assert.isOk(finalClusterPaths.find(seq => seq.indexOf(spur) > -1)));
    allDists.forEach((dist) => assert.isOk(allPaths.find(path => quotientPathHasInputs(path, [dist]))));
  });
});