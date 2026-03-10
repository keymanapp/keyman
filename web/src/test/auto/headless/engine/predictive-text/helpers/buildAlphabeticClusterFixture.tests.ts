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

    allPaths.forEach((spur) => assert.isTrue(finalClusterPaths.find(seq => seq.indexOf(spur) > -1)));
    allDists.forEach((dist) => allPaths.find(path => quotientPathHasInputs(path, [dist])));
  });
});