import { assert } from "chai";

import { constituentPaths } from "./constituentPaths.js";
import { quotientPathHasInputs } from "./quotientPathHasInputs.js";
import { buildCantLinearFixture } from "./buildCantLinearFixture.js";

describe('buildCantLinearFixture() fixture', () => {
  it('constructs paths properly', () => {
    const { paths, distributions } = buildCantLinearFixture();
    const pathToSplit = paths[4];

    assert.equal(pathToSplit.inputCount, 4);
    assert.equal(distributions.length, pathToSplit.inputCount);
    assert.equal(pathToSplit.codepointLength, 4); // one char per input, no deletions anywhere
    // Per assertions documented in the setup above.
    assert.deepEqual(pathToSplit.bestExample, distributions.reduce(
      (constructing, current) => ({text: constructing.text + current[0].sample.insert, p: constructing.p * current[0].p}),
      {text: '', p: 1})
    );
    assert.deepEqual(pathToSplit.parents[0].bestExample, distributions.slice(0, pathToSplit.inputCount-1).reduce(
      (constructing, current) => ({text: constructing.text + current[0].sample.insert, p: constructing.p * current[0].p}),
      {text: '', p: 1})
    );
    assert.isTrue(quotientPathHasInputs(pathToSplit, distributions));
    assert.equal(constituentPaths(pathToSplit).length, 1);
  });
});