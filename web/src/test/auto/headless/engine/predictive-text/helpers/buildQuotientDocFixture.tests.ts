import { assert } from "chai";

import { buildQuotientDocFixture } from "./buildQuotientDocFixture.js";

describe('buildQuotientDocFixture() fixture', () => {
    it('constructs paths properly', () => {
      const {searchRoot, nodes} = buildQuotientDocFixture();

      [searchRoot, nodes.sc1, nodes.sc2].forEach((n) => {
        assert.equal(n.inputCount, 0);
      });
      [/*nodes.k1c0,*/ nodes.k1c1, nodes.k1c2, nodes.k1c3].forEach((n) => {
        assert.equal(n.inputCount, 1);
      });
      [/*nodes.k2c0, nodes.k2c1,*/ nodes.k2c2, nodes.k2c3].forEach((n) => {
        assert.equal(n.inputCount, 2);
      });

      [searchRoot/*, nodes.k1c0, nodes.k2c0*/].forEach((n) => {
        assert.equal(n.codepointLength, 0);
      });
      [nodes.sc1, nodes.k1c1/*, nodes.k2c1*/].forEach((n) => {
        assert.equal(n.codepointLength, 1);
      });
      [nodes.sc2, nodes.k1c2, nodes.k2c2].forEach((n) => {
        assert.equal(n.codepointLength, 2);
      });
      [nodes.k1c3, nodes.k2c3].forEach((n) => {
        assert.equal(n.codepointLength, 3);
      });
    });
});