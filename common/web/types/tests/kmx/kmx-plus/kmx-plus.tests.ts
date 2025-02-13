/*
 * Keyman is copyright (C) SIL Global. MIT License.
 * 
 * Created by Dr Mark C. Sinclair on 2025-02-03
 * 
 * Test code for kmx-plus.ts
 */

import 'mocha';
import { assert } from 'chai';
import { Strs, StrsItem, StrsOptions, DependencySections } from '../../../src/kmx/kmx-plus/kmx-plus.js';

const GOTHIC_A = new StrsItem("𐌰", 0x10330);

describe('Test of KMX Plus file', () => {
  describe('Test of Strs', () => {
    describe('Test of implicit constructor', () => {
      it('can construct a Strs', () => {
        const strs = new Strs();
        assert.isNotNull(strs);
        assert.deepEqual(strs.strings, [new StrsItem('')]);
        assert.deepEqual(strs.allProcessedStrings, new Set<string>());
      });
    });
    describe('Test of allocString()', () => {
      it('can allocate a one-character string', () => {
        const strs = new Strs();
        strs['processString'] = stubStrsProcessString;
        const csi = strs.allocString("𐌰", { singleOk: true });
        assert.deepEqual(csi, GOTHIC_A);
      });
      it('can allocate an unknown string', () => {
        const strs = new Strs();
        strs['processString'] = stubStrsProcessString;
        const si = strs.allocString("𐌰𐌱𐌲");
        assert.deepEqual(si, new StrsItem("𐌰𐌱𐌲"));
        assert.deepEqual(strs.strings, [new StrsItem(''), new StrsItem("𐌰𐌱𐌲")]);
      });
      it('can allocate a known string', () => {
        const strs = new Strs();
        strs['processString'] = stubStrsProcessString;
        const one = strs.allocString("𐌰𐌱𐌲");
        assert.deepEqual(one, new StrsItem("𐌰𐌱𐌲"));
        assert.deepEqual(strs.strings, [new StrsItem(''), new StrsItem("𐌰𐌱𐌲")]);
        const two = strs.allocString("𐌰𐌱𐌲");
        assert.isTrue(two === one);
        assert.deepEqual(strs.strings, [new StrsItem(''), new StrsItem("𐌰𐌱𐌲")]);
      });
    });
  });
});

function stubStrsProcessString(s: string, opts: StrsOptions, sections: DependencySections) {
  return s;
}
