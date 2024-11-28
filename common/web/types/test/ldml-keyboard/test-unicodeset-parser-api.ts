/*
 * Keyman is copyright (C) SIL Global. MIT License.
 * 
 * Created by Dr Mark C. Sinclair on 2024-11-28
 * 
 * Test code for unicodeset-parser-api.ts
 */

import 'mocha';
import { assert } from 'chai';
import { UnicodeSet } from '../../src/ldml-keyboard/unicodeset-parser-api.js';

class MockUnicodeSet extends UnicodeSet {
  constructor(public pattern: string, public ranges: number[][]) {
    super(pattern, ranges); // does nothing
    this.pattern = pattern;
    this.ranges  = ranges;
  }
}

describe('Test of Unicode-Parser-API', () => {
  describe('Test UnicodeSet', () => {
    it('can provide a correct ranges length', () => {
      const unicodeSet = new MockUnicodeSet("[ħa-z]", [[0x41, 0x7A], [0x0127, 0x0127]]);
      assert.equal(unicodeSet.length, 2);
    });
    it('can provide a correct string representation', () => {
      const unicodeSet = new MockUnicodeSet("[ħa-z]", [[0x41, 0x7A], [0x0127, 0x0127]]);
      assert.deepEqual(unicodeSet.toString(), "[ħa-z]");
    });
  });
});