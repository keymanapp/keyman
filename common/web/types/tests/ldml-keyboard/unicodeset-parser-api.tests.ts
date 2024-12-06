/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2024-11-29
 *
 * Test code for unicodeset-parser-api.ts
 */

import 'mocha';
import { assert } from 'chai';
import { UnicodeSet } from '../../src/ldml-keyboard/unicodeset-parser-api.js';

describe('Test of Unicode-Parser-API', () => {
  describe('Test UnicodeSet', () => {
    it('can provide a correct ranges length', () => {
      const unicodeSet = new UnicodeSet("[ħa-z]", [[0x41, 0x7A], [0x0127, 0x0127]]);
      assert.equal(unicodeSet.length, 2);
    });
    it('can provide a correct string representation', () => {
      const unicodeSet = new UnicodeSet("[ħa-z]", [[0x41, 0x7A], [0x0127, 0x0127]]);
      assert.deepEqual(unicodeSet.toString(), "[ħa-z]");
    });
  });
});
