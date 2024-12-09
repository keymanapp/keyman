/*
 * Keyman is copyright (C) SIL Global. MIT License.
 * 
 * Created by Dr Mark C. Sinclair on 2024-12-09
 * 
 * Test code for element-string.ts
 */

import 'mocha';
import { assert } from 'chai';
import { ElemElementFlags, ElemElement } from '../../../src/kmx/kmx-plus/element-string.js';
import { StrsItem, UsetItem } from '../../../src/kmx/kmx-plus/kmx-plus.js';
import { UnicodeSet } from '../../../src/ldml-keyboard/unicodeset-parser-api.js';

const GOTHIC_A = new StrsItem("𐌰", 0x10330);
const GOTHIC_A_SET = new UsetItem(
  new UnicodeSet("[𐌰-𐍊]", [[0x10330,0x1034A]]),
  GOTHIC_A
);

describe('Test of ElementString', () => {
  describe('Test of ElemElement', () => {
    describe('Test of isEqual()', () => {
      it('returns true when elems identical', () => {
        const one = initElemElement();
        const two = initElemElement()
        assert.isTrue(one.isEqual(two));
      });
    });
  });
});

function initElemElement(
  value: StrsItem = GOTHIC_A,
  uset: UsetItem = GOTHIC_A_SET,
  order: number = 0,
  tertiary: number = 1,
  flags: ElemElementFlags = ElemElementFlags.none,
): ElemElement {
  const ee  = new ElemElement();
  ee.value = value;
  ee.uset = uset;
  ee.order = order;
  ee.tertiary = tertiary;
  ee.flags = flags;
  return ee;
}
