/*
 * Keyman is copyright (C) SIL Global. MIT License.
 * 
 * Created by Dr Mark C. Sinclair on 2024-12-09
 * 
 * Test code for element-string.ts
 */

import 'mocha';
import { assert } from 'chai';
import { ElemElementFlags, ElemElement, ElementString } from '../../../src/kmx/kmx-plus/element-string.js';
import { StrsItem, UsetItem, Strs, StrsOptions, DependencySections, CharStrsItem } from '../../../src/kmx/kmx-plus/kmx-plus.js';
import { UnicodeSet } from '../../../src/ldml-keyboard/unicodeset-parser-api.js';
import { ElementParser, ElementSegment, ElementType } from '../../../src/ldml-keyboard/pattern-parser.js';

const GOTHIC_A = new StrsItem("𐌰", 0x10330);
const GOTHIC_B = new StrsItem("𐌱", 0x10331);
const GOTHIC_C = new StrsItem("𐌲", 0x10332);
const UGARITIC_A = new StrsItem("𐎀", 0x10380);
const GOTHIC_SET = new UsetItem(
  new UnicodeSet("[𐌰-𐍊]", [[0x10330,0x1034A]]),
  GOTHIC_A
);
const UGARITIC_SET = new UsetItem(
  new UnicodeSet("[𐎀-𐎟]", [[0x10380,0x1039F]]),
  UGARITIC_A
);

let origElementParserSegment = ElementParser.segment;

describe('Test of ElementString', () => {
  describe('Test of ElemElement', () => {
    describe('Test of isEqual()', () => {
      it('returns true when elems identical', () => {
        const one = initElemElement();
        const two = initElemElement()
        assert.isTrue(one.isEqual(two));
      });
      it('returns false when value differs', () => {
        const one = initElemElement(GOTHIC_A);
        const two = initElemElement(GOTHIC_B);
        assert.isFalse(one.isEqual(two));
      });
      it('returns false when order differs', () => {
        const one = initElemElement(GOTHIC_A, GOTHIC_SET, 0);
        const two = initElemElement(GOTHIC_A, GOTHIC_SET, 1);
        assert.isFalse(one.isEqual(two));
      });
      it('returns false when tertiary differs', () => {
        const one = initElemElement(GOTHIC_A, GOTHIC_SET, 0, 0);
        const two = initElemElement(GOTHIC_A, GOTHIC_SET, 0, 1);
        assert.isFalse(one.isEqual(two));
      });
      it('returns false when flags differs', () => {
        const one = initElemElement(GOTHIC_A, GOTHIC_SET, 0, 0, ElemElementFlags.none);
        const two = initElemElement(GOTHIC_A, GOTHIC_SET, 0, 0, ElemElementFlags.type);
        assert.isFalse(one.isEqual(two));
      });
      it('returns true even though uset differs', () => {
        const one = initElemElement(GOTHIC_A, GOTHIC_SET);
        const two = initElemElement(GOTHIC_A, UGARITIC_SET);
        assert.isTrue(one.isEqual(two));
      });
    });
  });
  describe('Test of ElementString', () => {
    describe('Test of fromStrings()', () => {
      beforeEach(() => {
        ElementParser.segment = (str: string): ElementSegment[] => {
          return [...str].map(s => new ElementSegment(s, ElementType.codepoint));
        }
      });
      afterEach(() => {
        ElementParser.segment = origElementParserSegment;
      });
      it('returns an empty ElementString if source is null', () => {
        const es = ElementString.fromStrings({}, null);
        assert.deepEqual(es, new ElementString());
      });
      it('can create an ElementString from a string array', () => {
        const sections = { strs: new Strs() };
        sections.strs.allocString = stubStrsAllocString_Char;
        const actual = ElementString.fromStrings(sections, ["𐌰", "𐌱", "𐌲"]);
        const expected = [
          initElemElement(GOTHIC_A),
          initElemElement(GOTHIC_B),
          initElemElement(GOTHIC_C),
        ];
        assert.deepEqual(actual, expected);
      });
    });
    it('can create an ElementString from a string', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(sections, "𐌰𐌱𐌲");
      const expected = [
        initElemElement(GOTHIC_A),
        initElemElement(GOTHIC_B),
        initElemElement(GOTHIC_C),
      ];
      assert.deepEqual(actual, expected);
    });
    it('can apply order string', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        "1 2 3",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 1),
        initElemElement(GOTHIC_B, undefined, 2),
        initElemElement(GOTHIC_C, undefined, 3),
      ];
      assert.deepEqual(actual, expected);
    });
    it('can apply single order to all', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        "1",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 1),
        initElemElement(GOTHIC_B, undefined, 1),
        initElemElement(GOTHIC_C, undefined, 1),
      ];
      assert.deepEqual(actual, expected);
    });
    it.skip('can handle order string that is too short', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        "1 2",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 1),
        initElemElement(GOTHIC_B, undefined, 2),
        initElemElement(GOTHIC_C, undefined, 0),
      ];
      assert.deepEqual(actual, expected);
    });
    it.skip('can handle non-number in order string', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        "1 A 3",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 1),
        initElemElement(GOTHIC_B, undefined, 0),
        initElemElement(GOTHIC_C, undefined, 3),
      ];
      assert.deepEqual(actual, expected);
    });
    it('can apply tertiary string', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        null,
        "1 2 3",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 0, 1),
        initElemElement(GOTHIC_B, undefined, 0, 2),
        initElemElement(GOTHIC_C, undefined, 0, 3),
      ];
      assert.deepEqual(actual, expected);
    });
    it('can apply single tertiary to all', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        null,
        "1",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 0, 1),
        initElemElement(GOTHIC_B, undefined, 0, 1),
        initElemElement(GOTHIC_C, undefined, 0, 1),
      ];
      assert.deepEqual(actual, expected);
    });
    it.skip('can handle tertiary string that is too short', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        null,
        "1 2",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 0, 1),
        initElemElement(GOTHIC_B, undefined, 0, 2),
        initElemElement(GOTHIC_C, undefined, 0, 0),
      ];
      assert.deepEqual(actual, expected);
    });
    it.skip('can handle non-number in tertiary string', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        null,
        "1 A 3",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 0, 1),
        initElemElement(GOTHIC_B, undefined, 0, 0),
        initElemElement(GOTHIC_C, undefined, 0, 3),
      ];
      assert.deepEqual(actual, expected);
    });
    it('can apply tertiary_base string', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        null,
        null,
        "1 0 1",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 0, 0, ElemElementFlags.tertiary_base),
        initElemElement(GOTHIC_B, undefined, 0, 0, ElemElementFlags.none),
        initElemElement(GOTHIC_C, undefined, 0, 0, ElemElementFlags.tertiary_base),
      ];
      assert.deepEqual(actual, expected);
    });
    it('can apply single tertiary_base to all', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        null,
        null,
        "1",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 0, 0, ElemElementFlags.tertiary_base),
        initElemElement(GOTHIC_B, undefined, 0, 0, ElemElementFlags.tertiary_base),
        initElemElement(GOTHIC_C, undefined, 0, 0, ElemElementFlags.tertiary_base),
      ];
      assert.deepEqual(actual, expected);
    });
    it('can handle tertiary_base string that is too short', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        null,
        null,
        "1 0",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 0, 0, ElemElementFlags.tertiary_base),
        initElemElement(GOTHIC_B, undefined, 0, 0, ElemElementFlags.none),
        initElemElement(GOTHIC_C, undefined, 0, 0, ElemElementFlags.none),
      ];
      assert.deepEqual(actual, expected);
    });
    it('can apply prebase string', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        null,
        null,
        null,
        "1 0 1",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 0, 0, ElemElementFlags.prebase),
        initElemElement(GOTHIC_B, undefined, 0, 0, ElemElementFlags.none),
        initElemElement(GOTHIC_C, undefined, 0, 0, ElemElementFlags.prebase),
      ];
      assert.deepEqual(actual, expected);
    });
    it('can apply single prebase to all', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        null,
        null,
        null,
        "1",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 0, 0, ElemElementFlags.prebase),
        initElemElement(GOTHIC_B, undefined, 0, 0, ElemElementFlags.prebase),
        initElemElement(GOTHIC_C, undefined, 0, 0, ElemElementFlags.prebase),
      ];
      assert.deepEqual(actual, expected);
    });
    it('can handle prebase string that is too short', () => {
      const sections = { strs: new Strs() };
      sections.strs.allocString = stubStrsAllocString_Char;
      const actual = ElementString.fromStrings(
        sections,
        "𐌰𐌱𐌲",
        null,
        null,
        null,
        "1 0",
      );
      const expected = [
        initElemElement(GOTHIC_A, undefined, 0, 0, ElemElementFlags.prebase),
        initElemElement(GOTHIC_B, undefined, 0, 0, ElemElementFlags.none),
        initElemElement(GOTHIC_C, undefined, 0, 0, ElemElementFlags.none),
      ];
      assert.deepEqual(actual, expected);
    });
  });
});

function initElemElement(
  value: StrsItem = GOTHIC_A,
  uset: UsetItem = undefined,
  order: number = 0,
  tertiary: number = 0,
  flags: ElemElementFlags = ElemElementFlags.none,
): ElemElement {
  const ee  = new ElemElement();
  ee.value = value;
  ee.uset = uset;
  ee.order = order;
  ee.tertiary = tertiary;
  ee.flags = flags;
  return ee;
};

function stubStrsAllocString_Char(s?: string, opts?: StrsOptions, sections?: DependencySections): StrsItem {
  return new CharStrsItem(s);
};

