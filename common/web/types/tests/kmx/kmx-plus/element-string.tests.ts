/*
 * Keyman is copyright (C) SIL Global. MIT License.
 * 
 * Created by Dr Mark C. Sinclair on 2024-12-09
 * 
 * Test code for element-string.ts
 */

import 'mocha';
import { assert } from 'chai';
import { constants } from '@keymanapp/ldml-keyboard-constants';
import { ElemElementFlags, ElemElement, ElementString } from '../../../src/kmx/kmx-plus/element-string.js';
import { StrsItem, UsetItem, Strs, StrsOptions, DependencySections, CharStrsItem, Uset } from '../../../src/kmx/kmx-plus/kmx-plus.js';
import { UnicodeSet, UnicodeSetParser } from '../../../src/ldml-keyboard/unicodeset-parser-api.js';
import { ElementParser, ElementSegment, ElementType } from '../../../src/ldml-keyboard/pattern-parser.js';

const GOTHIC_A = new StrsItem("𐌰", 0x10330);
const GOTHIC_B = new StrsItem("𐌱", 0x10331);
const GOTHIC_C = new StrsItem("𐌲", 0x10332);
const GOTHIC_D = new StrsItem("𐌳", 0x10333);
const HI_GOTHIC_A = new StrsItem('\ud800', 0xd800);
const LO_GOTHIC_A = new StrsItem('\udf30', 0xdf30);
const GOTHIC_PATTERN = "[𐌰-𐍊]";
const GOTHIC_STRSITEM = new StrsItem(GOTHIC_PATTERN);
const GOTHIC_SET = new UnicodeSet(GOTHIC_PATTERN, [[0x10330,0x1034A]]);
const GOTHIC_USETITEM = new UsetItem(GOTHIC_SET, GOTHIC_STRSITEM);
const UGARITIC_PATTERN = "[𐎀-𐎟]";
const UGARITIC_STRSITEM = new StrsItem(UGARITIC_PATTERN);
const UGARITIC_SET = new UnicodeSet(UGARITIC_PATTERN, [[0x10380,0x1039F]]);
const UGARITIC_USETITEM = new UsetItem(UGARITIC_SET, UGARITIC_STRSITEM);

class TestUnicodeSetParser implements UnicodeSetParser {
  parseUnicodeSet = (pattern: string, rangeCount: number) : UnicodeSet | null => GOTHIC_SET;
  sizeUnicodeSet = (pattern: string) : number => 1;
};

const origElementParserSegment = ElementParser.segment;
let sections: DependencySections = null;

describe('Test of ElementString file', () => {
  describe('Test of ElemElement', () => {
    describe('Test of isEqual()', () => {
      it('returns true when elems identical', () => {
        const one = initElemElement();
        const two = initElemElement();
        assert.isTrue(one.isEqual(two));
      });
      it.skip('returns true when elems are clones', () => {
        const one = initElemElement(new StrsItem("𐌰", 0x10330));
        const two = initElemElement(new StrsItem("𐌰", 0x10330));
        assert.isTrue(one.isEqual(two));
      });
      it('returns false when value differs', () => {
        const one = initElemElement(GOTHIC_A);
        const two = initElemElement(GOTHIC_B);
        assert.isFalse(one.isEqual(two));
      });
      it('returns false when order differs', () => {
        const one = initElemElement(GOTHIC_A, GOTHIC_USETITEM, 0);
        const two = initElemElement(GOTHIC_A, GOTHIC_USETITEM, 1);
        assert.isFalse(one.isEqual(two));
      });
      it('returns false when tertiary differs', () => {
        const one = initElemElement(GOTHIC_A, GOTHIC_USETITEM, 0, 0);
        const two = initElemElement(GOTHIC_A, GOTHIC_USETITEM, 0, 1);
        assert.isFalse(one.isEqual(two));
      });
      it('returns false when flags differs', () => {
        const one = initElemElement(GOTHIC_A, GOTHIC_USETITEM, 0, 0, ElemElementFlags.none);
        const two = initElemElement(GOTHIC_A, GOTHIC_USETITEM, 0, 0, ElemElementFlags.type);
        assert.isFalse(one.isEqual(two));
      });
      it('returns true even though uset differs', () => {
        const one = initElemElement(GOTHIC_A, GOTHIC_USETITEM);
        const two = initElemElement(GOTHIC_A, UGARITIC_USETITEM);
        assert.isTrue(one.isEqual(two));
      });
    });
  });
  describe('Test of ElementString class', () => {
    beforeEach(() => {
      sections = {
        strs: new Strs(),
        uset: new Uset(),
        usetparser: new TestUnicodeSetParser(),
      };
      ElementParser.segment = stubElementParserSegment_CodePoint;
    });
    afterEach(() => {
      ElementParser.segment = origElementParserSegment;
    });
    describe('Test of fromStrings()', () => {
      it('returns an empty ElementString if source is null', () => {
        const es = ElementString.fromStrings({}, null);
        assert.deepEqual(es, new ElementString());
      });
      it('can create an ElementString from a string array', () => {
        sections.strs.allocString = stubStrsAllocString_Char;
        const actual = ElementString.fromStrings(sections, ["𐌰", "𐌱", "𐌲"]);
        const expected = [
          initElemElement(GOTHIC_A),
          initElemElement(GOTHIC_B),
          initElemElement(GOTHIC_C),
        ];
        assert.deepEqual(actual, expected);
      });
      it('can create an ElementString from a string', () => {
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
      it('can handle order string that is too short', () => {
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
      it('can handle non-number in order string', () => {
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
      it('can handle tertiary string that is too short', () => {
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
      it('can handle non-number in tertiary string', () => {
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
      it('can create an ElementString from a uset string', () => {
        ElementParser.segment = stubElementParserSegment_Uset;
        sections.strs.allocString = stubStrsAllocString_Str;
        sections.uset.allocUset = stubUsetAllocUset;
        const actual = ElementString.fromStrings(sections, "[𐌰-𐍊]");
        const expected = [
          initElemElement(
            new StrsItem(''),
            GOTHIC_USETITEM,
            0,
            0,
            constants.elem_flags_type_uset,
          ),
        ];
        assert.deepEqual(actual, expected);
      });
      it('returns null for an invalid unicode set size', () => {
        ElementParser.segment = stubElementParserSegment_Uset;
        sections.usetparser.sizeUnicodeSet = (pattern: string) : number => -1;
        assert.isNull(ElementString.fromStrings(sections, "[𐌰-𐍊]"));
      });
      it('returns null if it cannot parse the unicode set', () => {
        ElementParser.segment = stubElementParserSegment_Uset;
        sections.usetparser.parseUnicodeSet = (pattern: string, rangeCount: number) : UnicodeSet | null => null;
        assert.isNull(ElementString.fromStrings(sections, "[𐌰-𐍊]"));
      });
      it('can handle quad strings', () => {
        sections.strs.allocString = stubStrsAllocString_Char;
        ElementParser.segment = stubElementParserSegment_Escaped;
        const actual   = ElementString.fromStrings(sections, "\\ud800\\udf30");
        const expected = [
          initElemElement(HI_GOTHIC_A),
          initElemElement(LO_GOTHIC_A),
        ];
        assert.deepEqual(actual, expected);
      });
      it('can handle ElemElement of string type', () => {
        sections.strs.allocString = stubStrsAllocString_Str;
        const actual   = ElementString.fromStrings(sections, ["𐌰𐌱𐌲",]);
        const expected = [
          initElemElement(
            new StrsItem("𐌰𐌱𐌲"),
            undefined,
            0,
            0,
            constants.elem_flags_type_str,
          ),
        ];
        assert.deepEqual(actual, expected);
      });
    });
    describe('Test of isEqual()', () => {
      it('returns true when ElementStrings are identical', () => {
        const es = initElementString([
          initElemElement(GOTHIC_A),
          initElemElement(GOTHIC_B),
          initElemElement(GOTHIC_C),
        ]);
        assert.isTrue(es.isEqual(es));
      });
      it.skip('returns true when ElementStrings are clones', () => {
        const one = initElementString([
          initElemElement(GOTHIC_A),
          initElemElement(GOTHIC_B),
          initElemElement(GOTHIC_C),
        ]); 
        const two = initElementString([
          initElemElement(GOTHIC_A),
          initElemElement(GOTHIC_B),
          initElemElement(GOTHIC_C),
        ]); 
        assert.isTrue(one.isEqual(two));
      });
      it('returns false when ElementStrings are different lengths', () => {
        const one = initElementString([
          initElemElement(GOTHIC_A),
          initElemElement(GOTHIC_B),
          initElemElement(GOTHIC_C),
        ]); 
        const two = initElementString([
          initElemElement(GOTHIC_A),
          initElemElement(GOTHIC_B),
        ]); 
        assert.isFalse(one.isEqual(two));
      });
      it('returns false when ElementStrings have different ElemElements', () => {
        const one = initElementString([
          initElemElement(GOTHIC_A),
          initElemElement(GOTHIC_B),
          initElemElement(GOTHIC_C),
        ]); 
        const two = initElementString([
          initElemElement(GOTHIC_A),
          initElemElement(GOTHIC_B),
          initElemElement(GOTHIC_D),
        ]); 
        assert.isFalse(one.isEqual(two));
      });
    });
    describe('Test of parseIntOrZero()', () => {
      it('returns a number for a valid string', () => {
        const num = ElementString['parseIntOrZero']('1');
        assert.equal(num, 1);
      });
      it('returns zero for an invalid string', () => {
        const num = ElementString['parseIntOrZero']('A');
        assert.equal(num, 0);
      });
      it('returns zero for undefined', () => {
        const num = ElementString['parseIntOrZero'](undefined);
        assert.equal(num, 0);
      });
      it('returns zero for a null string', () => {
        const num = ElementString['parseIntOrZero'](null);
        assert.equal(num, 0);
      });
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

function initElementString(elemElements: ElemElement[]): ElementString {
  const es: ElementString = new ElementString();
  elemElements.forEach((ee) => {es.push(ee)});
  return es;
};

function stubStrsAllocString_Char(s?: string, opts?: StrsOptions, sections?: DependencySections): StrsItem {
  return new CharStrsItem(s);
};

function stubStrsAllocString_Str(s?: string, opts?: StrsOptions, sections?: DependencySections): StrsItem {
  return new StrsItem(s);
};

function stubElementParserSegment_CodePoint(str: string): ElementSegment[] {
  return [...str].map(s => new ElementSegment(s, ElementType.codepoint));
};

function stubElementParserSegment_Uset(str: string): ElementSegment[] {
  return [new ElementSegment(str, ElementType.uset)];
};

function stubElementParserSegment_Escaped(str: string): ElementSegment[] {
  const strs = str.match(/\\u[0-9a-fA-F]{4}/g);
  return strs.map((s) => new ElementSegment(s, ElementType.escaped));
};

function stubUsetAllocUset(set: UnicodeSet, sections: DependencySections) : UsetItem {
  return new UsetItem(set, new StrsItem(set.pattern));
};

