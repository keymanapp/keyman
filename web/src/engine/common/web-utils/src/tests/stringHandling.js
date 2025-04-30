import { assert } from 'chai';
// Trigger the String-extending methods.
import * as utils from '@keymanapp/web-utils';

describe('Unicode string handling', () => {
  describe('disabled: BMP-mode, BMP-only strings',  () => {
    before(() => {
      String.kmwEnableSupplementaryPlane(false);
    });

    it('_kmwCharAt', () => {
      const str = "string";
      assert.equal(str._kmwCharAt(-1), '');
      assert.equal(str._kmwCharAt(6), '');
      assert.equal(str._kmwCharAt(1000), '');
      assert.equal(str._kmwCharAt(0), "s");
      assert.equal(str._kmwCharAt(2), "r");
      assert.equal(str._kmwCharAt(5), "g");
    });

    it('_kmwCharCodeAt', () => {
      const str = "string"
      assert.isNaN(str._kmwCharCodeAt(-1));
      assert.isNaN(str._kmwCharCodeAt(6));
      assert.isNaN(str._kmwCharCodeAt(1000));
      assert.equal(str._kmwCharCodeAt(0), "s".charCodeAt(0));
      assert.equal(str._kmwCharCodeAt(2), "r".charCodeAt(0));
      assert.equal(str._kmwCharCodeAt(5), "g".charCodeAt(0));
    });

    it('_kmwIndexOf', () => {
      const banana_nation = "banana nation";
      assert.equal(banana_nation._kmwIndexOf("XYZ"), -1);
      assert.equal(banana_nation._kmwIndexOf("na"), 2);
      assert.equal(banana_nation._kmwIndexOf("na", -1), 2);
      assert.equal(banana_nation._kmwIndexOf("na", 0), 2);
      assert.equal(banana_nation._kmwIndexOf("na", 2), 2);
      assert.equal(banana_nation._kmwIndexOf("na", 3), 4);
      assert.equal(banana_nation._kmwIndexOf("na", 5), 7);
      assert.equal(banana_nation._kmwIndexOf("na", 8), -1);
      assert.equal(banana_nation._kmwIndexOf("na", 1000), -1);
      assert.equal(banana_nation._kmwIndexOf("ba", 0), 0);
      assert.equal(banana_nation._kmwIndexOf("ba", -1), 0);
      assert.equal(banana_nation._kmwIndexOf("on", 11), 11);
      assert.equal(banana_nation._kmwIndexOf("on", 12), -1);
      assert.equal(banana_nation._kmwIndexOf("on", 1000), -1);
    });

    it('_kmwLastIndexOf', () => {
      const banana_nation = "banana nation";
      assert.equal(banana_nation._kmwLastIndexOf("XYZ"), -1);
      assert.equal(banana_nation._kmwLastIndexOf("na"), 7);
      assert.equal(banana_nation._kmwLastIndexOf("na", -1), -1);
      assert.equal(banana_nation._kmwLastIndexOf("na", 0), -1);
      assert.equal(banana_nation._kmwLastIndexOf("na", 2), 2);
      assert.equal(banana_nation._kmwLastIndexOf("na", 3), 2);
      assert.equal(banana_nation._kmwLastIndexOf("na", 5), 4);
      assert.equal(banana_nation._kmwLastIndexOf("na", 8), 7);
      assert.equal(banana_nation._kmwLastIndexOf("na", 1000), 7);
      assert.equal(banana_nation._kmwLastIndexOf("ba", 0), 0);
      assert.equal(banana_nation._kmwLastIndexOf("ba", -1), 0);
      assert.equal(banana_nation._kmwLastIndexOf("on", 12), 11);
      assert.equal(banana_nation._kmwLastIndexOf("on", 11), 11);
      assert.equal(banana_nation._kmwLastIndexOf("on", 10), -1);
    });

    it('_kmwLength', () => {
      assert.equal(""._kmwLength(), 0);
      assert.equal("string"._kmwLength(), 6);
      // From the current MDN example:
      assert.equal("Life, the universe and everything. Answer:"._kmwLength(), 42);
    });

    it('_kmwSlice', () => {
      // From https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice
      const str = "The quick brown fox jumps over the lazy dog.";

      assert.equal(str._kmwSlice(31), "the lazy dog.");
      assert.equal(str._kmwSlice(4, 19), "quick brown fox");
      assert.equal(str._kmwSlice(-4), "dog.");
      assert.equal(str._kmwSlice(-9, -5), "lazy");
      assert.equal(str._kmwSlice(), str);

      // Per documented spec for slice...
      assert.equal(str._kmwSlice(31, -5), "the lazy"); // " dog." = 5 chars
      assert.equal(str._kmwSlice(-5, -9), "");
    });

    it('_kmwSubstr', () => {
      const str = "The quick brown fox jumps over the lazy dog.";

      assert.equal(str._kmwSubstr(1000), '');
      assert.equal(str._kmwSubstr(31), "the lazy dog.");
      assert.equal(str._kmwSubstr(-4), "dog.");
      // Standard string substr can handle this, but _kmwBMPSubstr doesn't handle it correctly!
      // assert.equal(str._kmwSubstr(-4, 2), "do");
      assert.equal(str._kmwSubstr(31, 31), "the lazy dog.");
      assert.equal(str._kmwSubstr(31, -5), ''); // The big difference from .slice:  length must be non-negative.
    });

    it('_kmwSubstring', () => {
      const str = "The quick brown fox jumps over the lazy dog.";

      assert.equal(str._kmwSubstring(1000), '');
      assert.equal(str._kmwSubstring(31), "the lazy dog.");
      assert.equal(str._kmwSubstring(31, 1000), "the lazy dog.");
      assert.equal(str._kmwSubstring(4, 9), "quick");

      // negatives are coerced to 0 & parameters are used in order of ascending value.
      assert.equal(str._kmwSubstring(9, 4), "quick");
      assert.equal(str._kmwSubstring(31, -4), str.substring(0, 31));
      assert.equal(str._kmwSubstring(-5, 9), str.substring(0, 9));
      assert.equal(str._kmwSubstring(-9, -5), '');
    });

    it('kmwCodePointToCodeUnit', () => {
      const str = "The quick brown fox jumps over the lazy dog.";
      assert.equal(str.kmwCodePointToCodeUnit(2), 2);
      assert.equal(str.kmwCodePointToCodeUnit(9), 9);
      assert.equal(str.kmwCodePointToCodeUnit(31), 31);
      assert.equal(str.kmwCodePointToCodeUnit(-4), 40);
    });

    it('kmwCodeUnitToCodePoint', () => {
      const str = "The quick brown fox jumps over the lazy dog.";
      assert.equal(str.kmwCodeUnitToCodePoint(2), 2);
      assert.equal(str.kmwCodeUnitToCodePoint(9), 9);
      assert.equal(str.kmwCodeUnitToCodePoint(31), 31);
      // Wait, why the wild functionality mismatch in contrast to kmwCodePointToCodeUnit?
      assert.equal(str.kmwCodeUnitToCodePoint(-2), 2);
    });
  });

  describe('enabled: SMP-mode, but BMP-only strings',  () => {
    before(() => {
      String.kmwEnableSupplementaryPlane(true);
    });

    it('_kmwCharAt', () => {
      const str = "string";
      assert.equal(str._kmwCharAt(-1), '');
      assert.equal(str._kmwCharAt(6), '');
      assert.equal(str._kmwCharAt(1000), '');
      assert.equal(str._kmwCharAt(0), "s");
      assert.equal(str._kmwCharAt(2), "r");
      assert.equal(str._kmwCharAt(5), "g");
    });

    it('_kmwCharCodeAt', () => {
      const str = "string";
      assert.isNaN(str._kmwCharCodeAt(-1));
      assert.isNaN(str._kmwCharCodeAt(6));
      assert.isNaN(str._kmwCharCodeAt(1000));
      assert.equal(str._kmwCharCodeAt(0), "s".charCodeAt(0));
      assert.equal(str._kmwCharCodeAt(2), "r".charCodeAt(0));
      assert.equal(str._kmwCharCodeAt(5), "g".charCodeAt(0));
    });

    it('_kmwIndexOf', () => {
      const banana_nation = "banana nation";
      assert.equal(banana_nation._kmwIndexOf("XYZ"), -1);
      assert.equal(banana_nation._kmwIndexOf("na"), 2);
      assert.equal(banana_nation._kmwIndexOf("na", -1), 2);
      assert.equal(banana_nation._kmwIndexOf("na", 0), 2);
      assert.equal(banana_nation._kmwIndexOf("na", 2), 2);
      assert.equal(banana_nation._kmwIndexOf("na", 3), 4);
      assert.equal(banana_nation._kmwIndexOf("na", 5), 7);
      assert.equal(banana_nation._kmwIndexOf("na", 8), -1);
      assert.equal(banana_nation._kmwIndexOf("na", 1000), -1);
      assert.equal(banana_nation._kmwIndexOf("ba", 0), 0);
      assert.equal(banana_nation._kmwIndexOf("ba", -1), 0);
      assert.equal(banana_nation._kmwIndexOf("on", 11), 11);
      assert.equal(banana_nation._kmwIndexOf("on", 12), -1);
      assert.equal(banana_nation._kmwIndexOf("on", 1000), -1);
    });

    it('_kmwLastIndexOf', () => {
      const banana_nation = "banana nation";
      assert.equal(banana_nation._kmwLastIndexOf("XYZ"), -1);
      assert.equal(banana_nation._kmwLastIndexOf("na"), 7);
      assert.equal(banana_nation._kmwLastIndexOf("na", -1), -1);
      assert.equal(banana_nation._kmwLastIndexOf("na", 0), -1);
      assert.equal(banana_nation._kmwLastIndexOf("na", 2), 2);
      assert.equal(banana_nation._kmwLastIndexOf("na", 3), 2);
      assert.equal(banana_nation._kmwLastIndexOf("na", 5), 4);
      assert.equal(banana_nation._kmwLastIndexOf("na", 8), 7);
      assert.equal(banana_nation._kmwLastIndexOf("na", 1000), 7);
      assert.equal(banana_nation._kmwLastIndexOf("ba", 0), 0);
      assert.equal(banana_nation._kmwLastIndexOf("ba", -1), 0);
      assert.equal(banana_nation._kmwLastIndexOf("on", 12), 11);
      assert.equal(banana_nation._kmwLastIndexOf("on", 11), 11);
      assert.equal(banana_nation._kmwLastIndexOf("on", 10), -1);
    });

    it('_kmwLength', () => {
      assert.equal(""._kmwLength(), 0);
      assert.equal("string"._kmwLength(), 6);
      // From the current MDN example:
      assert.equal("Life, the universe and everything. Answer:"._kmwLength(), 42);
    });

    it('_kmwSlice', () => {
      // From https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice
      const str = "The quick brown fox jumps over the lazy dog.";

      // FAILS:  does not handle `undefined` length like the real .slice() method.
      // assert.equal(str._kmwSlice(31), "the lazy dog.");
      assert.equal(str._kmwSlice(4, 19), "quick brown fox");
      // FAILS:  does not handle `undefined` length like the real .slice() method.
      // assert.equal(str._kmwSlice(-4), "dog.");
      assert.equal(str._kmwSlice(-9, -5), "lazy");
      // FAILS:  does not handle `undefined` start like the real .slice() method.
      // assert.equal(str._kmwSlice(), str);

      // Per documented spec for slice...
      assert.equal(str._kmwSlice(31, -5), "the lazy"); // " dog." = 5 chars
      assert.equal(str._kmwSlice(-5, -9), "");
    });

    it('_kmwSubstr', () => {
      const str = "The quick brown fox jumps over the lazy dog.";

      assert.equal(str._kmwSubstr(1000), '');
      assert.equal(str._kmwSubstr(31), "the lazy dog.");
      assert.equal(str._kmwSubstr(-4), "dog.");
      assert.equal(str._kmwSubstr(-4, 2), "do");
      assert.equal(str._kmwSubstr(31, 31), "the lazy dog.");
      assert.equal(str._kmwSubstr(31, -5), ''); // The big difference from .slice:  length must be non-negative.
    });

    it('_kmwSubstring', () => {
      const str = "The quick brown fox jumps over the lazy dog.";

      // FAILS - doesn't coerce the out-of-bounds value.
      // assert.equal(str._kmwSubstring(1000), '');
      assert.equal(str._kmwSubstring(31), "the lazy dog.");
      assert.equal(str._kmwSubstring(31, 1000), "the lazy dog.");
      assert.equal(str._kmwSubstring(4, 9), "quick");

      // negatives are coerced to 0 & parameters are used in order of ascending value.
      assert.equal(str._kmwSubstring(9, 4), "quick");
      // FAILS:  these do not handle negative-valued inputs the same way the real .substring does.
      // assert.equal(str._kmwSubstring(31, -4), str.substring(0, 31));
      // assert.equal(str._kmwSubstring(-5, 9), str.substring(0, 9));
      // assert.equal(str._kmwSubstring(-9, -5), '');
    });

    it('kmwCodePointToCodeUnit', () => {
      const str = "The quick brown fox jumps over the lazy dog.";
      assert.equal(str.kmwCodePointToCodeUnit(2), 2);
      assert.equal(str.kmwCodePointToCodeUnit(9), 9);
      assert.equal(str.kmwCodePointToCodeUnit(31), 31);
      assert.equal(str.kmwCodePointToCodeUnit(-4), 40);
      // This certainly doesn't seem right... for now, we're just verifying existing behaviors.
      assert.equal(str.kmwCodePointToCodeUnit(1000), null);
    });

    it('kmwCodeUnitToCodePoint', () => {
      const str = "The quick brown fox jumps over the lazy dog.";
      assert.equal(str.kmwCodeUnitToCodePoint(2), 2);
      assert.equal(str.kmwCodeUnitToCodePoint(9), 9);
      assert.equal(str.kmwCodeUnitToCodePoint(31), 31);
      // Again, odd mismatch in contrast to kmwCodePointToCodeUnit
      assert.equal(str.kmwCodeUnitToCodePoint(-2), 2);
    });
  });


  describe('enabled: SMP-mode, with mixed-plane strings',  () => {
    before(() => {
      String.kmwEnableSupplementaryPlane(true);
    });

    it('_kmwCharAt', () => {
      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      const apples = 'a' + String.fromCodePoint(0x1d5c9) + 'p' + 'l' + String.fromCodePoint(0x1d5be) + 's';
      assert.equal(apples._kmwCharAt(-1), '');
      assert.equal(apples._kmwCharAt(6), '');
      assert.equal(apples._kmwCharAt(1000), '');
      assert.equal(apples._kmwCharAt(0), "a");
      assert.equal(apples._kmwCharAt(1), String.fromCodePoint(0x1d5c9));
      assert.equal(apples._kmwCharAt(2), "p");
      assert.equal(apples._kmwCharAt(5), "s");
    });

    it('_kmwCharCodeAt', () => {
      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      const apples = 'a' + String.fromCodePoint(0x1d5c9) + 'p' + 'l' + String.fromCodePoint(0x1d5be) + 's';
      assert.isNaN(apples._kmwCharCodeAt(-1));
      assert.isNaN(apples._kmwCharCodeAt(6));
      assert.isNaN(apples._kmwCharCodeAt(1000));
      assert.equal(apples._kmwCharCodeAt(0), "a".charCodeAt(0));
      assert.equal(apples._kmwCharCodeAt(1), 0x1d5c9);
      assert.equal(apples._kmwCharCodeAt(2), "p".charCodeAt(0));
      assert.equal(apples._kmwCharCodeAt(5), "s".charCodeAt(0));
    });

    it('_kmwIndexOf', () => {
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      const a = String.fromCodePoint(0x1d5ba);
      const banana_nation = "ban" + a + "na n" + a + "tion";
      assert.equal(banana_nation._kmwIndexOf("XYZ"), -1);
      assert.equal(banana_nation._kmwIndexOf("na"), 4);
      assert.equal(banana_nation._kmwIndexOf("na", -1), 4);
      assert.equal(banana_nation._kmwIndexOf("na", 0), 4);
      assert.equal(banana_nation._kmwIndexOf("na", 2), 4);
      assert.equal(banana_nation._kmwIndexOf("na", 3), 4);
      assert.equal(banana_nation._kmwIndexOf("na", 4), 4);
      // ERROR:  returns 4, not -1 as it should!
      // assert.equal(banana_nation._kmwIndexOf("na", 5), -1);
      assert.equal(banana_nation._kmwIndexOf("na", 8), -1);
      assert.equal(banana_nation._kmwIndexOf("na", 1000), -1);

      const na = "n" + a;
      assert.equal(banana_nation._kmwIndexOf(na), 2);
      assert.equal(banana_nation._kmwIndexOf(na, -1), 2);
      assert.equal(banana_nation._kmwIndexOf(na, 0), 2);
      assert.equal(banana_nation._kmwIndexOf(na, 2), 2);
      assert.equal(banana_nation._kmwIndexOf(na, 3), 7);
      assert.equal(banana_nation._kmwIndexOf(na, 5), 7);
      // ERROR:  returns 7 (!)
      // assert.equal(banana_nation._kmwIndexOf(na, 8), -1);
      assert.equal(banana_nation._kmwIndexOf(na, 1000), -1);
      assert.equal(banana_nation._kmwIndexOf("ba", 0), 0);
      assert.equal(banana_nation._kmwIndexOf("ba", -1), 0);
      assert.equal(banana_nation._kmwIndexOf("on", 11), 11);
      // ERROR:  returns 11 (!)
      // assert.equal(banana_nation._kmwIndexOf("on", 12), -1);
      assert.equal(banana_nation._kmwIndexOf("on", 1000), -1);
    });

    it('_kmwLastIndexOf', () => {
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      const a = String.fromCodePoint(0x1d5ba);
      const banana_nation = "ban" + a + "na n" + a + "tion";
      assert.equal(banana_nation._kmwLastIndexOf("XYZ"), -1);
      assert.equal(banana_nation._kmwLastIndexOf("na"), 4);
      assert.equal(banana_nation._kmwLastIndexOf("na", -1), -1);
      assert.equal(banana_nation._kmwLastIndexOf("na", 0), -1);
      assert.equal(banana_nation._kmwLastIndexOf("na", 2), -1);
      assert.equal(banana_nation._kmwLastIndexOf("na", 3), -1);

      // ERROR:  Can't find the instance at the starting index?
      // assert.equal(banana_nation._kmwLastIndexOf("na", 4), 4);
      assert.equal(banana_nation._kmwLastIndexOf("na", 5), 4);
      assert.equal(banana_nation._kmwLastIndexOf("na", 8), 4);
      assert.equal(banana_nation._kmwLastIndexOf("na", 1000), 4);

      const na = "n" + a;
      assert.equal(banana_nation._kmwLastIndexOf(na), 7);
      assert.equal(banana_nation._kmwLastIndexOf(na, -1), -1);
      assert.equal(banana_nation._kmwLastIndexOf(na, 0), -1);
      assert.equal(banana_nation._kmwLastIndexOf(na, 2), 2);
      assert.equal(banana_nation._kmwLastIndexOf(na, 3), 2);
      assert.equal(banana_nation._kmwLastIndexOf(na, 5), 2);
      // ERROR:  Inconsistent with the index-2 case above.
      // assert.equal(banana_nation._kmwLastIndexOf(na, 7), 7);
      assert.equal(banana_nation._kmwLastIndexOf(na, 8), 7);
      assert.equal(banana_nation._kmwLastIndexOf(na, 1000), 7);
      assert.equal(banana_nation._kmwLastIndexOf("ba", 0), 0);
      assert.equal(banana_nation._kmwLastIndexOf("ba", -1), 0);
      // ERROR:  Both return -1 instead!
      // assert.equal(banana_nation._kmwLastIndexOf("on", 12), 11);
      // assert.equal(banana_nation._kmwLastIndexOf("on", 11), 11);
      assert.equal(banana_nation._kmwLastIndexOf("on", 10), -1);
    });

    it('_kmwLength', () => {
      assert.equal(""._kmwLength(), 0);
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      const a = String.fromCodePoint(0x1d5ba);
      const banana_nation = "ban" + a + "na n" + a + "tion";
      assert.equal(banana_nation._kmwLength(), 13);

      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      const apples = 'a' + String.fromCodePoint(0x1d5c9) + 'p' + 'l' + String.fromCodePoint(0x1d5be) + 's';
      assert.equal(apples._kmwLength(), 6);
    });

    it('_kmwSlice', () => {
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      // 0x1d5c2: MATHEMATICAL SANS-SERIF SMALL i
      const a = String.fromCodePoint(0x1d5ba);
      const p = String.fromCodePoint(0x1d5c9);
      const e = String.fromCodePoint(0x1d5be);
      const i = String.fromCodePoint(0x1d5c2);

      // From https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice
      const str = "Th" + e + " qu" + i + "ck brown fox jum" + p + "s over the l" + a + "zy dog.";

      // FAILS:  does not handle `undefined` length like the real .slice() method.
      // assert.equal(str._kmwSlice(31), "the l" + a + "zy dog.");
      assert.equal(str._kmwSlice(4, 19), "qu" + i + "ck brown fox");
      // FAILS:  does not handle `undefined` length like the real .slice() method.
      // assert.equal(str._kmwSlice(-4), "dog.");
      assert.equal(str._kmwSlice(-9, -5), "l" + a + "zy");
      // FAILS:  does not handle `undefined` start like the real .slice() method.
      // assert.equal(str._kmwSlice(), str);

      // Per documented spec for slice...
      assert.equal(str._kmwSlice(31, -5), "the l" + a + "zy"); // " dog." = 5 chars
      assert.equal(str._kmwSlice(-5, -9), "");
    });

    it('_kmwSubstr', () => {
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      // 0x1d5c2: MATHEMATICAL SANS-SERIF SMALL i
      const a = String.fromCodePoint(0x1d5ba);
      const p = String.fromCodePoint(0x1d5c9);
      const e = String.fromCodePoint(0x1d5be);
      const i = String.fromCodePoint(0x1d5c2);

      const str = "Th" + e + " qu" + i + "ck brown fox jum" + p + "s over the l" + a + "zy dog.";

      assert.equal(str._kmwSubstr(1000), '');
      assert.equal(str._kmwSubstr(31), "the l" + a + "zy dog.");
      assert.equal(str._kmwSubstr(-4), "dog.");
      assert.equal(str._kmwSubstr(-9, 4), "l" + a + "zy");
      assert.equal(str._kmwSubstr(31, 31), "the l" + a + "zy dog.");
      assert.equal(str._kmwSubstr(31, -5), ''); // The big difference from .slice:  length must be non-negative.
    });

    it('_kmwSubstring', () => {
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      // 0x1d5c2: MATHEMATICAL SANS-SERIF SMALL i
      const a = String.fromCodePoint(0x1d5ba);
      const p = String.fromCodePoint(0x1d5c9);
      const e = String.fromCodePoint(0x1d5be);
      const i = String.fromCodePoint(0x1d5c2);

      const str = "Th" + e + " qu" + i + "ck brown fox jum" + p + "s over the l" + a + "zy dog.";

      // FAILS - doesn't coerce the out-of-bounds value.
      // assert.equal(str._kmwSubstring(1000), '');
      assert.equal(str._kmwSubstring(31), "the l" + a + "zy dog.");
      assert.equal(str._kmwSubstring(31, 1000), "the l" + a + "zy dog.");
      assert.equal(str._kmwSubstring(4, 9), "qu" + i + "ck");

      // negatives are coerced to 0 & parameters are used in order of ascending value.
      assert.equal(str._kmwSubstring(9, 4), "qu" + i + "ck");
      // FAILS:  these do not handle negative-valued inputs the same way the real .substring does.
      // assert.equal(str._kmwSubstring(31, -4), str.substring(0, 31));
      // assert.equal(str._kmwSubstring(-5, 9), str.substring(0, 9));
      // assert.equal(str._kmwSubstring(-9, -5), '');
    });

    it('kmwCodePointToCodeUnit', () => {
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      // 0x1d5c2: MATHEMATICAL SANS-SERIF SMALL i
      const a = String.fromCodePoint(0x1d5ba);
      const p = String.fromCodePoint(0x1d5c9);
      const e = String.fromCodePoint(0x1d5be);
      const i = String.fromCodePoint(0x1d5c2);

      const str = "Th" + e + " qu" + i + "ck brown fox jum" + p + "s over the l" + a + "zy dog.";

      assert.equal(str.kmwCodePointToCodeUnit(2), 2);
      assert.equal(str.kmwCodePointToCodeUnit(9), 11); // "The quick" = 9; 2 SMP replacements before then.
      assert.equal(str.kmwCodePointToCodeUnit(31), 34); // "the lazy dog." is the remnant; 3 SMP replacements before then.
      assert.equal(str.kmwCodePointToCodeUnit(-13), 34); // "the lazy dog." is the remnant, 13 codepoints long.
    });

    it('kmwCodeUnitToCodePoint', () => {
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      // 0x1d5c2: MATHEMATICAL SANS-SERIF SMALL i
      const a = String.fromCodePoint(0x1d5ba);
      const p = String.fromCodePoint(0x1d5c9);
      const e = String.fromCodePoint(0x1d5be);
      const i = String.fromCodePoint(0x1d5c2);

      const str = "Th" + e + " qu" + i + "ck brown fox jum" + p + "s over the l" + a + "zy dog.";

      // Inverting the tests from the prior section on `kmwCodePointToCodeUnit`...
      assert.equal(str.kmwCodeUnitToCodePoint(2), 2);
      assert.equal(str.kmwCodeUnitToCodePoint(11), 9); // "The quick" = 9; 2 SMP replacements before then.
      assert.equal(str.kmwCodeUnitToCodePoint(34), 31); // "the lazy dog" is the remnant; 3 SMP replacements before then.
      // Clearly needs work in the future; only verifying behavioral stability for now.
      assert.equal(str.kmwCodeUnitToCodePoint(-2), 2);
    });
  });
});