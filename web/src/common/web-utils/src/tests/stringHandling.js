import { assert } from 'chai';
// Trigger the String-extending methods.
import { KMWString } from 'keyman/common/web-utils';

describe('Unicode string handling', () => {
  describe('disabled: BMP-mode, BMP-only strings',  () => {
    before(() => {
      KMWString.enableSupplementaryPlane(false);
    });

    it('charAt', () => {
      const str = "string";
      assert.equal(KMWString.charAt(str, -1), '');
      assert.equal(KMWString.charAt(str, 6), '');
      assert.equal(KMWString.charAt(str, 1000), '');
      assert.equal(KMWString.charAt(str, 0), "s");
      assert.equal(KMWString.charAt(str, 2), "r");
      assert.equal(KMWString.charAt(str, 5), "g");
    });

    it('charCodeAt', () => {
      const str = "string"
      assert.isNaN(KMWString.charCodeAt(str, -1));
      assert.isNaN(KMWString.charCodeAt(str, 6));
      assert.isNaN(KMWString.charCodeAt(str, 1000));
      assert.equal(KMWString.charCodeAt(str, 0), "s".charCodeAt(0));
      assert.equal(KMWString.charCodeAt(str, 2), "r".charCodeAt(0));
      assert.equal(KMWString.charCodeAt(str, 5), "g".charCodeAt(0));
    });

    it('indexOf', () => {
      const banana_nation = "banana nation";
      assert.equal(KMWString.indexOf(banana_nation, "XYZ"), -1);
      assert.equal(KMWString.indexOf(banana_nation, "na"), 2);
      assert.equal(KMWString.indexOf(banana_nation, "na", -1), 2);
      assert.equal(KMWString.indexOf(banana_nation, "na", 0), 2);
      assert.equal(KMWString.indexOf(banana_nation, "na", 2), 2);
      assert.equal(KMWString.indexOf(banana_nation, "na", 3), 4);
      assert.equal(KMWString.indexOf(banana_nation, "na", 5), 7);
      assert.equal(KMWString.indexOf(banana_nation, "na", 8), -1);
      assert.equal(KMWString.indexOf(banana_nation, "na", 1000), -1);
      assert.equal(KMWString.indexOf(banana_nation, "ba", 0), 0);
      assert.equal(KMWString.indexOf(banana_nation, "ba", -1), 0);
      assert.equal(KMWString.indexOf(banana_nation, "on", 11), 11);
      assert.equal(KMWString.indexOf(banana_nation, "on", 12), -1);
      assert.equal(KMWString.indexOf(banana_nation, "on", 1000), -1);
    });

    it('lastIndexOf', () => {
      const banana_nation = "banana nation";
      assert.equal(KMWString.lastIndexOf(banana_nation, "XYZ"), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na"), 7);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", -1), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 0), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 2), 2);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 3), 2);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 5), 4);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 8), 7);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 1000), 7);
      assert.equal(KMWString.lastIndexOf(banana_nation, "ba", 0), 0);
      assert.equal(KMWString.lastIndexOf(banana_nation, "ba", -1), 0);
      assert.equal(KMWString.lastIndexOf(banana_nation, "on", 12), 11);
      assert.equal(KMWString.lastIndexOf(banana_nation, "on", 11), 11);
      assert.equal(KMWString.lastIndexOf(banana_nation, "on", 10), -1);
    });

    it('length', () => {
      assert.equal(KMWString.length(""), 0);
      assert.equal(KMWString.length("string"), 6);
      // From the current MDN example:
      assert.equal(KMWString.length("Life, the universe and everything. Answer:"), 42);
    });

    it('slice', () => {
      // From https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice
      const str = "The quick brown fox jumps over the lazy dog.";

      assert.equal(KMWString.slice(str, 31), "the lazy dog.");
      assert.equal(KMWString.slice(str, 4, 19), "quick brown fox");
      assert.equal(KMWString.slice(str, -4), "dog.");
      assert.equal(KMWString.slice(str, -9, -5), "lazy");
      assert.equal(KMWString.slice(str), str);

      // Per documented spec for slice...
      assert.equal(KMWString.slice(str, 31, -5), "the lazy"); // " dog." = 5 chars
      assert.equal(KMWString.slice(str, -5, -9), "");
    });

    it('substr', () => {
      const str = "The quick brown fox jumps over the lazy dog.";

      assert.equal(KMWString.substr(str, 1000), '');
      assert.equal(KMWString.substr(str, 31), "the lazy dog.");
      assert.equal(KMWString.substr(str, -4), "dog.");
      // Standard string substr can handle this, but _kmwBMPSubstr doesn't handle it correctly!
      // assert.equal(KMWString.substr(-4, 2), "do");
      assert.equal(KMWString.substr(str, 31, 31), "the lazy dog.");
      assert.equal(KMWString.substr(str, 31, -5), ''); // The big difference from .slice:  length must be non-negative.
    });

    it('substring', () => {
      const str = "The quick brown fox jumps over the lazy dog.";

      assert.equal(KMWString.substring(str, 1000), '');
      assert.equal(KMWString.substring(str, 31), "the lazy dog.");
      assert.equal(KMWString.substring(str, 31, 1000), "the lazy dog.");
      assert.equal(KMWString.substring(str, 4, 9), "quick");

      // negatives are coerced to 0 & parameters are used in order of ascending value.
      assert.equal(KMWString.substring(str, 9, 4), "quick");
      assert.equal(KMWString.substring(str, 31, -4), str.substring(0, 31));
      assert.equal(KMWString.substring(str, -5, 9), str.substring(0, 9));
      assert.equal(KMWString.substring(str, -9, -5), '');
    });

    it('codePointToCodeUnit', () => {
      const str = "The quick brown fox jumps over the lazy dog.";
      assert.equal(KMWString.codePointToCodeUnit(str, 2), 2);
      assert.equal(KMWString.codePointToCodeUnit(str, 9), 9);
      assert.equal(KMWString.codePointToCodeUnit(str, 31), 31);
      assert.equal(KMWString.codePointToCodeUnit(str, -4), 40);
    });

    it('codeUnitToCodePoint', () => {
      const str = "The quick brown fox jumps over the lazy dog.";
      assert.equal(KMWString.codeUnitToCodePoint(str, 2), 2);
      assert.equal(KMWString.codeUnitToCodePoint(str, 9), 9);
      assert.equal(KMWString.codeUnitToCodePoint(str, 31), 31);
      // Wait, why the wild functionality mismatch in contrast to kmwCodePointToCodeUnit?
      assert.equal(KMWString.codeUnitToCodePoint(str, -2), 2);
    });
  });

  describe('enabled: SMP-mode, but BMP-only strings',  () => {
    before(() => {
      KMWString.enableSupplementaryPlane(true);
    });

    it('charAt', () => {
      const str = "string";
      assert.equal(KMWString.charAt(str, -1), '');
      assert.equal(KMWString.charAt(str, 6), '');
      assert.equal(KMWString.charAt(str, 1000), '');
      assert.equal(KMWString.charAt(str, 0), "s");
      assert.equal(KMWString.charAt(str, 2), "r");
      assert.equal(KMWString.charAt(str, 5), "g");
    });

    it('charCodeAt', () => {
      const str = "string";
      assert.isNaN(KMWString.charCodeAt(str, -1));
      assert.isNaN(KMWString.charCodeAt(str, 6));
      assert.isNaN(KMWString.charCodeAt(str, 1000));
      assert.equal(KMWString.charCodeAt(str, 0), "s".charCodeAt(0));
      assert.equal(KMWString.charCodeAt(str, 2), "r".charCodeAt(0));
      assert.equal(KMWString.charCodeAt(str, 5), "g".charCodeAt(0));
    });

    it('indexOf', () => {
      const banana_nation = "banana nation";
      assert.equal(KMWString.indexOf(banana_nation, "XYZ"), -1);
      assert.equal(KMWString.indexOf(banana_nation, "na"), 2);
      assert.equal(KMWString.indexOf(banana_nation, "na", -1), 2);
      assert.equal(KMWString.indexOf(banana_nation, "na", 0), 2);
      assert.equal(KMWString.indexOf(banana_nation, "na", 2), 2);
      assert.equal(KMWString.indexOf(banana_nation, "na", 3), 4);
      assert.equal(KMWString.indexOf(banana_nation, "na", 5), 7);
      assert.equal(KMWString.indexOf(banana_nation, "na", 8), -1);
      assert.equal(KMWString.indexOf(banana_nation, "na", 1000), -1);
      assert.equal(KMWString.indexOf(banana_nation, "ba", 0), 0);
      assert.equal(KMWString.indexOf(banana_nation, "ba", -1), 0);
      assert.equal(KMWString.indexOf(banana_nation, "on", 11), 11);
      assert.equal(KMWString.indexOf(banana_nation, "on", 12), -1);
      assert.equal(KMWString.indexOf(banana_nation, "on", 1000), -1);
    });

    it('lastIndexOf', () => {
      const banana_nation = "banana nation";
      assert.equal(KMWString.lastIndexOf(banana_nation, "XYZ"), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na"), 7);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", -1), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 0), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 2), 2);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 3), 2);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 5), 4);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 8), 7);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 1000), 7);
      assert.equal(KMWString.lastIndexOf(banana_nation, "ba", 0), 0);
      assert.equal(KMWString.lastIndexOf(banana_nation, "ba", -1), 0);
      assert.equal(KMWString.lastIndexOf(banana_nation, "on", 12), 11);
      assert.equal(KMWString.lastIndexOf(banana_nation, "on", 11), 11);
      assert.equal(KMWString.lastIndexOf(banana_nation, "on", 10), -1);
    });

    it('length', () => {
      assert.equal(KMWString.length(""), 0);
      assert.equal(KMWString.length("string"), 6);
      // From the current MDN example:
      assert.equal(KMWString.length("Life, the universe and everything. Answer:"), 42);
    });

    it('slice', () => {
      // From https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice
      const str = "The quick brown fox jumps over the lazy dog.";

      // FAILS:  does not handle `undefined` length like the real .slice() method.
      // assert.equal(KMWString.slice(str, 31), "the lazy dog.");
      assert.equal(KMWString.slice(str, 4, 19), "quick brown fox");
      // FAILS:  does not handle `undefined` length like the real .slice() method.
      // assert.equal(KMWString.slice(str, -4), "dog.");
      assert.equal(KMWString.slice(str, -9, -5), "lazy");
      // FAILS:  does not handle `undefined` start like the real .slice() method.
      // assert.equal(KMWString.slice(str), str);

      // Per documented spec for slice...
      assert.equal(KMWString.slice(str, 31, -5), "the lazy"); // " dog." = 5 chars
      assert.equal(KMWString.slice(str, -5, -9), "");
    });

    it('substr', () => {
      const str = "The quick brown fox jumps over the lazy dog.";

      assert.equal(KMWString.substr(str, 1000), '');
      assert.equal(KMWString.substr(str, 31), "the lazy dog.");
      assert.equal(KMWString.substr(str, -4), "dog.");
      assert.equal(KMWString.substr(str, -4, 2), "do");
      assert.equal(KMWString.substr(str, 31, 31), "the lazy dog.");
      assert.equal(KMWString.substr(str, 31, -5), ''); // The big difference from .slice:  length must be non-negative.
    });

    it('substring', () => {
      const str = "The quick brown fox jumps over the lazy dog.";

      // FAILS - doesn't coerce the out-of-bounds value.
      // assert.equal(KMWString.substring(str, 1000), '');
      assert.equal(KMWString.substring(str, 31), "the lazy dog.");
      assert.equal(KMWString.substring(str, 31, 1000), "the lazy dog.");
      assert.equal(KMWString.substring(str, 4, 9), "quick");

      // negatives are coerced to 0 & parameters are used in order of ascending value.
      assert.equal(KMWString.substring(str, 9, 4), "quick");
      // FAILS:  these do not handle negative-valued inputs the same way the real .substring does.
      // assert.equal(KMWString.substring(str, 31, -4), str.substring(0, 31));
      // assert.equal(KMWString.substring(str, -5, 9), str.substring(0, 9));
      // assert.equal(KMWString.substring(str, -9, -5), '');
    });

    it('codePointToCodeUnit', () => {
      const str = "The quick brown fox jumps over the lazy dog.";
      assert.equal(KMWString.codePointToCodeUnit(str, 2), 2);
      assert.equal(KMWString.codePointToCodeUnit(str, 9), 9);
      assert.equal(KMWString.codePointToCodeUnit(str, 31), 31);
      assert.equal(KMWString.codePointToCodeUnit(str, -4), 40);
      // This certainly doesn't seem right... for now, we're just verifying existing behaviors.
      assert.equal(KMWString.codePointToCodeUnit(str, 1000), null);
    });

    it('codeUnitToCodePoint', () => {
      const str = "The quick brown fox jumps over the lazy dog.";
      assert.equal(KMWString.codeUnitToCodePoint(str, 2), 2);
      assert.equal(KMWString.codeUnitToCodePoint(str, 9), 9);
      assert.equal(KMWString.codeUnitToCodePoint(str, 31), 31);
      // Again, odd mismatch in contrast to kmwCodePointToCodeUnit
      assert.equal(KMWString.codeUnitToCodePoint(str, -2), 2);
    });
  });


  describe('enabled: SMP-mode, with mixed-plane strings',  () => {
    before(() => {
      KMWString.enableSupplementaryPlane(true);
    });

    it('charAt', () => {
      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      const apples = 'a' + String.fromCodePoint(0x1d5c9) + 'p' + 'l' + String.fromCodePoint(0x1d5be) + 's';
      assert.equal(KMWString.charAt(apples, -1), '');
      assert.equal(KMWString.charAt(apples, 6), '');
      assert.equal(KMWString.charAt(apples, 1000), '');
      assert.equal(KMWString.charAt(apples, 0), "a");
      assert.equal(KMWString.charAt(apples, 1), String.fromCodePoint(0x1d5c9));
      assert.equal(KMWString.charAt(apples, 2), "p");
      assert.equal(KMWString.charAt(apples, 5), "s");
    });

    it('charCodeAt', () => {
      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      const apples = 'a' + String.fromCodePoint(0x1d5c9) + 'p' + 'l' + String.fromCodePoint(0x1d5be) + 's';
      assert.isNaN(KMWString.charCodeAt(apples, -1));
      assert.isNaN(KMWString.charCodeAt(apples, 6));
      assert.isNaN(KMWString.charCodeAt(apples, 1000));
      assert.equal(KMWString.charCodeAt(apples, 0), "a".charCodeAt(0));
      assert.equal(KMWString.charCodeAt(apples, 1), 0x1d5c9);
      assert.equal(KMWString.charCodeAt(apples, 2), "p".charCodeAt(0));
      assert.equal(KMWString.charCodeAt(apples, 5), "s".charCodeAt(0));
    });

    it('indexOf', () => {
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      const a = String.fromCodePoint(0x1d5ba);
      const banana_nation = "ban" + a + "na n" + a + "tion";
      assert.equal(KMWString.indexOf(banana_nation, "XYZ"), -1);
      assert.equal(KMWString.indexOf(banana_nation, "na"), 4);
      assert.equal(KMWString.indexOf(banana_nation, "na", -1), 4);
      assert.equal(KMWString.indexOf(banana_nation, "na", 0), 4);
      assert.equal(KMWString.indexOf(banana_nation, "na", 2), 4);
      assert.equal(KMWString.indexOf(banana_nation, "na", 3), 4);
      assert.equal(KMWString.indexOf(banana_nation, "na", 4), 4);
      // ERROR:  returns 4, not -1 as it should!
      // assert.equal(KMWString.indexOf(banana_nation, "na", 5), -1);
      assert.equal(KMWString.indexOf(banana_nation, "na", 8), -1);
      assert.equal(KMWString.indexOf(banana_nation, "na", 1000), -1);

      const na = "n" + a;
      assert.equal(KMWString.indexOf(banana_nation, na), 2);
      assert.equal(KMWString.indexOf(banana_nation, na, -1), 2);
      assert.equal(KMWString.indexOf(banana_nation, na, 0), 2);
      assert.equal(KMWString.indexOf(banana_nation, na, 2), 2);
      assert.equal(KMWString.indexOf(banana_nation, na, 3), 7);
      assert.equal(KMWString.indexOf(banana_nation, na, 5), 7);
      // ERROR:  returns 7 (!)
      // assert.equal(KMWString.indexOf(banana_nation, na, 8), -1);
      assert.equal(KMWString.indexOf(banana_nation, na, 1000), -1);
      assert.equal(KMWString.indexOf(banana_nation, "ba", 0), 0);
      assert.equal(KMWString.indexOf(banana_nation, "ba", -1), 0);
      assert.equal(KMWString.indexOf(banana_nation, "on", 11), 11);
      // ERROR:  returns 11 (!)
      // assert.equal(KMWString.indexOf(banana_nation, "on", 12), -1);
      assert.equal(KMWString.indexOf(banana_nation, "on", 1000), -1);
    });

    it('lastIndexOf', () => {
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      const a = String.fromCodePoint(0x1d5ba);
      const banana_nation = "ban" + a + "na n" + a + "tion";
      assert.equal(KMWString.lastIndexOf(banana_nation, "XYZ"), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na"), 4);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", -1), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 0), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 2), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 3), -1);

      // ERROR:  Can't find the instance at the starting index?
      // assert.equal(KMWString.lastIndexOf(banana_nation, "na", 4), 4);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 5), 4);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 8), 4);
      assert.equal(KMWString.lastIndexOf(banana_nation, "na", 1000), 4);

      const na = "n" + a;
      assert.equal(KMWString.lastIndexOf(banana_nation, na), 7);
      assert.equal(KMWString.lastIndexOf(banana_nation, na, -1), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, na, 0), -1);
      assert.equal(KMWString.lastIndexOf(banana_nation, na, 2), 2);
      assert.equal(KMWString.lastIndexOf(banana_nation, na, 3), 2);
      assert.equal(KMWString.lastIndexOf(banana_nation, na, 5), 2);
      // ERROR:  Inconsistent with the index-2 case above.
      // assert.equal(KMWString.lastIndexOf(banana_nation, na, 7), 7);
      assert.equal(KMWString.lastIndexOf(banana_nation, na, 8), 7);
      assert.equal(KMWString.lastIndexOf(banana_nation, na, 1000), 7);
      assert.equal(KMWString.lastIndexOf(banana_nation, "ba", 0), 0);
      assert.equal(KMWString.lastIndexOf(banana_nation, "ba", -1), 0);
      // ERROR:  Both return -1 instead!
      // assert.equal(KMWString.lastIndexOf(banana_nation, "on", 12), 11);
      // assert.equal(KMWString.lastIndexOf(banana_nation, "on", 11), 11);
      assert.equal(KMWString.lastIndexOf(banana_nation, "on", 10), -1);
    });

    it('length', () => {
      assert.equal(KMWString.length(""), 0);
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      const a = String.fromCodePoint(0x1d5ba);
      const banana_nation = "ban" + a + "na n" + a + "tion";
      assert.equal(KMWString.length(banana_nation), 13);

      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      const apples = 'a' + String.fromCodePoint(0x1d5c9) + 'p' + 'l' + String.fromCodePoint(0x1d5be) + 's';
      assert.equal(KMWString.length(apples), 6);
    });

    it('slice', () => {
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
      // assert.equal(KMWString.slice(str, 31), "the l" + a + "zy dog.");
      assert.equal(KMWString.slice(str, 4, 19), "qu" + i + "ck brown fox");
      // FAILS:  does not handle `undefined` length like the real .slice() method.
      // assert.equal(KMWString.slice(str, (-4), "dog.");
      assert.equal(KMWString.slice(str, -9, -5), "l" + a + "zy");
      // FAILS:  does not handle `undefined` start like the real .slice() method.
      // assert.equal(KMWString.slice(str, (), str);

      // Per documented spec for slice...
      assert.equal(KMWString.slice(str, 31, -5), "the l" + a + "zy"); // " dog." = 5 chars
      assert.equal(KMWString.slice(str, -5, -9), "");
    });

    it('substr', () => {
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      // 0x1d5c2: MATHEMATICAL SANS-SERIF SMALL i
      const a = String.fromCodePoint(0x1d5ba);
      const p = String.fromCodePoint(0x1d5c9);
      const e = String.fromCodePoint(0x1d5be);
      const i = String.fromCodePoint(0x1d5c2);

      const str = "Th" + e + " qu" + i + "ck brown fox jum" + p + "s over the l" + a + "zy dog.";

      assert.equal(KMWString.substr(str, 1000), '');
      assert.equal(KMWString.substr(str, 31), "the l" + a + "zy dog.");
      assert.equal(KMWString.substr(str, -4), "dog.");
      assert.equal(KMWString.substr(str, -9, 4), "l" + a + "zy");
      assert.equal(KMWString.substr(str, 31, 31), "the l" + a + "zy dog.");
      assert.equal(KMWString.substr(str, 31, -5), ''); // The big difference from .slice:  length must be non-negative.
    });

    it('substring', () => {
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
      // assert.equal(KMWString.substring(str, 1000), '');
      assert.equal(KMWString.substring(str, 31), "the l" + a + "zy dog.");
      assert.equal(KMWString.substring(str, 31, 1000), "the l" + a + "zy dog.");
      assert.equal(KMWString.substring(str, 4, 9), "qu" + i + "ck");

      // negatives are coerced to 0 & parameters are used in order of ascending value.
      assert.equal(KMWString.substring(str, 9, 4), "qu" + i + "ck");
      // FAILS:  these do not handle negative-valued inputs the same way the real .substring does.
      // assert.equal(KMWString.substring(str, 31, -4), str.substring(0, 31));
      // assert.equal(KMWString.substring(str, -5, 9), str.substring(0, 9));
      // assert.equal(KMWString.substring(str, -9, -5), '');
    });

    it('codePointToCodeUnit', () => {
      // 0x1d5ba: MATHEMATICAL SANS-SERIF SMALL a
      // 0x1d5c9: MATHEMATICAL SANS-SERIF SMALL p
      // 0x1d5be: MATHEMATICAL SANS-SERIF SMALL e
      // 0x1d5c2: MATHEMATICAL SANS-SERIF SMALL i
      const a = String.fromCodePoint(0x1d5ba);
      const p = String.fromCodePoint(0x1d5c9);
      const e = String.fromCodePoint(0x1d5be);
      const i = String.fromCodePoint(0x1d5c2);

      const str = "Th" + e + " qu" + i + "ck brown fox jum" + p + "s over the l" + a + "zy dog.";

      assert.equal(KMWString.codePointToCodeUnit(str, 2), 2);
      assert.equal(KMWString.codePointToCodeUnit(str, 9), 11); // "The quick" = 9; 2 SMP replacements before then.
      assert.equal(KMWString.codePointToCodeUnit(str, 31), 34); // "the lazy dog" is the remnant; 3 SMP replacements before then.
      assert.equal(KMWString.codePointToCodeUnit(str, -13), 34); // "the lazy dog." is the remnant, 13 codepoints long.
    });

    it('codeUnitToCodePoint', () => {
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
      assert.equal(KMWString.codeUnitToCodePoint(str, 2), 2);
      assert.equal(KMWString.codeUnitToCodePoint(str, 11), 9); // "The quick" = 9; 2 SMP replacements before then.
      assert.equal(KMWString.codeUnitToCodePoint(str, 34), 31); // "the lazy dog" is the remnant; 3 SMP replacements before then.
      // Clearly needs work in the future; only verifying behavioral stability for now.
      assert.equal(KMWString.codeUnitToCodePoint(str, -2), 2);
    });
  });
});