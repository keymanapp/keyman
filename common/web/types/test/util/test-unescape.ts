import 'mocha';
import {assert} from 'chai';
import {unescapeString, UnescapeError, isOneChar, toOneChar, unescapeOneQuadString, BadStringAnalyzer, isValidUnicode, describeCodepoint, isPUA, BadStringType, unescapeStringToRegex, unescapeQuadString, NFDAnalyzer} from '../../src/util/util.js';

describe('test UTF32 functions()', function() {
  it('should properly categorize strings', () => {
    [
      'x',
      '🙀',
    ].forEach(s => assert.isTrue(isOneChar(s), `isOneChar(${s})`));

    [
      'xx',
      '🙀🙀',
    ].forEach(s => assert.isFalse(isOneChar(s), `!isOneChar(${s})`));
  });

  it('should convert to single chars', function() {
    assert.equal(toOneChar('ħ'), 295);
    assert.equal(toOneChar('🙀'), 0x1F640);
    assert.throws(() => toOneChar('ħħħ'), /Not a single char/);
  });
});

describe('test unescapeString()', function() {
  it("should correctly handle multi strings", function() {
    assert.equal(unescapeString('Sa\\u{0127 0127}a'), 'Saħħa');
  });

  it("should pass through falsy strings", function() {
    assert.equal(unescapeString(''), '');
    assert.equal(unescapeString(null), null);
  });

  it("should pass through non-escaped strings", function() {
    assert.equal(unescapeString('abc'), 'abc');
    assert.equal(unescapeString('\\u{abc'), '\\u{abc');
  });

  it("should correctly unescape strings", function() {
    assert.equal(unescapeString('\\u{0127}'), 'ħ');
    assert.equal(unescapeString('\\u{0127}==\\u{0127}'), 'ħ==ħ');
    assert.equal(unescapeString('Q\\u{0127}'), 'Qħ');
  });

  it("should correctly handle 1..6 char escapes", function() {
    assert.equal(unescapeString('\\u{9}'),      '\u{0009}');   // TAB
    assert.equal(unescapeString('\\u{4a}'),     '\u{004a}');   // J
    assert.equal(unescapeString('\\u{3c8}'),    '\u{03c8}');   // ψ
    assert.equal(unescapeString('\\u{304B}'),   '\u{304b}');   // か
    assert.equal(unescapeString('\\u{1e109}'),  '\u{1e109}');  // 𞄉
    assert.equal(unescapeString('\\u{10fff0}'), '\u{10fff0}'); // Plane 16 Private Use
  });

  it("should throw UnescapeError on invalid escapes", function() {
    assert.throws(() => unescapeString('\\u{110000}'), UnescapeError);
  });
});

describe('test unescapeRegex()', () => {
  it("should correctly handle 1..6 char escapes", function() {
    assert.equal(unescapeStringToRegex('\\u{9}'),       '\\u0009');   // TAB
    assert.equal(unescapeStringToRegex('\\u{5b}'),      '\\u005b');   // [
    assert.equal(unescapeStringToRegex('\\u{005b}'),    '\\u005b');   // [
    assert.equal(unescapeStringToRegex('\\u{4a}'),     'J');   // J
    assert.equal(unescapeStringToRegex('\\u{8a}'),     '\\u008a');   // J
    assert.equal(unescapeStringToRegex('\\u{3c8}'),    'ψ');   // ψ
    assert.equal(unescapeStringToRegex('\\u{304B}'),   'か');   // か
    assert.equal(unescapeStringToRegex('\\u{ffff}'),   '\\uffff');   // noncharacter
    assert.equal(unescapeStringToRegex('\\u{1e109}'),  '𞄉');  // 𞄉
    assert.equal(unescapeStringToRegex('\\u{1ffff}'),  '\\U0001ffff');  // nonchar
    assert.equal(unescapeStringToRegex('\\u{10fff0}'), '\u{10fff0}'); // Plane 16 Private Use
    assert.equal(unescapeStringToRegex('\\u{10ffff}'), '\\U0010ffff'); // nonchar
  });
});

describe('test unescapeOneQuadString()', () => {
  it('should be able to convert', () => {
    // testing that `\u0127` is unescaped correctly (to U+0127: 'ħ')
    assert.equal(unescapeOneQuadString('\\u0127'), '\u{0127}');
    assert.equal(unescapeOneQuadString('\\U0010FFF0'), '\u{10fff0}');
    // test the fail cases
  });
  it('should fail when it needs to fail', () => {
    assert.throws(() => unescapeOneQuadString(null), null);
    assert.throws(() => unescapeOneQuadString('\uFFFFFFFFFFFF'));
  });
  const PAIRED=`\\uD838\\uDD09`;
  it('test of paired surrogates ${UNPAIRED}', () => {
    const s = unescapeQuadString(PAIRED);
    assert.equal(s, '\u{1e109}');
    assert.equal(s, '\u{d838}\u{dd09}');
  });
});

function titleize(o : any) {
  const s = JSON.stringify(o);
  if (!s) {
    return `''`;
  } else if (s.length < 10) {
    return s;
  } else {
    return s.substring(0,10)+'…';
  }
}

describe('test bad char functions', () => {
  it('should match test_kmx_xstring.cpp', () => {
    function Uni_IsValid(start: number, end?: number) {
      return [ start, end ];
    }
    function assert_equal(range: number[], expect : boolean) {
      const [start, end] = range;
      if (end) {
        assert.equal(isValidUnicode(start, end), expect, `for ${describeCodepoint(start)}-${describeCodepoint(end)}}`);
      } else {
        // if branch just for the message
        assert.equal(isValidUnicode(start), expect, `for ${describeCodepoint(start)}`);
      }
    }
    // following lines are from test_kmx_xstring.cpp
    assert_equal(Uni_IsValid(0x0000), true);
    assert_equal(Uni_IsValid(0x0127), true);
    assert_equal(Uni_IsValid('🙀'.codePointAt(0)), true);
    assert_equal(Uni_IsValid(0xDECAFBAD), false); // out of range
    assert_equal(Uni_IsValid(0x566D4128), false);
    assert_equal(Uni_IsValid(0xFFFF), false); // nonchar
    assert_equal(Uni_IsValid(0xFFFE), false); // nonchar
    assert_equal(Uni_IsValid(0x10FFFF), false); // nonchar
    assert_equal(Uni_IsValid(0x10FFFE), false); // nonchar
    assert_equal(Uni_IsValid(0x01FFFF), false); // nonchar
    assert_equal(Uni_IsValid(0x01FFFE), false); // nonchar
    assert_equal(Uni_IsValid(0x02FFFF), false); // nonchar
    assert_equal(Uni_IsValid(0x02FFFE), false); // nonchar
    assert_equal(Uni_IsValid(0xFDD1), false); // nonchar
    assert_equal(Uni_IsValid(0xD800), false); // orphaned surrogate
    assert_equal(Uni_IsValid(0xFDD0), false); // nonchar
    assert_equal(Uni_IsValid(0x100000, 0x10FFFD), true);
    assert_equal(Uni_IsValid(0x10, 0x20), true);
    assert_equal(Uni_IsValid(0x100000, 0x10FFFD), true);
    assert_equal(Uni_IsValid(0x0000, 0xD7FF), true);
    assert_equal(Uni_IsValid(0xD800, 0xDFFF), false); // orphaned surrogate
    assert_equal(Uni_IsValid(0xE000, 0xFDCF), true);
    assert_equal(Uni_IsValid(0xFDD0, 0xFDEF), false);
    assert_equal(Uni_IsValid(0xFDF0, 0xFDFF), true);
    assert_equal(Uni_IsValid(0xFDF0, 0xFFFD), true);
    assert_equal(Uni_IsValid(0, 0x10FFFF), false);         // ends with nonchar
    assert_equal(Uni_IsValid(0, 0x10FFFD), false);         // contains lots o' nonchars
    assert_equal(Uni_IsValid(0x20, 0x10), false);          // swapped
    assert_equal(Uni_IsValid(0xFDEF, 0xFDF0), false);      // just outside range
    assert_equal(Uni_IsValid(0x0000, 0x010000), false);    // crosses noncharacter plane boundary and other stuff
    assert_equal(Uni_IsValid(0x010000, 0x020000), false);  // crosses noncharacter plane boundary
    assert_equal(Uni_IsValid(0x0000, 0xFFFF), false);      // crosses other BMP prohibited and plane boundary
    assert_equal(Uni_IsValid(0x0000, 0xFFFD), false);      // crosses other BMP prohibited
    assert_equal(Uni_IsValid(0x0000, 0xE000), false);      // crosses surrogate space
    assert_equal(Uni_IsValid(0x0000, 0x20FFFF), false);      // out of bounds
    assert_equal(Uni_IsValid(0x10FFFD, 0x20FFFF), false);      // out of bounds
  });
  it('should detect non-PUA', () => {
    const strs = "abcd" +
    ([
      0xF900,
      0xFFFFF,
    ].map(ch => String.fromCodePoint(ch)).join(''));
    for (const s of strs) {
      const ch = s.codePointAt(0);
      assert.isFalse(isPUA(ch), describeCodepoint(ch));
    }
  });
  it('should detect PUA', () => {
    const strs = "\uE010" +
      ([
        0xE000,0xE001,0xE002,
        0xF000,
        0xF800,
        0xF8FF,

        0x0F0000,
        0x0FFFFD,

        0x100000,
        0x10FFFD
      ].map(ch => String.fromCodePoint(ch)).join(''));
    for (const s of strs) {
      const ch = s.codePointAt(0);
      assert.isTrue(isPUA(ch), describeCodepoint(ch));
    }
  });
});

describe('test BadStringAnalyzer', () => {
  describe('should return nothing for all valid strings', () => {
    const cases = [
      [],
      ['a',],
      ['a', 'b',]
    ];
    for (const strs of cases) {
      const title = titleize(strs);
      it(`should analyze ${title}`, () => {
        const bsa = new BadStringAnalyzer();
        for (const s of strs) {
          bsa.add(s);
        }
        const m = bsa.analyze();
        assert.isNull(m, `${title}`);
      });
    }
  });
  it('should handle a case with some odd strs in it', () => {
    const strs = "But you can call me “\uE010\uFDD0\uFFFE\uD800”, for short." +
      ([
        0xF800,
        0x05FFFF,
        0x102222,
        0x04FFFE,
      ].map(ch => String.fromCodePoint(ch)).join(''));

    const bsa = new BadStringAnalyzer();
    for (const s of strs) {
      bsa.add(s);
    }
    const m = bsa.analyze();
    assert.isNotNull(m);
    assert.containsAllKeys(m, [BadStringType.pua, BadStringType.illegal]);
    assert.sameDeepMembers(Array.from(m.get(BadStringType.pua).values()), [
      0xE010, 0xF800, 0x102222,
    ], `pua analysis`);
    assert.sameDeepMembers(Array.from(m.get(BadStringType.illegal).values()), [
      0xFDD0, 0xD800, 0xFFFE,
      0x05FFFF,
      0x04FFFE,
    ], `illegal analysis`);
  });
});

describe('test NFDAnalyzer', () => {
  describe('should return nothing for NFD strings', () => {
    const cases = [
      [],
      ['a',],
      ['a', 'b',]
    ];
    for (const strs of cases) {
      const title = titleize(strs);
      it(`should analyze ${title}`, () => {
        const bsa = new NFDAnalyzer();
        for (const s of strs) {
          bsa.add(s);
        }
        const m = bsa.analyze();
        assert.isNull(m, `${title}`);
      });
    }
  });
  it('should handle a case with some odd strs in it', () => {
    const strs = "This text is in NFD, but not all of it is." +
      ([
        0x00E8,
        0x0344,
        0x2FA1D,
      ].map(ch => String.fromCodePoint(ch)).join(''));

    const bsa = new NFDAnalyzer();
    for (const s of strs) {
      bsa.add(s);
    }
    const m = bsa.analyze();
    assert.isNotNull(m);
    assert.containsAllKeys(m, [BadStringType.denormalized]);
    assert.sameDeepMembers(Array.from(m.get(BadStringType.denormalized).values()), [
      0x00E8,
      0x0344,
      0x2FA1D,
  ], `denorm analysis`);
  });
});
