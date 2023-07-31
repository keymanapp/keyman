import 'mocha';
import {assert} from 'chai';
import {unescapeString, UnescapeError, isOneChar, toOneChar, unescapeOneQuadString} from '../../src/util/util.js';

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

describe('test unescapeOneQuadString()', () => {
  it('should be able to convert', () => {
    // testing that `\u0127` is unescaped correctly (to U+0127: 'ħ')
    assert.equal(unescapeOneQuadString('\\u0127'), '\u{0127}');
    // test the fail cases
  });
  it('should fail when it needs to fail', () => {
    assert.throws(() => unescapeOneQuadString(null), null);
    assert.throws(() => unescapeOneQuadString('\uFFFFFFFFFFFF'));
  });
});
