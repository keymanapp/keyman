import 'mocha';
import {assert} from 'chai';
import {unescapeString} from '../../src/util/util.js';

describe('test unescapeString()', function() {
  it("should pass through falsy strings", function() {
    assert.equal(unescapeString(''), '');
    assert.equal(unescapeString(null), null);
  });

  it("should pass through non-escaped strings", function() {
    assert.equal(unescapeString('abc'), 'abc');
  });

  it("should correctly unescape strings", function() {
    assert.equal(unescapeString('\\u{0127}'), 'ħ');
    assert.equal(unescapeString('\\u{0127}==\\u{0127}'), 'ħ==ħ');
    assert.equal(unescapeString('Q\\u{0127}'), 'Qħ');
  });

  it("should correctly handle 1..6 char escapes", function() {
    assert.equal(unescapeString('\\u{9}'),     '\t');
    assert.equal(unescapeString('\\u{4a}'),    'J');
    assert.equal(unescapeString('\\u{3c8}'),   'ψ');
    assert.equal(unescapeString('\\u{304B}'),  'か');
    assert.equal(unescapeString('\\u{1e109}'), '\u{1e109}');
    assert.equal(unescapeString('\\u{10fff0}'), '\u{10fff0}');
  });
});
