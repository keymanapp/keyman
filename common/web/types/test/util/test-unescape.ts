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
    assert.equal(unescapeString('Q\\u{0127}'), 'Qħ');
  });


});
