var assert = require('chai').assert;

var defaultWordBreaker = require('../../build/intermediate').wordBreakers['default'];
var joinWordBreaker = require('../../build/intermediate').wordBreakers['--join-word'];

describe('The join word breaker decorator', function () {
  it('should decorate an existing word breaker', function () {
    let breakWords = joinWordBreaker(defaultWordBreaker);
    assert.isFunction(breakWords);
  });
});