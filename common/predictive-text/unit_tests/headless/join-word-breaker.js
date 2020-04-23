var assert = require('chai').assert;

var defaultWordBreaker = require('../../build/intermediate').wordBreakers['default'];
var decorateWithJoin = require('../../build/intermediate').wordBreakers['join_'];

describe('The join word breaker decorator', function () {
  it('should decorate an existing word breaker', function () {
    let breakWords = decorateWithJoin(defaultWordBreaker, ['-']);
    assert.isFunction(breakWords);
  });
});