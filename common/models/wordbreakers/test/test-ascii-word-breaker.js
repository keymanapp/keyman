var assert = require('chai').assert;
var breakASCIIWords = require('..').wordBreakers['ascii'];

describe('The ASCII word breaker', function () {
  it('should break simple English sentences', function () {
    let breaks = breakASCIIWords('Look! -- The quick brown fox jumps... over the lazy dog!');
    let words = breaks.map(span => span.text);
    assert.deepEqual(words, ['Look', 'The', 'quick', 'brown', 'fox', 'jumps', 'over', 'the', 'lazy', 'dog']);
  });
});
