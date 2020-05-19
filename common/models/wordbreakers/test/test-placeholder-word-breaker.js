const assert = require('chai').assert;
const breakWords = require('..').wordBreakers['placeholder'];


describe('The placeholder word breaker', function () {
  it('should break simple English sentences', function () {
    let breaks = breakWords('Look! -- The quick brown fox jumps... over the lazy dog!');
    let words = breaks.map(span => span.text);
    assert.deepEqual(words, ['Look!', '--', 'The', 'quick', 'brown', 'fox', 'jumps...', 'over', 'the', 'lazy', 'dog!']);
  });
});
