import { assert } from 'chai';
import { ascii as breakASCIIWords } from '@keymanapp/models-wordbreakers';

describe('The ASCII word breaker', function () {
  it('should break simple English sentences', function () {
    let breaks = breakASCIIWords('Look! -- The quick brown fox jumps... over the lazy dog!');
    let words = breaks.map(span => span.text);
    assert.deepEqual(words, ['Look', 'The', 'quick', 'brown', 'fox', 'jumps', 'over', 'the', 'lazy', 'dog']);
  });
});
