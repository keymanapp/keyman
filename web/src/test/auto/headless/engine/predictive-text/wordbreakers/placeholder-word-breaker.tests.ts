
import { assert } from 'chai';
import { placeholder as breakWords } from '@keymanapp/models-wordbreakers';

describe('The placeholder word breaker', function () {
  it('should break simple English sentences', function () {
    let breaks = breakWords('Look! -- The quick brown fox jumps... over the lazy dog!');
    let words = breaks.map(span => span.text);
    assert.deepEqual(words, ['Look!', '--', 'The', 'quick', 'brown', 'fox', 'jumps...', 'over', 'the', 'lazy', 'dog!']);
  });
});
