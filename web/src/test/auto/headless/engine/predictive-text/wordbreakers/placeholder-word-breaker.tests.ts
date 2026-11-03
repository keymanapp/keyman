/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { assert } from 'chai';
import { placeholder as breakWords } from '@keymanapp/models-wordbreakers';

describe('The placeholder word breaker', function () {
  it('should break simple English sentences', function () {
    const breaks = breakWords('Look! -- The quick brown fox jumps... over the lazy dog!');
    const words = breaks.map(span => span.text);
    assert.deepEqual(words, ['Look!', '--', 'The', 'quick', 'brown', 'fox', 'jumps...', 'over', 'the', 'lazy', 'dog!']);
  });
});
