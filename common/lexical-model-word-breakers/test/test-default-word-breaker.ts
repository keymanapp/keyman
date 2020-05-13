/**
 * Smoke-test the default
 */

import {assert} from 'chai';
import {default as breakWords} from '../'

const SHY = '\u00AD';

describe('The default word breaker', function () {
  it('should break multilingual text', function () {
    let breaks = breakWords(
      `Добрый день! ᑕᐻ᙮ — after working on ka${SHY}wen${SHY}non:${SHY}nis,
       let's eat phở! 🥣`
    );
    let words = breaks.map(span => span.text);
    assert.deepEqual(words, [
      'Добрый', 'день', '!', 'ᑕᐻ', '᙮', '—', 'after',
      'working', 'on', `ka${SHY}wen${SHY}non:${SHY}nis`, ',',
      "let's", 'eat', 'phở', '!', '🥣'
    ]);
  });
});
