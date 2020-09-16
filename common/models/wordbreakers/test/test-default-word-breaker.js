/**
 * Smoke-test the default
 */

const assert = require('chai').assert;
const breakWords = require('..').wordBreakers['default'];

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
