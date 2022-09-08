/**
 * Smoke-test the default
 */

const assert = require('chai').assert;
const breakWords = require('../build').wordBreakers['default'];

const SHY = '\u00AD'; // Other, Format.  The "Soft HYphen" - usually invisible unless needed for word-wrapping.

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

  it('handles heavily-punctuated English text', function() {
    // This test case brought to you by http://unicode.org/reports/tr29/#Word_Boundaries, Figure 1.
    let breaks = breakWords(
      `The quick ("brown") fox can't jump 32.3 feet, right?`
    );
    let words = breaks.map(span => span.text);
    assert.deepEqual(words, [
      'The', 'quick', '(', '"', 'brown', '"', ')', 'fox', "can't",
      'jump', '32.3', 'feet', ',', 'right', '?'
    ]);
  });
});
