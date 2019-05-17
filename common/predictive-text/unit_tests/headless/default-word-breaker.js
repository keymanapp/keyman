/**
 * Smoke-test the default 
 */
var assert = require('chai').assert;

var breakWords = require('../../build/intermediate').wordBreakers['uax29'];
const SHY = '\u00AD';

describe('The default word breaker', function () {
  it('should break multilingual text', function () {
    let breaks = breakWords(
      `ᑖᓂᓯ᙮ рабочий — after working on ka${SHY}wen${SHY}non:${SHY}nis
         let's eat phở! 🥣`
    );
    let words = breaks.map(span => span.text);
    assert.deepEqual(words, [
      'ᑖᓂᓯ', '᙮', 'рабочий', '—', 'after', 'working', 'on',
      `ka${SHY}wen${SHY}non:${SHY}nis`,
      "let's", 'eat', 'phở', '!', '🥣'
    ]);
  });
});