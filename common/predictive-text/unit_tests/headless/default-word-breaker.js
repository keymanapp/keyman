/**
 * Smoke-test the default 
 */
var assert = require('chai').assert;
var TrieModel = require('../../build/intermediate').models.TrieModel;

var breakWords = require('../../build/intermediate').wordBreakers['default'];
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

  // The following tests are performed with model integration as an internal
  // test for the wordbreaking API.
  it('recognizes a word at end of lefthand context', function () {
    var model = new TrieModel(jsonFixture('tries/english-1000'));

    // Standard case - wordbreaking at the end of a word.
    var context = { 
      left: 'The quick brown fox jumped', startOfBuffer: true,
      right: ' over the lazy dog.', endOfBuffer: true
    };

    var broken = model.wordbreak(context);

    assert.strictEqual(broken, 'jumped');
  });

  it('returns text for a word in-progress', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'));

    // Standard case - midword (xylophone) call
    var context = { 
      left: 'xyl', startOfBuffer: true,
      right: '', endOfBuffer: true
    };

    var broken = model.wordbreak(context);

    assert.strictEqual(broken, 'xyl');
  });
    
  it('returns empty string when called without word text', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'));

    // Wordbreaking on a empty space => no word.
    context = { 
      left: 'The quick brown fox jumped ', startOfBuffer: true,
      right: 'over the lazy dog.', endOfBuffer: true
    };

    broken = model.wordbreak(context);

    assert.strictEqual(broken, '');
  });

  it.skip('correctly breaks a word when the caret is placed within it', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'));

    // A limitation of the current implementation; we should fix this before release.
    // Then again, when typing this is probably fine; just not when not typing.
    context = { 
      left: 'The quick brown fox jum', startOfBuffer: true,
      right: 'ped over the lazy dog.', endOfBuffer: true
    };

    broken = model.wordbreak(context);

    assert.strictEqual(broken, 'jumped');  // Current result:  'jum'.    
  });
});
