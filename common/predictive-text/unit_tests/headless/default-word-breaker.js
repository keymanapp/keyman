/**
 * Smoke-test the default word breaker.
 */
var assert = require('chai').assert;
var TrieModel = require('../../build/intermediate').models.TrieModel;

var breakWords = require('../../build/intermediate').wordBreakers['default'];
const SHY = '\u00AD';

// The following are **integration tests** testing the interaction between the
// Trie model "template" and the default word breaker.
//
// They exercise whether the default word breaker produces the correct word
// breaks, and whether the Trie model can consume the word breaks properly.
describe('The default word breaker', function () {
  it('recognizes a word at end of complete lefthand context', function () {
    var model = new TrieModel(jsonFixture('tries/english-1000'), {
      wordBreaker: breakWords // wordBreakers['default'] when fully integrated.
    });

    // Standard case - wordbreaking at the end of a word.
    var context = {
      left: 'The quick brown fox jumped', startOfBuffer: true,
      right: ' over the lazy dog.', endOfBuffer: true
    };

    var broken = model.wordbreak(context);

    assert.strictEqual(broken, 'jumped');
  });

  // Same test as before, but we want to be sure the start/end of buffer flags
  // don't affect our results.
  it('recognizes a word at end of incomplete lefthand context', function () {
    var model = new TrieModel(jsonFixture('tries/english-1000'), {
      wordBreaker: breakWords
    });

    // Standard case - wordbreaking at the end of a word.
    var context = {
      left: 'The quick brown fox jumped', startOfBuffer: false,
      right: ' over the lazy dog.', endOfBuffer: false
    };

    var broken = model.wordbreak(context);

    assert.strictEqual(broken, 'jumped');
  });

  it('returns text for a word in-progress', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'), {
      wordBreaker: breakWords
    });

    // Standard case - midword (xylophone) call
    var context = {
      left: 'xyl', startOfBuffer: true,
      right: '', endOfBuffer: true
    };

    var broken = model.wordbreak(context);

    assert.strictEqual(broken, 'xyl');
  });

  it('returns empty string when called without word text', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'), {
      wordBreaker: breakWords
    });

    // Wordbreaking on a empty space => no word.
    context = {
      left: 'The quick brown fox jumped ', startOfBuffer: true,
      right: 'over the lazy dog.', endOfBuffer: true
    };

    broken = model.wordbreak(context);

    assert.strictEqual(broken, '');
  });

  it('returns empty string when called with empty context', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'), {
      wordBreaker: breakWords
    });

    // Wordbreaking on a empty space => no word.
    context = {
      left: '', startOfBuffer: true,
      right: '', endOfBuffer: true
    };

    broken = model.wordbreak(context);

    assert.strictEqual(broken, '');
  });

  it('returns empty string when called with nil context', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'), {
      wordBreaker: breakWords
    });

    // Wordbreaking on a empty space => no word.
    context = {
      left: '', startOfBuffer: false,
      right: '', endOfBuffer: false
    };

    broken = model.wordbreak(context);

    assert.strictEqual(broken, '');
  });

  it.skip('correctly breaks a word when the caret is placed within it', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'), {
      wordBreaker: breakWords
    });

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
