/*
 * Unit tests for the Dummy prediction model.
 */

var assert = require('chai').assert;

var WordListModel = require('../../build/intermediate').models.WordListModel;

describe('LMLayerWorker word list model', function() {
  describe('instantiation', function () {
    it('can be instantiated with an empty word list', function () {
      var model = new WordListModel(defaultCapabilities(), []);
      assert.isObject(model);
    });

    it('can be instantiated with an word list', function () {
    });
  });

  describe('prediction', function () {
    it('should predict prefixes with a single letter transform', function () {
      var MIN_SUGGESTIONS = 3;
      var model = new WordListModel(
        defaultCapabilities(),
        jsonFixture('wordlists/english-1000')
      );

      var suggestions = model.predict({
        insert: 't',
        deleteLeft: 0,
      }, emptyContext());
      assert.isAtLeast(suggestions.length, MIN_SUGGESTIONS);

      // Ensure all of the suggestions actually start with 't'
      var suggestion;
      var suggestedWord;
      for (var i = 0; i < MIN_SUGGESTIONS; i++) {
        suggestion = suggestions[i];
        suggestedWord = suggestion.transform.insert;
        assert.strictEqual(suggestedWord.substr(0, 1), 't');
      }
    });
  });
});

