/*
 * Unit tests for the Trie prediction model.
 */

var assert = require('chai').assert;
var TrieModel = require('../../build/intermediate').models.TrieModel;

describe('LMLayerWorker trie model for word lists', function() {
  describe('prediction', function () {
    var MIN_SUGGESTIONS = 3;

    it('should predict prefixes with an empty context and a single letter transform', function () {
      // Predicting when the user JUST typed 't' should result in something like this:
      //
      //   «t|                        » [Send]
      //   [   to   ] [   the   ] [   this    ]
      var model = new TrieModel(
        jsonFixture('tries/english-1000')
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

    it.skip('should predict prefixes within an word and a single letter transform', function () {
    });

    it.skip('should produce suggestions with an empty buffer and a zero transform', function () {
    });

    it.skip('should produce after typing at least one word', function () {
    });
  });
});
