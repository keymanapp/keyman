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
    var MIN_SUGGESTIONS = 3;

    it('should predict prefixes with an empty context and a single letter transform', function () {
      // Predicting when the user JUST typed 't' should result in something like this:
      //
      //   «t|                        » [Send]
      //   [   to   ] [   the   ] [   this    ]
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

    it('should predict prefixes within an word and a single letter transform', function () {
      // Predicting when the user JUST typed 'h', with the buffer having a 't' in it:
      //
      //   «th|                       » [Send]
      //   [  this  ] [   the   ] [   there   ]
      var model = new WordListModel(
        defaultCapabilities(),
        jsonFixture('wordlists/english-1000')
      );

      var suggestions = model.predict({
        insert: 'h',
        deleteLeft: 0,
      }, {
        left: 't',
        startOfBuffer: false,
        endOfBuffer: true
      });
      assert.isAtLeast(suggestions.length, MIN_SUGGESTIONS);

      // Ensure all of the suggestions actually start with 'th'
      var suggestion;
      var suggestedWord;
      for (var i = 0; i < MIN_SUGGESTIONS; i++) {
        suggestion = suggestions[i];
        suggestedWord = suggestion.transform.insert;
        assert.strictEqual(suggestedWord.substr(0, 2), 'th');
        assert.strictEqual(suggestion.transform.deleteLeft, 1);
      }
    });
  });
});

