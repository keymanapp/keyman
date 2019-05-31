/*
 * Unit tests for the Dummy prediction model.
 */

var assert = require('chai').assert;
var WordListModel = require('../../build/intermediate').models.WordListModel;

describe('LMLayerWorker word list model', function() {
  describe('instantiation', function () {
    it('can be instantiated with an empty word list', function () {
      var model = new WordListModel([]);
      assert.isObject(model);
    });

    it('can be instantiated with an word list', function () {
      var model = new WordListModel([
        ['foo', 0],
        ['bar', 1],
        ['baz', Math.MAX_SAFE_INTEGER]
      ]);
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
        jsonFixture('wordlists/english-1000')
      );

      var suggestions = model.predict({
        insert: 't',
        deleteLeft: 0,
      }, emptyContext()).map(function(value) { return value.sample });
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
        jsonFixture('wordlists/english-1000')
      );

      var initialPrefix = 't';
      var insertedLetter = 'h';
      var truePrefix = initialPrefix + insertedLetter;
      var suggestions = model.predict({
        insert: insertedLetter,
        deleteLeft: 0,
      }, {
        left: initialPrefix,
        startOfBuffer: false,
        endOfBuffer: true
      }).map(function(value) { return value.sample });
      assert.isAtLeast(suggestions.length, MIN_SUGGESTIONS);

      // Ensure all of the suggestions actually start with 'th'
      var suggestion;
      var suggestedWord;

      // XXX: this tests knows TOO MUCH about the implementation of the function.
      // Need a test that asserts behavior; not how it got there.
      for (var i = 0; i < MIN_SUGGESTIONS; i++) {
        suggestion = suggestions[i];
        suggestedWord = suggestion.displayAs;
        // Assuming the transform where nothing is deleted, but
        // the rest of the suggested word is inserted.
        assert.strictEqual(suggestion.transform.deleteLeft, 0);
        // The first two characters should match the prefix WITH the transform.
        assert.strictEqual(suggestedWord.substr(0, truePrefix.length), truePrefix);
        assert.strictEqual(suggestion.transform.insert[0], insertedLetter);
      }
    });

    it('should produce suggestions with an empty buffer and a zero transform', function () {
      // Predicting when the user has activated an empty text field:
      //
      //   «|                         » [Send]
      //   [   I'm  ] [    I    ] [    Hey    ]
      var model = new WordListModel(
        jsonFixture('wordlists/english-1000')
      );

      var suggestions = model.predict(zeroTransform(), emptyContext()).map(function(value) { return value.sample });
      assert.isAtLeast(suggestions.length, MIN_SUGGESTIONS);

      // Ensure all of the suggestions seem valid.
      var suggestion;
      for (var i = 0; i < MIN_SUGGESTIONS; i++) {
        suggestion = suggestions[i];
        assert.isNotEmpty(suggestion.transform.insert);
        assert.strictEqual(suggestion.transform.deleteLeft, 0);
        assert.isNotEmpty(suggestion.displayAs);
      }
    });

    it('should produce after typing at least one word', function () {
      // Predicting after typing at least one word.
      //
      //   «I g|                        » [Send]
      //   [  gave  ] [   got   ] [  got the  ]
      var model = new WordListModel(
        jsonFixture('wordlists/english-1000')
      );

      var truePrefix = 'g';
      var suggestions = model.predict({
        insert: truePrefix,
        deleteLeft: 0
      }, {
        left: 'I ',
        startOfBuffer: false,
        endOfBuffer: true,
      }).map(function(value) { return value.sample });
      assert.isAtLeast(suggestions.length, MIN_SUGGESTIONS);

      // Ensure all of the suggestions seem valid.
      var suggestion;
      for (var i = 0; i < MIN_SUGGESTIONS; i++) {
        suggestion = suggestions[i];
        assert.strictEqual(suggestion.transform.insert.substr(0, truePrefix.length), truePrefix);
        assert.strictEqual(suggestion.transform.deleteLeft, 0);
        assert.isNotEmpty(suggestion.displayAs.substr(0, truePrefix.length), truePrefix);
      }
    });
  });
});
