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

    it('should predict prefixes within an word and a single letter transform', function () {
      // Predicting when the user JUST typed 'h', with the buffer having a 't' in it:
      //
      //   «th|                       » [Send]
      //   [  this  ] [   the   ] [   there   ]
      var model = new TrieModel(
        jsonFixture('tries/english-1000')
      );

      var initialPrefix = 't';
      var insertedLetter = 'h';
      var truePrefix = initialPrefix + insertedLetter;
      var context = {
        left: initialPrefix,
        startOfBuffer: false,
        endOfBuffer: true
      };
      var suggestions = model.predict({
        insert: insertedLetter,
        deleteLeft: 0,
      }, context);
      assert.isAtLeast(suggestions.length, MIN_SUGGESTIONS);

      // Ensure all of the suggestions actually start with 'th'
      var suggestion;
      var firstTwoChars;

      for (var i = 0; i < MIN_SUGGESTIONS; i++) {
        suggestion = suggestions[i];
        firstTwoChars = applyTransform(context, suggestion.transform).substr(0, 2);
        assert.equal(firstTwoChars, truePrefix);
      }
    });

    it('should produce suggestions with an empty buffer and a zero transform', function () {
      // Predicting when the user has activated an empty text field:
      //
      //   «|                         » [Send]
      //   [   I'm  ] [    I    ] [    Hey    ]
      var model = new TrieModel(
        jsonFixture('tries/english-1000')
      );

      var suggestions = model.predict(zeroTransform(), emptyContext());
      assert.isAtLeast(suggestions.length, MIN_SUGGESTIONS);

      // Ensure all of the suggestions seem okay.
      var suggestion;
      for (var i = 0; i < MIN_SUGGESTIONS; i++) {
        suggestion = suggestions[i];
        assert.isNotEmpty(suggestion.transform.insert);
        assert.strictEqual(suggestion.transform.deleteLeft, 0);
        assert.isNotEmpty(suggestion.displayAs);
      }
    });

    it.skip('should produce after typing at least one word', function () {
    });

  });

  function applyTransform(context, transform) {
    assert.isTrue(context.endOfBuffer, "cannot only apply transform to end of buffer");
    var buffer = context.left;
    buffer = buffer.substr(0, buffer.length - transform.deleteLeft) + transform.insert;
    return buffer;
  }
});
