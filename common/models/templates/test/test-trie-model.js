/*
 * Unit tests for the Trie prediction model.
 */

import { assert } from 'chai';
import { TrieModel } from '@keymanapp/models-templates';

describe('LMLayerWorker trie model for word lists', function() {
  describe('instantiation', function () {
    it('should expose the punctuation object', function () {
      var spaceMark = "👩🏻‍🚀";
      var openQuote = "🌜";
      var closeQuote = "🌛";

      var model = new TrieModel(jsonFixture('tries/english-1000'), {
        punctuation: {
          insertAfterWord: spaceMark,
          quotesForKeepSuggestion: {
            open: openQuote, close: closeQuote
          }
        }
      })

      assert.equal(model.punctuation.insertAfterWord, spaceMark);
      assert.equal(model.punctuation.quotesForKeepSuggestion.open, openQuote);
      assert.equal(model.punctuation.quotesForKeepSuggestion.close, closeQuote);
    })
  });

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
      }, emptyContext()).map(getSample);
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
      }, context).map(function(value) { return value.sample });
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

      var suggestions = model.predict(zeroTransform(), emptyContext()).map(function(value) { return value.sample });
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

    it('should produce after typing at least one word', function () {
      // Predicting after typing at least one word.
      //
      //   «I g|                        » [Send]
      //   [  gave  ] [   got   ] [  got the  ]
      var model = new TrieModel(
        jsonFixture('tries/english-1000')
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

  describe('Using an alternate key function', function () {
    it('can use an alternate key function', function () {
      var model = new TrieModel(jsonFixture('tries/english-1000'), {
        // This test is a bit silly. We can only search strings that
        // begin with a sequence of "a"s.
        searchTermToKey: function (searchTerm) {
          let result = searchTerm.replace(/[^a]/g, '');
          return result;
        }
      });

      // This search should yield results for "a"
      var firstResults = model.predict({
        insert: "a", deleteLeft: 0
      }, {
        left: '', startOfBuffer: false, endOfBuffer: true
      });
      assert.isAbove(firstResults.length, 0);

      // This should yield the SAME results, because it used the same internal
      // query.
      var otherResults = model.predict({
        insert: "a", deleteLeft: 0
      }, {
        left: 't', startOfBuffer: false, endOfBuffer: true
      });

      // the SAME results should be suggested (made the same query)
      assert.deepEqual(
        otherResults.map(s => s.displayAs),
        firstResults.map(s => s.displayAs)
      );
    });
  });

  describe('The default key function', function () {
    it('uses the default key function', function () {
      var model = new TrieModel(jsonFixture('tries/accented'));

      //   «nai|                » [Send]
      //   [  ???  ] [  naïve  ] [  ???  ]
      var [suggestion] = model.predict({
        insert: 'i', deleteLeft: 0
      }, {
        left: 'na',
        startOfBuffer: false,
        endOfBuffer: true
      }).map(getSample);
      assert.strictEqual(suggestion.displayAs, 'naïve');

      //   «Let‘s get some pho|       » [Send]
      //   [  ???  ] [  phở  ] [  ???  ]
      var [suggestion] = model.predict({
        insert: 'o', deleteLeft: 0
      }, {
        left: 'Let‘s get some ph',
        startOfBuffer: false,
        endOfBuffer: true
      }).map(getSample);
      assert.strictEqual(suggestion.displayAs, 'phở');

    });
  });

  it('replaces the entire typed word when a suggestion is accepted', function () {
      // Ensure that all input is lower-cased when we try to look it up.
      var model = new TrieModel(jsonFixture('tries/english-1000'), {
        searchTermToKey: function (searchTerm) {
          let result = searchTerm.toLowerCase();
          return result;
        }
      });

      // Note: the input is **UPPERCASE**
      var context = { left: 'T', startOfBuffer: false, endOfBuffer: true };
      var transform = { insert: "H", deleteLeft: 0 };

      // Predict upon typing
      //   «TH|                        » [Send]
      //   [  there ] [   the   ] [   they    ]
      var [suggestion] = model.predict(transform, context).map(getSample);
      // I'm assuming the top word in English is "the".
      assert.strictEqual(suggestion.displayAs, 'the');

      var newBuffer = applyTransform(context, suggestion.transform);
      // The suggestion should change it to lowercase.
      assert.strictEqual(newBuffer, 'the');
  });

  function applyTransform(context, transform) {
    assert.isTrue(context.endOfBuffer, "cannot only apply transform to end of buffer");
    var buffer = context.left;
    buffer = buffer.substr(0, buffer.length - transform.deleteLeft) + transform.insert;
    return buffer;
  }

  /**
   * For use in predict().map(FUNC) to get the underlying suggestion.
   * @param {ProbabilityMass<T>} pm
   */
  function getSample(pm) {
    return pm.sample
  }
});
