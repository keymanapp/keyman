/*
 * Unit tests for the Dummy prediction model.
 */

import { assert } from 'chai';

import { ModelCompositor, models } from '@keymanapp/lm-worker/test-index';
import DummyModel = models.DummyModel;

describe('Custom Punctuation', function () {
  it('appears in the keep suggestion', async function () {
    let dummySuggestions = [{
        transform: {
          insert: 'Hello',
          deleteLeft: 0,
        },
        displayAs: 'Hello',
      },
      {
        transform: {
          insert: 'Jello',
          deleteLeft: 0,
        },
        displayAs: 'Jello',
      }
    ];

    var model = new DummyModel({
      futureSuggestions: [dummySuggestions],
      punctuation: {
        quotesForKeepSuggestion: {
          open: "«", close: "»"
        },
        insertAfterWord: ''
      }
    });

    // The model compositor is responsible for adding this to the display as
    // string.
    var composite = new ModelCompositor(model, true);
    var suggestions = await composite.predict([{ sample: { insert: 'o', deleteLeft: 0 }, p: 1.00 }], {
      left: 'Hrll', startOfBuffer: false, endOfBuffer: true
    });
    assert.lengthOf(suggestions, 3);

    // We only care about the "keep" suggestion.
    var keepSuggestions = suggestions.filter(suggestion => suggestion.tag === 'keep');
    assert.lengthOf(keepSuggestions, 1,
      `Expected exactly one "keep" suggestion, but found ${keepSuggestions.length}`
    );

    // The moment of truth: has our punctuation been applied?
    var suggestion = keepSuggestions[0];
    assert.equal(suggestion.displayAs, `«Hrllo»`);
  });

  describe("insertAfterWord", function () {
    it('appears after "word" suggestion', async function () {
      let dummySuggestions = [
        {
          transform: { insert: 'ᚈᚑᚋ', deleteLeft: 2, },
          displayAs: 'ᚈᚑᚋ',
        },
        {
          transform: { insert: 'ᚄ', deleteLeft: 2, },
          displayAs: 'ᚄ',
        },
        {
          transform: { insert: 'ᚉᚑᚈᚈ', deleteLeft: 2, },
          displayAs: 'ᚉᚑᚈᚈ',
        }
      ];

      var model = new DummyModel({
        futureSuggestions: [dummySuggestions],
        punctuation: {
          // U+1680 OGHAM SPACE MARK:
          // it's technically whitespace, but it don't look it!
          insertAfterWord: " ",
          quotesForKeepSuggestion: {
            open: "'",
            close: "'"
          }
        },
        // Some of the suggestions above actually wordbreak differently from
        // what might be expected.  So, we override the wordbreaker to ensure
        // the tests run smoothly.
        wordbreaker: (text) => {
          const textLen = text.length;
          if(text.charAt(textLen - 1) == " ") {
            return [
              {text: text.substring(0, 1), start: 0, end: 1, length: 1},
              {text: text.substring(1, textLen-2), start: 1, end: textLen-1, length: textLen-2},
              {text: text.substring(textLen-1), start: textLen-1, end: textLen, length: 1}
            ];
          } else {
            return [
              {text: text.substring(0, 1), start: 0, end: 1, length: 1},
              {text: text.substring(1), start: 1, end: textLen, length: textLen-1}
            ];
          }
        }
      });

      // The model compositor is responsible for adding this to the display as
      // string.
      var composite = new ModelCompositor(model, true);
      var suggestions = await composite.predict([{ sample: { insert: 'ᚋ', deleteLeft: 0 }, p: 1.00 }], {
        left: '᚛ᚈᚑ', startOfBuffer: false, endOfBuffer: true
      });
      assert.lengthOf(suggestions, dummySuggestions.length);

      // Check that it has been changed:
      for (var i = 0; i < dummySuggestions.length; i++) {
        assert.isTrue(suggestions[i].appendedTransform.insert == ' ');
      }
    });
  })
});