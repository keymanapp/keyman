/*
 * Unit tests for the Dummy prediction model.
 */

import { assert } from 'chai';

import DummyModel from '#./models/dummy-model.js';
import ModelCompositor from '#./model-compositor.js';

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
          transform: { insert: 'ᚈᚑᚋ', deleteLeft: 0, },
          displayAs: 'ᚈᚑᚋ',
        },
        {
          transform: { insert: 'ᚄ', deleteLeft: 0, },
          displayAs: 'ᚄ',
        },
        {
          transform: { insert: 'ᚉᚑᚈᚈ', deleteLeft: 0, },
          displayAs: 'ᚉᚑᚈᚈ',
        }
      ];

      var model = new DummyModel({
        futureSuggestions: [dummySuggestions],
        punctuation: {
          // U+1680 OGHAM SPACE MARK:
          // it's technically whitespace, but it don't look it!
          insertAfterWord: " ",
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
        assert.isTrue(suggestions[i].transform.insert.endsWith(' '));
      }
    });
  })
});