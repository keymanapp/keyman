/*
 * Unit tests for the Dummy prediction model.
 */

var assert = require('chai').assert;
var DummyModel = require('../../build/intermediate').models.DummyModel;
var ModelCompositor = require('../../build/intermediate').ModelCompositor;

describe('Custom Punctuation', function() {
  it('appears in the keep suggestion', function () {
    let dummySuggestions = [
      {
        transform: {
          insert: 'Hrllo',
          deleteLeft: 0,
        },
        tag: 'keep',
        displayAs: 'Hrllo',
      },
      {
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
        }
      }
    });

    // The model compositor is responsible for adding this to the display as
    // string.
    var composite = new ModelCompositor(model);
    var suggestions =  composite.predict([{ sample: { insert: 'o', deleteLeft: 0 }, p: 1.00 }], {
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
});