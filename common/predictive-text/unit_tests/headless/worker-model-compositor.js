/*
 * Integration tests for the model compositor with the trie model.
 */

const { models } = require('../../build/intermediate');

var assert = require('chai').assert;
var TrieModel = require('../../build/intermediate').models.TrieModel;
var ModelCompositor = require('../../build/intermediate').ModelCompositor;

describe('ModelCompositor', function() {
  it('should compose suggestions from a fat-fingered keypress (no keying needed)', function () {
    var model = new TrieModel(
      jsonFixture('tries/english-1000')
    );
    var composite = new ModelCompositor(model);
    
    // Initialize context
    let context = {
      left: 'th', startOfBuffer: false, endOfBuffer: true,
    };
    composite.predict({insert: '', deleteLeft: 0}, context);

    // Pretend to fat finger "the" as "thr"
    var the = { sample: { insert: 'r', deleteLeft: 0}, p: 0.45 };
    var thr = { sample: { insert: 'e', deleteLeft: 0}, p: 0.55 };
    var suggestions = composite.predict([thr, the], context);

    // Get the top suggest for 'the' and 'thr*'.
    var theSuggestion = suggestions.filter(function (s) { return s.displayAs === 'the' || s.displayAs === '“the”'; })[0];
    var thrSuggestion = suggestions.filter(function (s) { return s.displayAs.startsWith('thr'); })[0];

    // Sanity check: do we have actual real-valued probabilities?
    assert.isAbove(thrSuggestion.p, 0.0);
    assert.isBelow(thrSuggestion.p, 1.0);
    assert.isAbove(theSuggestion.p, 0.0);
    assert.isBelow(theSuggestion.p, 1.0);
    // 'the' should be the intended the result here.
    assert.isAbove(theSuggestion.p, thrSuggestion.p);
  });

  it('should compose suggestions from a fat-fingered keypress (keying needed)', function () {
    var model = new TrieModel(
      jsonFixture('tries/english-1000')
    );
    var composite = new ModelCompositor(model);
    
    // Initialize context
    let context = {
      left: 'Th', startOfBuffer: false, endOfBuffer: true,
    };
    composite.predict({insert: '', deleteLeft: 0}, context);

    // Pretend to fat finger "the" as "thr"
    var the = { sample: { insert: 'r', deleteLeft: 0}, p: 0.45 };
    var thr = { sample: { insert: 'e', deleteLeft: 0}, p: 0.55 };
    var suggestions = composite.predict([thr, the], context);

    // Get the top suggest for 'the' and 'thr*'.
    var theSuggestion = suggestions.filter(function (s) { return s.displayAs === 'The' || s.displayAs === '“The”'; })[0];
    var thrSuggestion = suggestions.filter(function (s) { return s.displayAs.startsWith('thr'); })[0];

    // Sanity check: do we have actual real-valued probabilities?
    assert.isAbove(thrSuggestion.p, 0.0);
    assert.isBelow(thrSuggestion.p, 1.0);
    assert.isAbove(theSuggestion.p, 0.0);
    assert.isBelow(theSuggestion.p, 1.0);
    // 'the' should be the intended the result here.
    assert.isAbove(theSuggestion.p, thrSuggestion.p);
  });

  // The nomenclature's a minor sneak-peek from child PRs.
  describe('toAnnotatedSuggestion', function() {
    let baseSuggestion = {
      transform: {
        insert: 'hello',
        deleteLeft: 0,
        id: 0
      },
      transformId: 0,
      displayAs: 'hello'
    };

    let englishPunctuation = {
      quotesForKeepSuggestion: { open: `“`, close: `”`},
      insertAfterWord: ' '
    };

    let angledPunctuation = {
      quotesForKeepSuggestion: { open: `«`, close: `»`},
      insertAfterWord: " "
    }

    describe("'keep'", function() {
      let annotationTest = function(punctuation, displayText, quoteStyle) {
        let options = {
          punctuation: punctuation
        };
  
        let model = new models.DummyModel(options);
        let compositor = new ModelCompositor(model);
  
        var keep;
        if(quoteStyle) {
          keep = compositor.toAnnotatedKeepSuggestion(baseSuggestion, quoteStyle);
        } else {
          keep = compositor.toAnnotatedKeepSuggestion(baseSuggestion);
        }

        // Make sure we didn't accidentally leak any mutations to the parameter.
        assert.notDeepEqual(keep, baseSuggestion);
  
        assert.equal(keep.displayAs, displayText);
        assert.equal(keep.tag, 'keep');
      }

      it('quoteBehavior: (.default)', function() {
        annotationTest(englishPunctuation, "“hello”");
        annotationTest(angledPunctuation, "«hello»");
      });

      it('quoteBehavior: .useQuotes', function() {
        annotationTest(englishPunctuation, "“hello”", models.QuoteBehavior.useQuotes);
        annotationTest(angledPunctuation, "«hello»", models.QuoteBehavior.useQuotes);
      });

      it('quoteBehavior: .noQuotes', function() {
        annotationTest(englishPunctuation, "hello", models.QuoteBehavior.noQuotes);
        annotationTest(angledPunctuation, "hello", models.QuoteBehavior.noQuotes);
      });

      it.skip('RTL test', function() {
        // TODO:
      });
    });
  });
});
