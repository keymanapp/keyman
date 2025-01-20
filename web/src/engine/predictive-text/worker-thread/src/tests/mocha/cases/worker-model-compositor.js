/*
 * Integration tests for the model compositor with the trie model.
 */

import ModelCompositor from '#./model-compositor.js';
import { toAnnotatedSuggestion } from '#./predict-helpers.js';
import * as models from '#./models/index.js';
import * as wordBreakers from '@keymanapp/models-wordbreakers';

import { assert } from 'chai';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

var TrieModel = models.TrieModel;

describe('ModelCompositor', function() {
  describe('Prediction with 14.0+ models', function() {
    describe('Basic suggestion generation', function() {
      var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
        {wordBreaker: wordBreakers.default}
      );

      it('generates suggestions with expected properties', async function() {
        let compositor = new ModelCompositor(plainModel, true);
        let context = {
          left: 'th', startOfBuffer: true, endOfBuffer: true,
        };

        let inputTransform = {
          insert: 'e',
          deleteLeft: 0
        };

        let suggestions = await compositor.predict(inputTransform, context);
        suggestions.forEach(function(suggestion) {
          // Suggstions are built based on the context state BEFORE the triggering
          // input, replacing the prediction's root with the complete word.
          //
          // This is necessary, in part, for proper display-string construction.
          assert.equal(suggestion.transform.deleteLeft, 2);
        });

        let keep = suggestions.find(function(suggestion) {
          return suggestion.tag == 'keep';
        });

        assert.isDefined(keep);
        assert.equal(keep.transform.insert, 'the ');

        // Expect an appended space.
        let expectedEntries = ['they ', 'there ', 'their ', 'these ', 'themselves '];
        expectedEntries.forEach(function(entry) {
          assert.isDefined(suggestions.find(function(suggestion) {
            return suggestion.transform.insert == entry;
          }));
        });
      });

      it('strongly avoids corrections for single-character roots', async function() {
        let compositor = new ModelCompositor(plainModel, true);
        let context = {
          left: '', startOfBuffer: true, endOfBuffer: true,
        };

        // The 'weights' involved imply that we have an edge-case fat finger on the bottom of
        // the 'q' key, slightly in its favor.
        let inputDistribution = [
          {sample: {insert: 'q', deleteLeft: 0}, p: 0.5},  // 'quite' (679) and 'question' (644) are included!
          {sample: {insert: 'a', deleteLeft: 0}, p: 0.4}   // but at lower weight than 'and' (998).
        ];

        await compositor.predict({insert: '', deleteLeft: 0}, context); // Initialize context tracking first!
        let suggestions = await compositor.predict(inputDistribution, context);

        // remove the keep suggestion; we're not testing that here.
        suggestions = suggestions.filter((suggestion) => suggestion.tag != 'keep');

        suggestions.sort((a, b) => b.p - a.p);

        // There are only 4 suggestions in this limited test model that begin with 'q'.
        // We expect more than that, since 'a' is indicated to be very close by.
        assert.isAbove(suggestions.length, 4, "fat-finger style corrections needed for test comparisons are missing");

        // Note:  'and' is (currently) modeled by the text-fixture model to have 9.3x the base probability
        // that the worst 'q'-rooted suggestion ('quality') does.  Without single-character correction
        // avoidance logic, this test _will_ fail.
        //
        // In case a tweak to test parameters is desired, note that 'and' beats rank #3 - 'questions' -
        // at 3.36x base.  At the time of writing this test, upping 'a's probability to 0.45 will block
        // 'quality' while the top three 'q's (ending with 'questions') remain in place.
        let qRange = suggestions.slice(0, 4);
        assert.isUndefined(qRange.find((suggestion) => suggestion.transform.insert.charAt(0) != 'q'));

        let aRange = suggestions.slice(4);
        assert.isUndefined(aRange.find((suggestion) => suggestion.transform.insert.charAt(0) == 'q'));
      });

      it('properly handles suggestions after a backspace', async function() {
        let compositor = new ModelCompositor(plainModel, true);
        let context = {
          left: 'the ', startOfBuffer: true, endOfBuffer: true,
        };

        let inputTransform = {
          insert: '',
          deleteLeft: 1
        };

        let suggestions = await compositor.predict(inputTransform, context);
        suggestions.forEach(function(suggestion) {
          // Suggestions always delete the full root of the suggestion.
          //
          // After a backspace, that means the text 'the' - 3 chars.
          // Char 4 is for the original backspace, as suggestions are built
          // based on the context state BEFORE the triggering input -
          // here, a backspace.
          assert.equal(suggestion.transform.deleteLeft, 4);
        });
      });

      it('properly handles suggestions for the first letter after a ` `', async function() {
        let compositor = new ModelCompositor(plainModel, true);
        let context = {
          left: 'the', startOfBuffer: true, endOfBuffer: true,
        };

        let inputTransform = {
          insert: ' ',
          deleteLeft: 0
        };

        let suggestions = await compositor.predict(inputTransform, context);
        suggestions.forEach(function(suggestion) {
          // After a space, predictions are based on a new, zero-length root.
          // With nothing to replace, .deleteLeft should be zero.
          assert.equal(suggestion.transform.deleteLeft, 0);
        });
      });

      it('properly handles suggestions for the first letter after a `\'`', async function() {
        let compositor = new ModelCompositor(plainModel, true);
        let context = {
          left: "the '", startOfBuffer: true, endOfBuffer: true,
        };

        // This results in a new word boundary (between the `'` and the `a`).
        // Basically, an implied (but nonexistent) ` `.
        let inputTransform = {
          insert: "a",
          deleteLeft: 0
        };

        let suggestions = await compositor.predict(inputTransform, context);
        suggestions.forEach(function(suggestion) {
          // Suggestions always delete the full root of the suggestion.
          // Which, here, didn't exist before the input.  Nothing to
          // replace => nothing for the suggestion to delete.
          assert.equal(suggestion.transform.deleteLeft, 0);
        });
      });

      it('stops predicting early if new prediction request comes in', async function() {
        const compositor = new ModelCompositor(plainModel, true);

        const firstPredict = compositor.predict({
          insert: "a",
          deleteLeft: 0
        }, {
          left: "the ", startOfBuffer: true, endOfBuffer: true
        });

        const firstTimer = compositor.activeTimer;
        assert.isOk(firstTimer);
        assert.isFalse(firstTimer.elapsed);

        const secondPredict = compositor.predict({
          insert: "l",
          deleteLeft: 0
        }, {
          left: "the apl", startOfBuffer: true, endOfBuffer: true
        });

        assert.notEqual(compositor.activeTimer, firstTimer);
        assert.isTrue(firstTimer.elapsed);

        await Promise.race([
          firstPredict,
          secondPredict.then(() => Promise.reject(new Error("second prediction should not beat the first"))),
        ]);

        await Promise.all([firstPredict, secondPredict]);

        // We can't make many solid guarantees about the state at which the first predict()
        // call was interrupted.
        //
        // Possible cases:
        // - an early OS-level context switch can land between processing the root search node
        //   and the first possible search result (even for a single char)
        // - It's possible to interrupt after the first result (exact match) and before any
        //   secondary corrections may be found
        // - It's possible to interrupt "too late" if the correction search proceeds quickly,
        //   returning a standard full set.
        await firstPredict;
        const finalSuggestions = await secondPredict;

        assert.isOk(finalSuggestions.find((entry) => entry.displayAs == 'applied'));
      });
    });

    describe('applySuggestionCasing', function() {
      let plainApplyCasing = function(caseToApply, text) {
        switch(caseToApply) {
          case 'lower':
            return text.toLowerCase();
          case 'upper':
            return text.toUpperCase();
          case 'initial':
            return plainApplyCasing('upper', text.charAt(0)) . concat(text.substring(1));
          default:
            return text;
        }
      };

      var plainCasedModel = new TrieModel(
        jsonFixture('models/tries/english-1000'), {
          languageUsesCasing: true,
          applyCasing: plainApplyCasing,
          wordBreaker: wordBreakers.default,
          searchTermToKey: function(text) {
            // We're dealing with very simple English text; no need to normalize or remove diacritics here.
            return applyCasing('lower', text);
          }
        }
      );

      it('properly cases suggestions with no suggestion root', function() {
        var compositor = new ModelCompositor(plainCasedModel, true);

        let suggestion = {
          transform: {
            insert: 'the',
            deleteLeft: 0
          },
          displayAs: 'the'
        };

        compositor.applySuggestionCasing(suggestion, '', 'initial');
        assert.equal(suggestion.displayAs, 'The');
        assert.equal(suggestion.transform.insert, 'The');

        suggestion = {
          transform: {
            insert: 'thE',
            deleteLeft: 0
          },
          displayAs: 'thE'
        };

        compositor.applySuggestionCasing(suggestion, '', 'initial');
        assert.equal(suggestion.displayAs, 'ThE');
        assert.equal(suggestion.transform.insert, 'ThE');

        suggestion = {
          transform: {
            insert: 'the',
            deleteLeft: 0
          },
          displayAs: 'the'
        };

        compositor.applySuggestionCasing(suggestion, '', 'upper');
        assert.equal(suggestion.displayAs, 'THE');
        assert.equal(suggestion.transform.insert, 'THE');
      });

      it('properly cases suggestions that fully replace the suggestion root', function() {
        var compositor = new ModelCompositor(plainCasedModel, true);

        let suggestion = {
          transform: {
            insert: 'therefore',
            deleteLeft: 3
          },
          displayAs: 'therefore'
        };

        compositor.applySuggestionCasing(suggestion, 'the', 'initial');
        assert.equal(suggestion.displayAs, 'Therefore');
        assert.equal(suggestion.transform.insert, 'Therefore');

        suggestion = {
          transform: {
            insert: 'thereFore',
            deleteLeft: 3
          },
          displayAs: 'thereFore'
        };

        compositor.applySuggestionCasing(suggestion, 'the', 'initial');
        assert.equal(suggestion.displayAs, 'ThereFore');
        assert.equal(suggestion.transform.insert, 'ThereFore');

        suggestion = {
          transform: {
            insert: 'therefore',
            deleteLeft: 3
          },
          displayAs: 'therefore'
        };

        compositor.applySuggestionCasing(suggestion, 'the', 'upper');
        assert.equal(suggestion.displayAs, 'THEREFORE');
        assert.equal(suggestion.transform.insert, 'THEREFORE');
      });

      it('properly cases suggestions that do not fully replace the suggestion root', function() {
        var compositor = new ModelCompositor(plainCasedModel, true);

        let suggestion = {
          transform: {
            insert: 'erefore',
            deleteLeft: 1
          },
          displayAs: 'therefore'
        };

        // When integrated, the 'the' string comes from a wordbreak operation on the current context.
        compositor.applySuggestionCasing(suggestion, 'the', 'initial');
        assert.equal(suggestion.displayAs, 'Therefore');
        assert.equal(suggestion.transform.insert, 'Therefore');

        suggestion = {
          transform: {
            insert: 'ereFore',
            deleteLeft: 1
          },
          displayAs: 'thereFore'
        };

        compositor.applySuggestionCasing(suggestion, 'the', 'initial');
        assert.equal(suggestion.displayAs, 'ThereFore');
        assert.equal(suggestion.transform.insert, 'ThereFore');

        suggestion = {
          transform: {
            insert: 'erefore',
            deleteLeft: 1
          },
          displayAs: 'therefore'
        };

        compositor.applySuggestionCasing(suggestion, 'the', 'upper');
        assert.equal(suggestion.displayAs, 'THEREFORE');
        assert.equal(suggestion.transform.insert, 'THEREFORE');
      });
    });

    describe('Model uses default-style keying, no casing', function () {
      var uncasedModel = new TrieModel(
        jsonFixture('models/tries/english-1000'), {
          languageUsesCasing: false,
          wordBreaker: wordBreakers.default,
          searchTermToKey: function(text) {
            // We're dealing with very simple English text; without casing, there's no keying to be done
            // for such simple data.  No need to normalize or remove diacritics here.
            return text;
          }
        }
      );

      it('should produce suggestions from uncased input', async function() {
        let model = uncasedModel;
        var compositor = new ModelCompositor(model, true);

        // Initialize context
        let context = {
          left: 'th', startOfBuffer: false, endOfBuffer: true,
        };
        await compositor.predict({insert: '', deleteLeft: 0}, context);

        // Pretend to fat finger "the" as "thr"
        var the = { sample: { insert: 'r', deleteLeft: 0}, p: 0.45 };
        var thr = { sample: { insert: 'e', deleteLeft: 0}, p: 0.55 };
        var suggestions = await compositor.predict([thr, the], context);

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

      it('should not produce suggestions from cased input', async function() {
        let model = uncasedModel;
        var compositor = new ModelCompositor(model, true);

        // Initialize context
        let context = {
          left: 'TH', startOfBuffer: false, endOfBuffer: true,
        };
        await compositor.predict({insert: '', deleteLeft: 0}, context);

        // Pretend to fat finger "the" as "thr"
        var the = { sample: { insert: 'R', deleteLeft: 0}, p: 0.45 };
        var thr = { sample: { insert: 'E', deleteLeft: 0}, p: 0.55 };
        var suggestions = await compositor.predict([thr, the], context);

        // We should only receive a 'keep' suggestion.
        assert.equal(suggestions.length, 1);
        assert.equal(suggestions[0].tag, 'keep');
      });
    });

    describe('Model uses default-style keying, provides casing', function () {
      let applyCasing = function(caseToApply, text) {
        switch(caseToApply) {
          case 'lower':
            return text.toLowerCase();
          case 'upper':
            return text.toUpperCase();
          case 'initial':
            return applyCasing('upper', text.charAt(0)) . concat(text.substring(1));
          default:
            return text;
        }
      };

      var casedModel = new TrieModel(
        jsonFixture('models/tries/english-1000'), {
          languageUsesCasing: true,
          applyCasing: applyCasing,
          wordBreaker: wordBreakers.default,
          searchTermToKey: function(text) {
            // We're dealing with very simple English text; no need to normalize or remove diacritics here.
            return applyCasing('lower', text);
          }
        }
      );

      it('should produce suggestions from uncased input', async function() {
        let model = casedModel;
        let compositor = new ModelCompositor(model, true);

        // Initialize context
        let context = {
          left: 'th', startOfBuffer: false, endOfBuffer: true,
        };
        await compositor.predict({insert: '', deleteLeft: 0}, context);

        // Pretend to fat finger "the" as "thr"
        var the = { sample: { insert: 'r', deleteLeft: 0}, p: 0.45 };
        var thr = { sample: { insert: 'e', deleteLeft: 0}, p: 0.55 };
        var suggestions = await compositor.predict([thr, the], context);

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

      it('should produce capitalized suggestions from fully-uppercased input', async function() {
        let model = casedModel;
        let compositor = new ModelCompositor(model, true);

        // Initialize context
        let context = {
          left: 'TH', startOfBuffer: false, endOfBuffer: true,
        };
        await compositor.predict({insert: '', deleteLeft: 0}, context);

        // Pretend to fat finger "the" as "thr"
        var the = { sample: { insert: 'R', deleteLeft: 0}, p: 0.45 };
        var thr = { sample: { insert: 'E', deleteLeft: 0}, p: 0.55 };
        var suggestions = await compositor.predict([thr, the], context);

        // Get the top suggest for 'the' and 'thr*'.
        var theSuggestion = suggestions.filter(function (s) { return s.displayAs === 'THE' || s.displayAs === '“THE”'; })[0];
        var thrSuggestion = suggestions.filter(function (s) { return s.displayAs.startsWith('THR'); })[0];

        // Sanity check: do we have actual real-valued probabilities?
        assert.isAbove(thrSuggestion.p, 0.0);
        assert.isBelow(thrSuggestion.p, 1.0);
        assert.isAbove(theSuggestion.p, 0.0);
        assert.isBelow(theSuggestion.p, 1.0);
        // 'the' should be the intended the result here.
        assert.isAbove(theSuggestion.p, thrSuggestion.p);
      });

      it('should produce "initial-case" suggestions from input with an initial capital', async function() {
        let model = casedModel;
        let compositor = new ModelCompositor(model, true);

        // Initialize context
        let context = {
          left: 'Th', startOfBuffer: false, endOfBuffer: true,
        };
        await compositor.predict({insert: '', deleteLeft: 0}, context);

        // Pretend to fat finger "the" as "thr"
        var the = { sample: { insert: 'r', deleteLeft: 0}, p: 0.45 };
        var thr = { sample: { insert: 'e', deleteLeft: 0}, p: 0.55 };
        var suggestions = await compositor.predict([thr, the], context);

        // Get the top suggest for 'the' and 'thr*'.
        var theSuggestion = suggestions.filter(function (s) { return s.displayAs === 'The' || s.displayAs === '“The”'; })[0];
        var thrSuggestion = suggestions.filter(function (s) { return s.displayAs.startsWith('Thr'); })[0];

        // Sanity check: do we have actual real-valued probabilities?
        assert.isAbove(thrSuggestion.p, 0.0);
        assert.isBelow(thrSuggestion.p, 1.0);
        assert.isAbove(theSuggestion.p, 0.0);
        assert.isBelow(theSuggestion.p, 1.0);
        // 'the' should be the intended the result here.
        assert.isAbove(theSuggestion.p, thrSuggestion.p);
      });

      it('also from input with partial capitalization when including an initial capital', async function() {
        let model = casedModel;
        let compositor = new ModelCompositor(model, true);

        // Initialize context
        let context = {
          left: 'TH', startOfBuffer: false, endOfBuffer: true,
        };
        await compositor.predict({insert: '', deleteLeft: 0}, context);

        // Pretend to fat finger "the" as "thr"
        var the = { sample: { insert: 'r', deleteLeft: 0}, p: 0.45 };
        var thr = { sample: { insert: 'e', deleteLeft: 0}, p: 0.55 };
        var suggestions = await compositor.predict([thr, the], context);

        // Get the top suggest for 'the' and 'thr*'.
        var theSuggestion = suggestions.filter(function (s) { return s.displayAs.startsWith('The'); })[0];
        var thrSuggestion = suggestions.filter(function (s) { return s.displayAs.startsWith('Thr'); })[0];

        // Sanity check: do we have actual real-valued probabilities?
        assert.isAbove(thrSuggestion.p, 0.0);
        assert.isBelow(thrSuggestion.p, 1.0);
        assert.isAbove(theSuggestion.p, 0.0);
        assert.isBelow(theSuggestion.p, 1.0);
      });
    });
  });

  describe('Prediction with legacy Models (12.0 / 13.0)', function() {
    it('should compose suggestions from a fat-fingered keypress (no keying needed)', async function () {
      var model = new TrieModel(
        jsonFixture('models/tries/english-1000')
      );
      let compositor = new ModelCompositor(model, true);

      // Initialize context
      let context = {
        left: 'th', startOfBuffer: false, endOfBuffer: true,
      };
      await compositor.predict({insert: '', deleteLeft: 0}, context);

      // Pretend to fat finger "the" as "thr"
      var the = { sample: { insert: 'r', deleteLeft: 0}, p: 0.45 };
      var thr = { sample: { insert: 'e', deleteLeft: 0}, p: 0.55 };
      var suggestions = await compositor.predict([thr, the], context);

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

    it('should compose suggestions from a fat-fingered keypress (keying needed)', async function () {
      var model = new TrieModel(
        jsonFixture('models/tries/english-1000')
      );
      let compositor = new ModelCompositor(model, true);

      // Initialize context
      let context = {
        left: 'Th', startOfBuffer: false, endOfBuffer: true,
      };
      await compositor.predict({insert: '', deleteLeft: 0}, context);

      // Pretend to fat finger "the" as "thr"
      var the = { sample: { insert: 'r', deleteLeft: 0}, p: 0.45 };
      var thr = { sample: { insert: 'e', deleteLeft: 0}, p: 0.55 };
      var suggestions = await compositor.predict([thr, the], context);

      // Get the top suggest for 'the' and 'thr*'.
      // As of 15.0+, because of #5429, the keep suggestion `"The"` will not be merged
      // with the model's suggestion of `the`.
      var capTheSuggestion = suggestions.filter(function (s) { return s.displayAs === '“The”'; })[0];
      var theSuggestion = suggestions.filter(function (s) { return s.displayAs === 'the'})[0];
      var thrSuggestion = suggestions.filter(function (s) { return s.displayAs.startsWith('thr'); })[0];

      // Sanity check: do we have actual real-valued probabilities?
      assert.isAbove(thrSuggestion.p, 0.0);
      assert.isBelow(thrSuggestion.p, 1.0);
      assert.isAbove(theSuggestion.p, 0.0);
      assert.isBelow(theSuggestion.p, 1.0);
      assert.equal(capTheSuggestion.tag, 'keep'); // The keep suggestion
      // 'the' should be the intended the result here.
      assert.isAbove(theSuggestion.p, thrSuggestion.p);
    });
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

        var keep;
        if(quoteStyle) {
          keep = toAnnotatedSuggestion(model, baseSuggestion, 'keep', quoteStyle);
        } else {
          keep = toAnnotatedSuggestion(model, baseSuggestion, 'keep');
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

  describe('acceptSuggestion', function() {
    let acceptanceTest = function(punctuation, suggestion, context, postTransform) {
      let options = {
        punctuation: punctuation
      };

      let model = new models.DummyModel(options);
      let compositor = new ModelCompositor(model, true);

      return compositor.acceptSuggestion(suggestion, context, postTransform);
    }

    let englishPunctuation = {
      quotesForKeepSuggestion: { open: `“`, close: `”`},
      insertAfterWord: ' '
    };

    let angledPunctuation = {
      quotesForKeepSuggestion: { open: `«`, close: `»`},
      insertAfterWord: " "
    }

    it('first word of context, postTransform provided, .deleteLeft = 0', function() {
      let baseSuggestion = {
        transform: {
          insert: 'hello ',
          deleteLeft: 2,
          id: 0
        },
        transformId: 0,
        id: 1,
        displayAs: 'hello'
      };

      let baseContext = {
        left: 'he', startOfBuffer: true, endOfBuffer: true
      }

      // Represents the keystroke that triggered the suggestion.  It's not technically part
      // of the Context when the suggestion is built.
      let postTransform = {
        insert: 'l',
        deleteLeft: 0
      }

      let reversion = acceptanceTest(englishPunctuation, baseSuggestion, baseContext, postTransform);
      assert.equal(reversion.transformId, baseSuggestion.transformId);
      assert.equal(reversion.id, -baseSuggestion.id);

      // Check #1:  Does the returned reversion properly revert the context to its pre-application state?
      //            Does this include characters not considered when the Suggestion was built?
      let unappliedContext = models.applyTransform(postTransform, baseContext);
      let appliedContext = models.applyTransform(baseSuggestion.transform, baseContext);
      assert.equal(appliedContext.left, "hello ");

      let revertedContext = models.applyTransform(reversion.transform, appliedContext);
      assert.deepEqual(revertedContext, unappliedContext);

      // Check #2:  Are the correct display strings built, depending on the active model's punctuation?
      assert.equal(reversion.displayAs, "“hel”"); // text should _basically_ be a quoted version of `preApplyContext.left`

      let angledReversion = acceptanceTest(angledPunctuation, baseSuggestion, baseContext, postTransform);
      assert.equal(angledReversion.displayAs, "«hel»");
    });

    it('second word of context, postTransform provided, .deleteLeft = 0', function() {
      let baseSuggestion = {
        transform: {
          insert: 'world ',
          deleteLeft: 3,
          id: 0
        },
        transformId: 0,
        id: 0,
        displayAs: 'world'
      };

      let baseContext = {
        left: 'hello wot', startOfBuffer: true, endOfBuffer: true
      }

      // Represents the keystroke that triggered the suggestion.  It's not technically part
      // of the Context when the suggestion is built.
      let postTransform = {
        insert: 'l',
        deleteLeft: 0
      }

      let reversion = acceptanceTest(englishPunctuation, baseSuggestion, baseContext, postTransform);
      assert.equal(reversion.transformId, baseSuggestion.transformId);
      assert.equal(reversion.id, -baseSuggestion.id);

      // Check #1:  Does the returned reversion properly revert the context to its pre-application state?
      //            Does this include characters not considered when the Suggestion was built?
      let unappliedContext = models.applyTransform(postTransform, baseContext);
      let appliedContext = models.applyTransform(baseSuggestion.transform, baseContext);
      assert.equal(appliedContext.left, "hello world ");

      let revertedContext = models.applyTransform(reversion.transform, appliedContext);
      assert.deepEqual(revertedContext, unappliedContext);

      // Check #2:  Are the correct display strings built, depending on the active model's punctuation?
      assert.equal(reversion.displayAs, "“wotl”"); // text should _basically_ be a quoted version of `preApplyContext.left`

      let angledReversion = acceptanceTest(angledPunctuation, baseSuggestion, baseContext, postTransform);
      assert.equal(angledReversion.displayAs, "«wotl»");
    });

    it('second word of context, postTransform undefined', function() {
      let baseSuggestion = {
        transform: {
          insert: 'world ',
          deleteLeft: 3,
          id: 0
        },
        transformId: 0,
        id: 0,
        displayAs: 'world'
      };

      let baseContext = {
        left: 'hello wot', startOfBuffer: true, endOfBuffer: true
      }

      let reversion = acceptanceTest(englishPunctuation, baseSuggestion, baseContext);
      assert.equal(reversion.transformId, baseSuggestion.transformId);
      assert.equal(reversion.id, -baseSuggestion.id);

      // Check #1:  Does the returned reversion properly revert the context to its pre-application state?
      //            Does this include characters not considered when the Suggestion was built?
      let unappliedContext = models.applyTransform({insert: '', deleteLeft: 0}, baseContext); // to clone the original context.
      let appliedContext = models.applyTransform(baseSuggestion.transform, baseContext);
      assert.equal(appliedContext.left, "hello world ");

      let revertedContext = models.applyTransform(reversion.transform, appliedContext);
      assert.deepEqual(revertedContext, unappliedContext);

      // Check #2:  Are the correct display strings built, depending on the active model's punctuation?
      assert.equal(reversion.displayAs, "“wot”"); // text should _basically_ be a quoted version of `preApplyContext.left`

      let angledReversion = acceptanceTest(angledPunctuation, baseSuggestion, baseContext);
      assert.equal(angledReversion.displayAs, "«wot»");
    });

    it('first word of context + postTransform provided, .deleteLeft > 0', function() {
      let baseSuggestion = {
        transform: {
          insert: 'hello ',
          deleteLeft: 2,
          id: 0
        },
        transformId: 0,
        id: 0,
        displayAs: 'hello'
      };

      let baseContext = {
        left: 'he', startOfBuffer: true, endOfBuffer: true
      }

      // Represents the keystroke that triggered the suggestion.  It's not technically part
      // of the Context when the suggestion is built.
      let postTransform = {
        insert: 'i',
        deleteLeft: 1
      }

      let reversion = acceptanceTest(englishPunctuation, baseSuggestion, baseContext, postTransform);
      assert.equal(reversion.transformId, baseSuggestion.transformId);
      assert.equal(reversion.id, -baseSuggestion.id);

      // Check #1:  Does the returned reversion properly revert the context to its pre-application state?
      //            Does this include characters not considered when the Suggestion was built?
      let unappliedContext = models.applyTransform(postTransform, baseContext);
      let appliedContext = models.applyTransform(baseSuggestion.transform, baseContext);
      assert.equal(appliedContext.left, "hello ");

      let revertedContext = models.applyTransform(reversion.transform, appliedContext);
      assert.deepEqual(revertedContext, unappliedContext);

      // Check #2:  Are the correct display strings built, depending on the active model's punctuation?
      assert.equal(reversion.displayAs, "“hi”"); // text should _basically_ be a quoted version of `preApplyContext.left`

      let angledReversion = acceptanceTest(angledPunctuation, baseSuggestion, baseContext, postTransform);
      assert.equal(angledReversion.displayAs, "«hi»");
    });
  });

  describe('acceptReversion', function() {
    let executeAcceptance = function(model, suggestion, context, postTransform) {
      let compositor = new ModelCompositor(model, true);

      return {compositor: compositor, reversion: compositor.acceptSuggestion(suggestion, context, postTransform)};
    }

    let englishPunctuation = {
      quotesForKeepSuggestion: { open: `“`, close: `”`},
      insertAfterWord: ' '
    };

    // While this isn't a state the LMLayer should ever operate within, this provides
    // a useful base state for developing further tests against the method.
    it('model without traversals: returns appropriate suggestions upon reversion', async function() {
      // This setup matches 'acceptSuggestion' the test case
      // it('first word of context + postTransform provided, .deleteLeft > 0')
      // seen earlier in the file.

      let baseSuggestion = {
        transform: {
          insert: 'hello ',
          deleteLeft: 2,
          id: 0
        },
        transformId: 0,
        id: 0,
        displayAs: 'hello'
      };

      let baseContext = {
        left: 'he', startOfBuffer: true, endOfBuffer: true
      }

      // Represents the keystroke that triggered the suggestion.  It's not technically part
      // of the Context when the suggestion is built.
      let postTransform = {
        insert: 'i',
        deleteLeft: 1
      }

      let model = new models.DummyModel({punctuation: englishPunctuation});
      let compositor = new ModelCompositor(model, true);

      let reversion = compositor.acceptSuggestion(baseSuggestion, baseContext, postTransform);
      assert.equal(reversion.transformId, baseSuggestion.transformId);
      assert.equal(reversion.id, -baseSuggestion.id);

      let appliedContext = models.applyTransform(baseSuggestion.transform, baseContext);
      assert.equal(appliedContext.left, "hello ");

      let suggestions = await compositor.applyReversion(reversion, appliedContext);

      // As this test is a bit... 'hard-wired', we only get the 'keep' suggestion.
      // It should still be accurate, though.
      assert.equal(suggestions.length, 1);

      let expectedTransform = {
        insert: 'hi ',  // Keeps current context the same, though it adds a wordbreak.
        deleteLeft: 2
      }
      assert.deepEqual(suggestions[0].transform, expectedTransform);
    });

    it('model with traversals: returns appropriate suggestions upon reversion', async function() {
      // This setup matches 'acceptSuggestion' the test case
      // it('first word of context + postTransform provided, .deleteLeft > 0')
      // seen earlier in the file.

      let baseContext = {
        left: 'he', startOfBuffer: true, endOfBuffer: true
      }

      // Represents the keystroke that triggered the suggestion.  It's not technically part
      // of the Context when the suggestion is built.
      let postTransform = {
        insert: 'i',
        deleteLeft: 1,
        id: 13
      }

      let model = new models.TrieModel(jsonFixture('models/tries/english-1000'), {punctuation: englishPunctuation});
      let compositor = new ModelCompositor(model, true);

      let initialSuggestions = await compositor.predict(postTransform, baseContext);
      let keepSuggestion = initialSuggestions[0];
      assert.equal(keepSuggestion.tag, 'keep'); // corresponds to `postTransform`, but the transform isn't equal.

      let baseSuggestion = initialSuggestions[1];
      let reversion = compositor.acceptSuggestion(baseSuggestion, baseContext, postTransform);
      assert.equal(reversion.transformId, -baseSuggestion.transformId);
      assert.equal(reversion.id, -baseSuggestion.id);

      let appliedContext = models.applyTransform(baseSuggestion.transform, baseContext);
      let reversionSuggestions = await compositor.applyReversion(reversion, appliedContext);

      // The returned suggestion list should match the original suggestion list.
      assert.deepEqual(reversionSuggestions, initialSuggestions);
    });

    it('model with traversals: properly tracks context state', async function() {
      // Could be merged with the previous test case, but I think it's good to have the error
      // sets flagged separately.

      let baseContext = {
        left: 'he', startOfBuffer: true, endOfBuffer: true
      }

      // Represents the keystroke that triggered the suggestion.  It's not technically part
      // of the Context when the suggestion is built.
      let postTransform = {
        insert: 'i',
        deleteLeft: 1,
        id: 13
      }

      let model = new models.TrieModel(jsonFixture('models/tries/english-1000'), {punctuation: englishPunctuation});
      let compositor = new ModelCompositor(model, true);

      let initialSuggestions = await compositor.predict(postTransform, baseContext);
      const suggestionContextState = compositor.contextTracker.newest;

      let keepSuggestion = initialSuggestions[0];
      assert.equal(keepSuggestion.tag, 'keep'); // corresponds to `postTransform`, but the transform isn't equal.

      // One for base state, before the transform...
      // one for after, since it makes an edit.
      assert.equal(compositor.contextTracker.count, 2);

      let baseSuggestion = initialSuggestions[1];
      let reversion = compositor.acceptSuggestion(baseSuggestion, baseContext, postTransform);
      assert.equal(reversion.transformId, -baseSuggestion.transformId);
      assert.equal(reversion.id, -baseSuggestion.id);

      // Accepting the suggestion adds an extra context state.
      assert.equal(compositor.contextTracker.count, 3);

      // The replacement should be marked on the context-tracking token.
      assert.isOk(suggestionContextState.tail.replacement);

      let appliedContext = models.applyTransform(baseSuggestion.transform, baseContext);
      compositor.applyReversion(reversion, appliedContext);

      // Reverting the suggestion should remove that extra state.
      assert.equal(compositor.contextTracker.count, 2);
      assert.equal(compositor.contextTracker.item(1), suggestionContextState);

      // The replacement should no longer be marked for the context-tracking token.
      assert.isNotOk(suggestionContextState.tail.replacement);
    });
  });
});
