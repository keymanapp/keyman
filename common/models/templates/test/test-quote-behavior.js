/*
 * Unit tests for quote behaviors.
 */

import { assert } from 'chai';
import { QuoteBehavior } from '@keymanapp/models-templates';

describe('Quote behaviors', function() {
  describe('Script directionality', function() {
    it('(default)', function() {
      let englishPunctuation = {
        quotesForKeepSuggestion: { open: `“`, close: `”`},
        insertAfterWord: " "
      }

      var plainQuotedHello;
      plainQuotedHello = QuoteBehavior.apply(QuoteBehavior.useQuotes, "hello", englishPunctuation, QuoteBehavior.useQuotes);
      assert.equal(plainQuotedHello, "“hello”");
    });

    it('LTR', function() {
      let englishPunctuation = {
        quotesForKeepSuggestion: { open: `“`, close: `”`},
        insertAfterWord: " ",
        isRTL: false
      }

      var plainQuotedHello;
      plainQuotedHello = QuoteBehavior.apply(QuoteBehavior.useQuotes, "hello", englishPunctuation, QuoteBehavior.useQuotes);
      assert.equal(plainQuotedHello, "“hello”");
    });

    it.skip('RTL', function() {
      let englishPunctuation = {
        quotesForKeepSuggestion: { open: `”`, close: `“`},
        insertAfterWord: " ",
        isRTL: false
      }

      var plainQuotedHello;
      plainQuotedHello = QuoteBehavior.apply(QuoteBehavior.useQuotes, "hello", englishPunctuation, QuoteBehavior.useQuotes);
      assert.equal(plainQuotedHello, "”hello“");

      // That said... it's very hard to get an assertion in place regarding the
      // resulting visual appearance of this test.
    });
  });

  describe('Application', function() {
    it('.useQuotes', function() {
      let englishPunctuation = {
        quotesForKeepSuggestion: { open: `“`, close: `”`},
        insertAfterWord: " "
      }

      var plainQuotedHello;
      plainQuotedHello = QuoteBehavior.apply(QuoteBehavior.useQuotes, "hello", englishPunctuation, QuoteBehavior.useQuotes);
      assert.equal(plainQuotedHello, "“hello”");

      plainQuotedHello = QuoteBehavior.apply(QuoteBehavior.useQuotes, "hello", englishPunctuation, QuoteBehavior.noQuotes);
      assert.equal(plainQuotedHello, "“hello”");

      let angleQuotePunctuation = {
        quotesForKeepSuggestion: { open: `«`, close: `»`},
        insertAfterWord: " "
      }

      var angleQuotedHello;
      angleQuotedHello = QuoteBehavior.apply(QuoteBehavior.useQuotes, "hello", angleQuotePunctuation, QuoteBehavior.useQuotes);
      assert.equal(angleQuotedHello, "«hello»");

      angleQuotedHello = QuoteBehavior.apply(QuoteBehavior.useQuotes, "hello", angleQuotePunctuation, QuoteBehavior.noQuotes);
      assert.equal(angleQuotedHello, "«hello»");
    });

    it('.noQuotes', function() {
      let englishPunctuation = {
        quotesForKeepSuggestion: { open: `“`, close: `”`},
        insertAfterWord: " "
      }

      var plainQuotedHello;
      plainQuotedHello = QuoteBehavior.apply(QuoteBehavior.noQuotes, "hello", englishPunctuation, QuoteBehavior.useQuotes);
      assert.equal(plainQuotedHello, "hello");

      plainQuotedHello = QuoteBehavior.apply(QuoteBehavior.noQuotes, "hello", englishPunctuation, QuoteBehavior.noQuotes);
      assert.equal(plainQuotedHello, "hello");

      let angleQuotePunctuation = {
        quotesForKeepSuggestion: { open: `«`, close: `»`},
        insertAfterWord: " "
      }

      var angleQuotedHello;
      angleQuotedHello = QuoteBehavior.apply(QuoteBehavior.noQuotes, "hello", angleQuotePunctuation, QuoteBehavior.useQuotes);
      assert.equal(angleQuotedHello, "hello");

      angleQuotedHello = QuoteBehavior.apply(QuoteBehavior.noQuotes, "hello", angleQuotePunctuation, QuoteBehavior.noQuotes);
      assert.equal(angleQuotedHello, "hello");
    });

    it('.default -> .useQuotes', function() {
      let englishPunctuation = {
        quotesForKeepSuggestion: { open: `“`, close: `”`},
        insertAfterWord: " "
      }

      var plainQuotedHello;
      plainQuotedHello = QuoteBehavior.apply(QuoteBehavior.default, "hello", englishPunctuation, QuoteBehavior.useQuotes);
      assert.equal(plainQuotedHello, "“hello”");

      let angleQuotePunctuation = {
        quotesForKeepSuggestion: { open: `«`, close: `»`},
        insertAfterWord: " "
      }

      var angleQuotedHello;
      angleQuotedHello = QuoteBehavior.apply(QuoteBehavior.default, "hello", angleQuotePunctuation, QuoteBehavior.useQuotes);
      assert.equal(angleQuotedHello, "«hello»");
    });

    it('.default -> .noQuotes', function() {
      let englishPunctuation = {
        quotesForKeepSuggestion: { open: `“`, close: `”`},
        insertAfterWord: " "
      }

      var plainQuotedHello;
      plainQuotedHello = QuoteBehavior.apply(QuoteBehavior.default, "hello", englishPunctuation, QuoteBehavior.noQuotes);
      assert.equal(plainQuotedHello, "hello");

      let angleQuotePunctuation = {
        quotesForKeepSuggestion: { open: `«`, close: `»`},
        insertAfterWord: " "
      }

      var angleQuotedHello;
      angleQuotedHello = QuoteBehavior.apply(QuoteBehavior.default, "hello", angleQuotePunctuation, QuoteBehavior.noQuotes);
      assert.equal(angleQuotedHello, "hello");
    });

    it('Error case:  .default as fall-back behavior', function() {
      let englishPunctuation = {
        quotesForKeepSuggestion: { open: `“`, close: `”`},
        insertAfterWord: " "
      }

      assert.throws(function() {
        QuoteBehavior.apply(QuoteBehavior.default, "hello", englishPunctuation, QuoteBehavior.default);
      });

      assert.throws(function() {
        QuoteBehavior.apply(QuoteBehavior.useQuotes, "hello", englishPunctuation, QuoteBehavior.default);
      });

      assert.throws(function() {
        QuoteBehavior.apply(QuoteBehavior.noQuotes, "hello", englishPunctuation, QuoteBehavior.default);
      });
    });
  });
});