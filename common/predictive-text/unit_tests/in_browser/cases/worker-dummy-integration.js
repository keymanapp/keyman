import { assert } from '../../../../../node_modules/chai/chai.js';

import { LMLayer, Worker }   from "../../../build/lib/web/index.mjs";
import * as helpers from "../helpers.mjs";

/*
 * Shows off the LMLayer API, using the full prediction interface.
 * The dummy model (or mock model) is a language model which has
 * **injectable** suggestions: that is, you, as the tester, have
 * to provide the predictions. The dummy model does not create any
 * suggestions on its own. The dummy model can take in a series
 * of suggestions when loaded and return them sequentially.
 */
describe('LMLayer using dummy model', function () {
  this.timeout(testconfig.timeouts.standard);

  describe('Prediction', function () {
    it('will predict future suggestions', function () {
      this.timeout(testconfig.timeouts.standard * 3); // This one makes multiple subsequent calls across
                                                      // the WebWorker boundary, so we should be generous here.

      var lmLayer = new LMLayer(helpers.defaultCapabilities, Worker.constructInstance(), true);

      var stripIDs = function(suggestions) {
        suggestions.forEach(function(suggestion) {
          delete suggestion.id;
        });
      }

      // We're testing many as asynchronous messages in a row.
      // this would be cleaner using async/await syntax, but
      // alas some of our browsers don't support it.
      return lmLayer.loadModel(
        // We need to provide an absolute path since the worker is based within a blob.
        document.location.protocol + '//' + document.location.host + "/resources/models/simple-dummy.js"
      ).then(function (actualConfiguration) {
        return Promise.resolve();
      }).then(function () {
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        stripIDs(suggestions);
        assert.deepEqual(suggestions, iGotDistractedByHazel()[0]);
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        stripIDs(suggestions);
        assert.deepEqual(suggestions, iGotDistractedByHazel()[1]);
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        stripIDs(suggestions);
        assert.deepEqual(suggestions, iGotDistractedByHazel()[2]);
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        stripIDs(suggestions);
        assert.deepEqual(suggestions, iGotDistractedByHazel()[3]);
        lmLayer.shutdown();
        return Promise.resolve();
      });
    });
  });

  describe('Wordbreaking', function () {
    it('will perform (default) wordbreaking and return word at caret', function () {
      this.timeout(testconfig.timeouts.standard * 3); // This one makes multiple subsequent calls across
                                                      // the WebWorker boundary, so we should be generous here.
      var lmLayer = new LMLayer(helpers.defaultCapabilities, Worker.constructInstance());

      // We're testing many as asynchronous messages in a row.
      // this would be cleaner using async/await syntax, but
      // alas some of our browsers don't support it.
      return lmLayer.loadModel(
        // We need to provide an absolute path since the worker is based within a blob.
        document.location.protocol + '//' + document.location.host + "/resources/models/simple-dummy.js"
      ).then(function (actualConfiguration) {
        return Promise.resolve();
      }).then(function () {
        // We'll keep it simple here, as this is primarily an integration test.
        // Functionality is handled in the 'headless' test case 'default-word-breaker.js'.
        let context = {
          left: 'The quick brown fox jumped', startOfBuffer: true,
          right: ' over the lazy dog.', endOfBuffer: true
        };
        return lmLayer.wordbreak(context);
      }).then(function (word) {
        assert.deepEqual(word, 'jumped');
        return lmLayer.predict(zeroTransform(), emptyContext());
      });
    });
  });

  function emptyContext() {
    return { left: '', startOfBuffer: true, endOfBuffer: true };
  }

  function zeroTransform() {
    return { insert: '', deleteLeft: 0 };
  }

  function iGotDistractedByHazel() {
    return __json__['models/future_suggestions/i_got_distracted_by_hazel'];
  }
});
