import { assert } from '../../../../../node_modules/chai/chai.js';

import { LMLayer, Worker }   from "../../../build/lib/web/index.mjs";
import * as helpers from "../helpers.mjs";

/*
 * How to run the worlist
 */
describe('LMLayer using the trie model', function () {
  this.timeout(testconfig.timeouts.standard);

  describe('Prediction', function () {
    var EXPECTED_SUGGESTIONS = 3;
    it('will predict an empty buffer', function () {
      this.timeout(testconfig.timeouts.standard * 3); // This one makes multiple subsequent calls across
                                                  // the WebWorker boundary, so we should be generous here.

      // Parameter 3 = true:  enables 'test mode', disables correction-search timeout.
      // This helps prevent the correction-search timeout from flaking out periodically during unit tests in
      // CI, since remote servers / devices are involved.
      var lmLayer = new LMLayer(helpers.defaultCapabilities, Worker.constructInstance(), true);

      // We're testing many as asynchronous messages in a row.
      // this would be cleaner using async/await syntax, but
      // alas some of our browsers don't support it.
      return lmLayer.loadModel(
        // We need to provide an absolute path since the worker is based within a blob.
        document.location.protocol + '//' + document.location.host + "/resources/models/simple-trie.js"
      ).then(function (_actualConfiguration) {
        return Promise.resolve();
      }).then(function () {
        return lmLayer.predict(zeroTransform(), atEndOfBuffer(''));
      }).then(function (suggestions) {
        // TODO: not super sure what to assert here!
        assert.isAtLeast(suggestions.length, EXPECTED_SUGGESTIONS);
        return lmLayer.predict(type('t'), atEndOfBuffer(''));
      }).then(function (suggestions) {
        assert.isAtLeast(suggestions.length, EXPECTED_SUGGESTIONS);
        return lmLayer.predict(type('h'), atEndOfBuffer('t'));
      }).then(function (suggestions) {
        assert.isAtLeast(suggestions.length, EXPECTED_SUGGESTIONS);
        return lmLayer.predict(type('q'), atEndOfBuffer('the '));
      }).then(function (suggestions) {
        assert.isAtLeast(suggestions.length, EXPECTED_SUGGESTIONS);
        lmLayer.shutdown();
        return Promise.resolve();
      });
    });
  });

  describe('Regression tests', function () {
    // Building the model with the default searchTermToKey implementation
    // should also use the default implementation in the frontend.
    //
    // I've compiled a model with the defaults for the word 'naïve', and its
    // search key is 'naive' accordingly (default searchTermToKey transforms
    // to NFD and removes common diacritics).
    //
    // The lookup should also do the same to the input!
    //
    // https://community.software.sil.org/t/search-term-to-key-in-lexical-model-not-working-both-ways-by-default/3133
    it('should use the default searchTermToKey()', function () {
      var lmLayer = new LMLayer(helpers.defaultCapabilities, Worker.constructInstance(), /* testMode */ true);

      return lmLayer.loadModel(
        // We need to provide an absolute path since the worker is based within a blob.
        document.location.protocol + '//' + document.location.host + "/resources/models/naive-trie.js"
      ).then(function (_actualConfiguration) {
        return Promise.resolve();
      }).then(function () {
        // Pretend we are typing 'na|ï'
        // Notice that we are typing the diacritic!
        return lmLayer.predict(type('ï'), atEndOfBuffer('na'));
      }).then(function (rawSuggestions) {
        // Discard the keep suggestion
        var suggestions = rawSuggestions.filter(function skimKeepSuggestions(s) {
          return s.tag !== 'keep'
        })
        assert.isAtLeast(suggestions.length, 1)

        // We SHOULD get 'naïve' suggested
        var topSuggestion = suggestions[0];
        assert.equal(topSuggestion.displayAs, "naïve")
        assert.include(topSuggestion.transform.insert, "ïve")
      });
    });
  });

  /**
   * Type some text (as a transform).
   * @param  {string} text to type
   * @return {Transform}
   */
  function type(text)  {
    return { insert: text, deleteLeft: 0 };
  }

  /**
   * Create a context at the end of the buffer with the given text.
   * @param {string} buffer
   * @return {Context}
   */
  function atEndOfBuffer(buffer) {
    return { left: buffer, startOfBuffer: buffer.length === 0, endOfBuffer: true }
  }

  function zeroTransform() {
    return { insert: '', deleteLeft: 0 };
  }
});
