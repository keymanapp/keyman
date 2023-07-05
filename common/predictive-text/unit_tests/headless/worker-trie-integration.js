import { assert } from 'chai';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { LMLayer, SourcemappedWorker as Worker } from '#./node/index.js';
import { capabilities } from '@keymanapp/common-test-resources/model-helpers.mjs';

/*
 * How to run the worlist
 */
describe('LMLayer using the trie model', function () {
  let lmLayer;
  let worker;

  beforeEach(function() {
    worker = Worker.constructInstance();
    lmLayer = new LMLayer(capabilities(), worker);
  });

  afterEach(function () {
    // As we're using Node worker threads here, failure to terminate them will cause the
    // headless test run to hang after completion.
    lmLayer.shutdown();
    worker.terminate();  // should be covered by the former, but just in case... for CI stability.
  });

  describe('Prediction', function () {
    var EXPECTED_SUGGESTIONS = 3;
    it('will predict an empty buffer', function () {
      // We're testing many as asynchronous messages in a row.
      // this would be cleaner using async/await syntax.
      // Not done yet, as this test case is a slightly-edited copy of the in-browser version.
      return lmLayer.loadModel(
        // We're running headlessly, so the path can be relative to the npm root directory.
        require.resolve("@keymanapp/common-test-resources/models/simple-trie.js")
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
      return lmLayer.loadModel(
        // We're running headlessly, so the path can be relative to the npm root directory.
        require.resolve("@keymanapp/common-test-resources/models/naive-trie.js")
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
