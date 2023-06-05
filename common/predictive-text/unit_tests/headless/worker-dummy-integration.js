import { assert } from 'chai';
import fs from 'fs';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { LMLayer, SourcemappedWorker as Worker } from '#./node/index.js';
import { capabilities, iGotDistractedByHazel } from '@keymanapp/common-test-resources/model-helpers.mjs';

/*
 * Shows off the LMLayer API, using the full prediction interface.
 * The dummy model (or mock model) is a language model which has
 * **injectable** suggestions: that is, you, as the tester, have
 * to provide the predictions. The dummy model does not create any
 * suggestions on its own. The dummy model can take in a series
 * of suggestions when loaded and return them sequentially.
 */
describe('LMLayer using dummy model', function () {
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
    it('will predict future suggestions (loaded from file)', function () {
      var stripIDs = function(suggestions) {
        suggestions.forEach(function(suggestion) {
          delete suggestion.id;
        });
      }

      // We're testing many as asynchronous messages in a row.
      // this would be cleaner using async/await syntax.
      // Not done yet, as this test case is a slightly-edited copy of the in-browser version.
      return lmLayer.loadModel(
        // We're running headlessly, so the path can be relative to the npm root directory.
        require.resolve("@keymanapp/common-test-resources/models/simple-dummy.js")
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
        return Promise.resolve();
      });
    });

    it('will predict future suggestions (loaded from raw source)', function () {
      var stripIDs = function(suggestions) {
        suggestions.forEach(function(suggestion) {
          delete suggestion.id;
        });
      };

      // We're running headlessly, so the path can be relative to the npm root directory.
      let modelCode = fs.readFileSync(require.resolve("@keymanapp/common-test-resources/models/simple-dummy.js")).toString();

      // We're testing many as asynchronous messages in a row.
      // this would be cleaner using async/await syntax.
      // Not done yet, as this test case is a slightly-edited copy of the in-browser version.
      return lmLayer.loadModel(
        modelCode, 'raw'
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
        return Promise.resolve();
      });
    });
  });

  describe('Wordbreaking', function () {
    it('will perform (default) wordbreaking and return word at caret', function () {
      // We're testing many as asynchronous messages in a row.
      // this would be cleaner using async/await syntax.
      // Not done yet, as this test case is a slightly-edited copy of the in-browser version.
      return lmLayer.loadModel(
        // We're running headlessly, so the path can be relative to the npm root directory.
        require.resolve("@keymanapp/common-test-resources/models/simple-dummy.js")
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
});
