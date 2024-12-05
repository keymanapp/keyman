import { assert } from 'chai';

import { LMLayer, Worker }   from "@keymanapp/lexical-model-layer/web";

import { DEFAULT_BROWSER_TIMEOUT } from '@keymanapp/common-test-resources/test-timeouts.mjs';
import { defaultCapabilities } from '../helpers.mjs';

// Import assertions, even using 'with', aren't yet supported in Firefox's engine.
// import hazelModel from '@keymanapp/common-test-resources/json/models/future_suggestions/i_got_distracted_by_hazel.json' with { type: 'json' };

let domain: string;

// Is based on raw JSON.
let hazelModel;

/*
 * Shows off the LMLayer API, using the full prediction interface.
 * The dummy model (or mock model) is a language model which has
 * **injectable** suggestions: that is, you, as the tester, have
 * to provide the predictions. The dummy model does not create any
 * suggestions on its own. The dummy model can take in a series
 * of suggestions when loaded and return them sequentially.
 */
describe('LMLayer using dummy model', function () {
  this.timeout(DEFAULT_BROWSER_TIMEOUT);

  before(async () => {
    let loc = document.location;
    // config.testFile generally starts with a '/', with the path resembling the actual full local
    // filesystem for the drive.
    domain = `${loc.protocol}/${loc.host}`

    // Test-config setups will take care of the rest; the server-path will be rooted at the repo root.
    // With aliasing for resources/.

    // Since Firefox can't do JSON imports quite yet.
    const hazelFixture = await fetch(new URL(`${domain}/resources/json/models/future_suggestions/i_got_distracted_by_hazel.json`));
    hazelModel = await hazelFixture.json();
    hazelModel = hazelModel.map((set) => set.map((entry) => {
      return {
        ...entry,
        // Dummy-model predictions all claim probability 1; there's no actual probability stuff
        // used here.
        'lexical-p': 1,
        // We're predicting from a single transform, not a distribution, so probability 1.
        'correction-p': 1,
        // Multiply 'em together.
        p: 1,
      }
    }));
  });

  describe('Prediction', function () {
    it('will predict future suggestions', function () {
      var lmLayer = new LMLayer(defaultCapabilities, Worker.constructInstance(), true);

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
        `${domain}/resources/models/simple-dummy.js`
      ).then(function (actualConfiguration) {
        return Promise.resolve();
      }).then(function () {
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        stripIDs(suggestions);
        assert.deepEqual(suggestions, hazelModel[0]);
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        stripIDs(suggestions);
        assert.deepEqual(suggestions, hazelModel[1]);
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        stripIDs(suggestions);
        assert.deepEqual(suggestions, hazelModel[2]);
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        stripIDs(suggestions);
        assert.deepEqual(suggestions, hazelModel[3]);
        lmLayer.shutdown();
        return Promise.resolve();
      });
    });
  });

  describe('Wordbreaking', function () {
    it('will perform (default) wordbreaking and return word at caret', function () {
      var lmLayer = new LMLayer(defaultCapabilities, Worker.constructInstance());

      // We're testing many as asynchronous messages in a row.
      // this would be cleaner using async/await syntax, but
      // alas some of our browsers don't support it.
      return lmLayer.loadModel(
        // We need to provide an absolute path since the worker is based within a blob.
        `${domain}/resources/models/simple-dummy.js`
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
