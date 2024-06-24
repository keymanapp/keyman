import { assert } from 'chai';
import sinon from 'sinon';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import LMLayerWorker from '#./index.js';

import { configWorker, createMessageEventWithData, emptyContext, iGotDistractedByHazel,
         importScriptsWith, randomToken, zeroTransform } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { timedPromise } from '@keymanapp/web-utils';

describe('LMLayerWorker', function () {
  describe('#predict()', function () {
    it('should send back suggestions', async function () {
      var suggestion = {
        transform: {
          insert: 'I ',
          deleteLeft: 0
        },
        displayAs: 'I'
      };

      // Initialize the worker with a model that will produce one suggestion.
      var fakePostMessage = sinon.fake();
      var filteredFakePostMessage = function(event) {
        if(event.message == 'suggestions') {
          let suggestions = event.suggestions;

          // Strip any IDs set by the model compositor.
          suggestions.forEach(function(suggestion) {
            delete suggestion.id;
          });
        }

        fakePostMessage(event);
      }
      var context = {
        postMessage: filteredFakePostMessage
      };
      context.importScripts = importScriptsWith(context);

      var worker = LMLayerWorker.install(context);
      configWorker(worker);

      worker.onMessage(createMessageEventWithData({
        message: 'load',
        source: {
          type: 'file',
          file: require.resolve("@keymanapp/common-test-resources/models/simple-dummy.js")
        }
      }));
      sinon.assert.calledWithMatch(fakePostMessage.lastCall, {
        message: 'ready',
      });

      var token = randomToken();

      // Now predict! We should get the suggestions back.
      worker.onMessage(createMessageEventWithData({
        message: 'predict',
        token: token,
        transform: zeroTransform(),
        context: emptyContext()
      }));

      // predict() is async, so we need to relinquish control flow temporarily
      // in order for a return message to become available.
      await timedPromise(500);

      // Retrieve the internal 'dummy' suggestions for comparison.
      var hazel = iGotDistractedByHazel();

      sinon.assert.calledWithMatch(fakePostMessage.lastCall, {
        message: 'suggestions',
        token: token,
        suggestions: hazel[0].map((entry) => {
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
        })
      });
    });

    afterEach(function () {
      // Restore all fakes.
      sinon.restore();
    });
  });
});
