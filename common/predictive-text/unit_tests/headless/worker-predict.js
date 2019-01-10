var assert = require('chai').assert;
var sinon = require('sinon');

let LMLayerWorker = require('../../build/intermediate');


describe('LMLayerWorker', function () {
  describe('#predict()', function () {
    it('should send back suggestions', function () {
      var suggestion = {
        transform: {
          insert: 'I ',
          deleteLeft: 0
        },
        displayAs: 'I'
      };

      // Initialize the worker with a model that will produce one suggestion.
      var fakePostMessage = sinon.fake();
      var worker = new LMLayerWorker({ postMessage: fakePostMessage });
      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        model: dummyModel([
          [suggestion]
        ]),
        capabilities: defaultCapabilities()
      }));
      sinon.assert.calledWithMatch(fakePostMessage.lastCall, {
        message: 'ready',
      });

      // Now predict! We should get the suggestions back.
      worker.onMessage(createMessageEventWithData({
        message: 'predict',
        // TODO: token
        transform: zeroTransform(),
        context: emptyContext()
      }));
      sinon.assert.calledWithMatch(fakePostMessage.lastCall, {
        message: 'suggestions',
        // TODO: token
        suggestions: sinon.match.array.deepEquals([suggestion])
      });
    });

    afterEach(function () {
      // Restore all fakes.
      sinon.restore();
    });
  });

  // Helpers (TODO: factor out!)

  /**
   * Creates a MessageEvent (for inter-worker communication), with the given data payload.
   * @param {*} data 
   */
  function createMessageEventWithData(data) {
    return { data };
  }

  /**
   * A valid model that suggests exactly what you want it to suggest.
   */
  function dummyModel(futureSuggestions) {
    return {
      type: 'dummy',
      futureSuggestions: futureSuggestions || []
    };
  }
  /**
   * Returns reasonable defaults for the default capabilities
   * to initialize the LMLayer.
   */
  function defaultCapabilities() {
    return {
      maxLeftContextCodeUnits: 64
    };
  }

  /**
   * Context of an empty buffer; no text, at both the start and end of the buffer.
   */
  function emptyContext() {
    return { left: '', startOfBuffer: true, endOfBuffer: true };
  }

  /**
   * This transform, when applied, makes no changes to the buffer.
   */
  function zeroTransform() {
    return { insert: '', deleteLeft: 0 };
  }
});
