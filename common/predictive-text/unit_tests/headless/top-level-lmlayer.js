var assert = require('chai').assert;
var sinon = require('sinon');

let LMLayer = require('../../build');

// Test the top-level LMLayer interface.
// Note: these tests can only be run after BOTH stages of compilation are completed.
describe('LMLayer', function() {
  describe('[[constructor]]', function () {
    it('should accept a Worker to instantiate', function () {
      new LMLayer(createFakeWorker());
    });
  });

  describe('#initialize()', function () {
    it('should accept capabilities and model description', function () {
      let fakeWorker = createFakeWorker();

      let lmLayer = new LMLayer(fakeWorker);
      lmLayer.initialize(
        {
          maxLeftContextCodeUnits: 32,
        },
        {
          kind: 'wordlist',
          words: ['foo', 'bar', 'baz', 'quux']
        }
      );

      assert.isFunction(fakeWorker.onmessage, 'LMLayer failed to set a callback!');
    });

    it('should send the `initialize` message to the LMLayer', async function () {
      let fakeWorker = createFakeWorker(fakePostMessage);
      let lmLayer = new LMLayer(fakeWorker);
      let configuration = await lmLayer.initialize(
        {
          maxLeftContextCodeUnits: 32,
        },
        {
          kind: 'wordlist',
          words: ['foo', 'bar', 'baz', 'quux']
        }
      );

      assert.propertyVal(fakeWorker.postMessage, 'callCount', 1);
      // In the "Worker", assert the message structure and reply.
      function fakePostMessage(data) {
        assert.propertyVal(data, 'message', 'initialize')
        assert.isObject(data.capabilities);
        assert.isObject(data.model);
      
        // send the message on setTimeout() to emulate "asynchronous" call.
        setTimeout(() => fakeWorker.onmessage({
          data: {
            message: 'ready',
            configuration: {}
          }
        }), 0);
      }
      assert.isObject(configuration);
    });
  });

  // Since the Blob API is limited to browsers, look for those
  // tests for .asBlobURI() in the in_browser tests.
  describe('.unwrap', function () {
    it('should return the inner code of a function', function () {
      // Create a multi-line function body we can match in a RegExp.
      let text = LMLayer.unwrap(function hello() {
        var hello;
        var world;
      });
      // Unwrap should give us back ONLY the body. Whitespace isn't really important.
      assert.match(text, /^\s*var\s+hello;\s*var\s+world;\s*$/);
    });
  });

  /**
   * Returns an object implementing *enough* of the Worker
   * interface to fool the LMLayer into thinking it's
   * communicating with a bona fide Web Worker.
   * 
   * @returns {Worker} an object with sinon.fake() instances.
   */
  function createFakeWorker(postMessage) {
    return {
        postMessage: postMessage ? sinon.fake(postMessage) : sinon.fake(),
        onmessage: null
      };
  }
});
