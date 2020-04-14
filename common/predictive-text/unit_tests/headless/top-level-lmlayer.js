var assert = require('chai').assert;
var sinon = require('sinon');

let LMLayer = require('../../build');
let LMLayerBase = LMLayer.LMLayerBase;

// Test the top-level LMLayer interface.
// Note: these tests can only be run after BOTH stages of compilation are completed.
describe('LMLayer', function() {
  describe('[[constructor]]', function () {
    it('should accept a Worker to instantiate', function () {
      new LMLayerBase(capabilities(), createFakeWorkerFactory());
    });

    it('should send the `config` message to the LMLayer', async function () {
      let fakeWorker = createFakeWorkerFactory(fakePostMessage);
      let lmLayer = new LMLayerBase(capabilities(), fakeWorker);

      assert.propertyVal(fakeWorker.instance.postMessage, 'callCount', 1);
      // In the "Worker", assert the message looks right
      function fakePostMessage(data) {
        assert.propertyVal(data, 'message', 'config');
        assert.isObject(data.capabilities);
      }
    });
  });

  describe('#loadModel()', function () {
    it('should accept capabilities and model description', function () {
      let fakeWorker = createFakeWorkerFactory();

      let lmLayer = new LMLayerBase(capabilities(), fakeWorker);
      lmLayer.loadModel("./unit_tests/in_browser/resources/models/simple-dummy.js");

      assert.isFunction(fakeWorker.instance.onmessage, 'LMLayer failed to set a callback!');
    });

    it('should send the `load` message to the LMLayer', async function () {
      let fakeWorker = createFakeWorkerFactory(fakePostMessage);
      let lmLayer = new LMLayerBase(capabilities(), fakeWorker);
      let configuration = await lmLayer.loadModel("./unit_tests/in_browser/resources/models/simple-dummy.js");

      assert.propertyVal(fakeWorker.instance.postMessage, 'callCount', 2);
      // In the "Worker", assert the message looks right and
      // ASYNCHRONOUSLY reply with ready message.
      function fakePostMessage(data) {
        // Expected first call:  config.  Ignore it.
        if(data.message == 'config') {
          return;
        }

        assert.propertyVal(data, 'message', 'load');
        assert.isString(data.model);
      
        callAsynchronously(() => fakeWorker.instance.onmessage({
          data: {
            message: 'ready',
            configuration: {}
          }
        }));
      }
    });

    it('should resolve with the model configuration', async function () {
      let expectedConfiguration = {
        leftContextCodeUnits: 32,
        rightContextCodeUnits: 0,
      }

      let fakeWorker = createFakeWorkerFactory(function fakePostMessage(_data) {
        callAsynchronously(() => fakeWorker.instance.onmessage({
          data: {
            message: 'ready',
            configuration: expectedConfiguration
          }
        }));
      });

      let lmLayer = new LMLayerBase(capabilities, fakeWorker);
      let actualConfiguration = await lmLayer.loadModel(
        {
          maxLeftContextCodeUnits: 32,
        },
        {
          kind: 'wordlist',
          words: ['foo', 'bar', 'baz', 'quux']
        }
      );

      // This SHOULD be called by loadModel().
      assert.deepEqual(actualConfiguration, expectedConfiguration);
    })
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
   * Returns a factory producing an object implementing *enough* of the Worker interface 
   * to fool the LMLayer into thinking it's communicating with a bona fide Web Worker.
   * 
   * Also stores the produced instance for use in assertions.
   * 
   * @returns {WorkerFactory} a 'factory' class returning a Worker object with sinon.fake() instances.
   */
  function createFakeWorkerFactory(postMessage) {
    let FakeFactory = function() {}; // barebones JS class definition - requires function as core, constructor.
    FakeFactory.prototype.constructInstance = function() {
      this.instance = {
        postMessage: postMessage ? sinon.fake(postMessage) : sinon.fake(),
        onmessage: null
      };

      return this.instance;
    };
    return new FakeFactory();
  }

  /**
   * Call a function in the future, i.e., later in the event loop.  
   * The call does NOT block the current execution.
   * Use this to fake asynchronous callbacks. 
   * 
   * @param {Function} fn function to call
   */
  function callAsynchronously(fn) {
    if (typeof process !== 'undefined' && process.nextTick) {
      // Preferred way to do it in Node.JS
      process.nextTick(fn);
    } else {
      // In all other JavaScript environments (e.g., the browser)
      setTimeout(fn, 0)
    }
  }
});
