var assert = require('chai').assert;
var sinon = require('sinon');

let LMLayerWorker = require('../../build/intermediate');

// Unit tests for instantiating and initializing the LMLayer Worker in isolation.
//
// Although the LMLayerWorker expected to be used inside a DedicatedWorkerGlobalScope,
// these unit tests DO NOT run inside a Worker, and instead use Sinon fakes to assert
// behavior.
describe('LMLayerWorker', function() {
  describe('#constructor()', function() {
    it('should allow for the mocking of postMessage()', function () {
      var fakePostMessage = sinon.fake();
      var worker = new LMLayerWorker({ postMessage: fakePostMessage });
      
      // Sending it the initialize it should notify us that it's initialized!
      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        model: dummyModel(),
        capabilities: defaultCapabilities()
      }));
      assert(fakePostMessage.calledOnce);
    });
  });

  describe('#onMessage()', function() {
    it('should fail if not given the `message` attribute', function () {
      var worker = new LMLayerWorker({
        postMessage: sinon.fake(), // required, but ignored in this test case
      });
      // Every message is a discriminated union with the tag being `message`.
      // If it doesn't see 'message', something is deeply wrong,
      // and it should loudly let us know.
      assert.throws(function () {
        worker.onMessage(createMessageEventWithData({
          model: 'dummy attribute'
        }));
      });
    });
  });

  describe('.install()', function () {
    it('should create a new instance, installed on our global object', function () {
      var fakeWorkerGlobal = {
        onmessage: undefined,
        postMessage: new sinon.fake(),
      };
      // Instantiate and install a worker on our global object.
      var worker = LMLayerWorker.install(fakeWorkerGlobal);
      assert.instanceOf(worker, LMLayerWorker);
      // It should have installed a callback.
      assert.isFunction(fakeWorkerGlobal.onmessage);

      // Send a message; we should get something back.
      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        model: dummyModel(),
        capabilities: defaultCapabilities()
      }));

      // It called the postMessage() in its global scope. 
      assert.isTrue(fakeWorkerGlobal.postMessage.calledOnce)
    });
  });

  describe('Message: initialize', function () {
    it('should disallow any other message', function () {
      var worker = new LMLayerWorker({
        postMessage: sinon.fake(), // required, but ignored
      });

      // It should not respond to 'predict'
      assert.throws(function () {
        worker.onMessage(createMessageEventWithData({
          message: 'predict',
        }));
      }, /invalid message/i);
    });

    it('should send back a "ready" message', function () {
      var fakePostMessage = sinon.fake();
      var worker = new LMLayerWorker({ postMessage: fakePostMessage });
      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        model: dummyModel(),
        capabilities: defaultCapabilities()
      }));

      assert(fakePostMessage.calledOnceWith(sinon.match({
        message: 'ready'
      })));
    });

    it('should send back configuration', function () {
      var fakePostMessage = sinon.fake();
      var worker = new LMLayerWorker({ postMessage: fakePostMessage });
      var maxCodeUnits = 64;
      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        model: dummyModel(),
        capabilities: {
          maxLeftContextCodeUnits: maxCodeUnits,
        }
      }));

      assert(fakePostMessage.calledOnceWith(sinon.match({
        message: 'ready',
        configuration: {
          leftContextCodeUnits: maxCodeUnits,
          rightContextCodeUnits: 0,
        }
      })), lastMessageAsString(fakePostMessage));
    });
  });

  /**
   * Creates a MessageEvent (for inter-worker communication), with the given data payload.
   * @param {*} data 
   */
  function createMessageEventWithData(data) {
    return { data };
  }

  /**
   * Deprecation warning: Soon, models will not be required to be passed as source code;
   * when this happens, tests should refrain from sending source code for the model
   * parameter.
   */
  function dummyModel() {
    return 'return {model: {}, configuration: {}}';
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
   * Returns the last message received in a pretty string format. 
   * @param {sinon.SinonFake} fakePostMessage 
   * @returns {string}
   */
  function lastMessageAsString(fakePostMessage) {
    return JSON.stringify(fakePostMessage.lastCall.args[0], null, 2);
  }
});
