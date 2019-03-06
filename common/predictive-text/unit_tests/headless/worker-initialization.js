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
      var context = {
        postMessage: fakePostMessage
      };
      context.importScripts = importScriptsWith(context);

      var worker = LMLayerWorker.install(context);
      
      // Sending it the initialize it should notify us that it's initialized!
      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        model: "./unit_tests/in_browser/resources/models/simple-dummy.js"
      }));
      assert(fakePostMessage.calledOnce);
    });
  });

  describe('#onMessage()', function() {
    it('should fail if not given the `message` attribute', function () {
      var context = {
        postMessage: sinon.fake()
      };
      context.importScripts = importScriptsWith(context);
      var worker = LMLayerWorker.install(context);
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
      fakeWorkerGlobal.importScripts = importScriptsWith(fakeWorkerGlobal);

      // Instantiate and install a worker on our global object.
      var worker = LMLayerWorker.install(fakeWorkerGlobal);
      assert.instanceOf(worker, LMLayerWorker);
      // It should have installed a callback.
      assert.isFunction(fakeWorkerGlobal.onmessage);

      // Send a message; we should get something back.
      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        model: "./unit_tests/in_browser/resources/models/simple-dummy.js"
      }));

      // It called the postMessage() in its global scope. 
      assert.isTrue(fakeWorkerGlobal.postMessage.calledOnce)
    });
  });

  describe('Message: initialize', function () {
    it('should disallow any other message', function () {
      var context = {
        postMessage: sinon.fake()
      };
      context.importScripts = importScriptsWith(context);

      var worker = LMLayerWorker.install(context);

      // It should not respond to 'predict'
      assert.throws(function () {
        worker.onMessage(createMessageEventWithData({
          message: 'predict',
        }));
      }, /invalid message/i);
    });

    it('should send back a "ready" message', function () {
      var fakePostMessage = sinon.fake();
      var context = {
        postMessage: fakePostMessage
      };
      context.importScripts = importScriptsWith(context);

      var worker = LMLayerWorker.install(context);
      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        model: "./unit_tests/in_browser/resources/models/simple-dummy.js"
      }));

      assert(fakePostMessage.calledOnceWith(sinon.match({
        message: 'ready'
      })));
    });

    it('should send back configuration', function () {
      var fakePostMessage = sinon.fake();
      var context = {
        postMessage: fakePostMessage
      };
      context.importScripts = importScriptsWith(context);

      var worker =  LMLayerWorker.install(context);
      // simple-dummy.js is set with the following.
      var maxCodeUnits = 64;
      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        model: "./unit_tests/in_browser/resources/models/simple-dummy.js"
      }));

      sinon.assert.calledWithMatch(fakePostMessage, {
        message: 'ready',
        configuration: {
          leftContextCodeUnits: maxCodeUnits,
          rightContextCodeUnits: 0,
        }
      });
    });
  });
});
