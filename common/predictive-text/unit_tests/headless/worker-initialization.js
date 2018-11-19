var assert = require('chai').assert;
var sinon = require('sinon');

let LMLayerWorker = require('../../worker');

describe('LMLayerWorker', function() {

  describe('#constructor()', function() {
    it('should allow for the mocking of postMessage()', function () {
      var fakePostMessage = sinon.fake();
      var worker = new LMLayerWorker({ postMessage: fakePostMessage });
      
      // Sending it the initialize it should notify us that it's initialized!
      worker.onMessage(createMessageEventWithData({
        message: 'initialize'
      }));
      assert(fakePostMessage.calledOnce);
    });

    // TODO: remove from here; convert into integration test
    it.skip('should work with zero arguments within a Web Worker', function() {
      var fakePostMessage;
      var WorkerInSandbox = defineLMLayerWorkerWithinSandbox({
        postMessage: fakePostMessage = sinon.fake(),
      });
      let worker = new WorkerInSandbox();

      // Now try it out.
      worker.onMessage(createMessageEventWithData({ message: 'initialize' }));
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
      })
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
       message: 'invalid-message',
       model: 'en-x-dummy'
      }));

      // It called the postMessage() in its global scope. 
      assert.isTrue(fakeWorkerGlobal.postMessage.calledOnce)
    });
  });

  describe('Message: initialize', function () {
    it('should send back a "ready" message', function () {
      var fakePostMessage = sinon.fake();
      var worker = new LMLayerWorker({ postMessage: fakePostMessage });
      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        model: 'en-x-dummy'
      }));

      assert(fakePostMessage.calledOnceWith(sinon.match({
        message: 'ready'
      })));
    });

    it('should send back configuration', function () {
      var fakePostMessage = sinon.fake();
      var worker = new LMLayerWorker({ postMessage: fakePostMessage });
      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        model: 'en-x-dummy'
      }));

      assert(fakePostMessage.calledOnceWith(sinon.match({
        message: 'ready',
        configuration: {
          leftContextCodeUnits: sinon.match.number,
          rightContextCodeUnits: sinon.match.number,
        }
      })));
    });

    it('should run the code for the model', function () {
      var fakePostMessage;
      var worker = new LMLayerWorker({
        postMessage: fakePostMessage = sinon.fake(), // required, but ignored in this test case
      });

      // Create some values that the function can't fake:
      var fakeLeft = Math.round(Math.random() * 64);
      var fakeRight = Math.round(Math.random() * 64);

      worker.onMessage(createMessageEventWithData({
        message: 'initialize',
        // We can only send source code through the Structure Clones algorithm,
        // (we can't send mocks :c) so, send something that cannot
        // be easily faked, and we can determine if it's correct.
        model: `
          return {
            model: {},
            configuration: {
              leftContextCodeUnits: ${fakeLeft},
              rightContextCodeUnits: ${fakeRight}
            }
          };
        `
      }));

      assert(fakePostMessage.calledOnceWith(sinon.match({
        message: 'ready',
        configuration: {
          leftContextCodeUnits: fakeLeft,
          rightContextCodeUnits: fakeRight,
        }
      })), JSON.stringify(fakePostMessage.lastCall.args[0], null, 2));
    });
  });

  describe('Message: predict', function () {
    it.skip('should predict from a local model', function () {
      // will need import scripts figured out
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
   * Evaluate the source code of the LMLayerWorker within a
   * sandbox, with our own mocked self. Create a new
   * "sandboxed" LMLayer. It's only "sandboxed", in that it
   * believes it's in a WebWorker.
   * 
   * @param self `self` used in the fake Worker. You should add
   * at least `postMessage()` to this object.
   */
  function defineLMLayerWorkerWithinSandbox(self) {
    var fs = require('fs');
    var sourceCode = fs.readFileSync(require.resolve('../../worker'));
    var sandbox = `(function (self) {
      var exports = {}; // TypeScript CommonJS code generation will attempt to write to this.
      var postMessage = self.postMessage;
      ${sourceCode}
      return LMLayerWorker;
    })`;
    return eval(sandbox)(self);
  }
});
