var assert = require('chai').assert;
var sinon = require('sinon');

let LMLayerWorker = require('../../worker');

describe('LMLayerWorker', function() {

  describe('#constructor()', function() {

    it('should allow for the mocking of postMessage()', function () {
      var fakePostMessage = sinon.fake();
      var worker = new LMLayerWorker({postMessage: fakePostMessage});
      
      // Sending it the initialize it should notify us that it's initialized!
      worker.onMessage({
        data: {
          message: 'initialize',
          model: 'en-x-dummy'
        }
      });
      assert(fakePostMessage.called);
    });

    it('should work with zero arguments within a Web Worker', function() {
      var fakePostMessage;
      var WorkerInSandbox = defineLMLayerWorkerWithinSandbox({
        postMessage: fakePostMessage = sinon.fake()
      });
      let worker = new WorkerInSandbox();
      assert.isOk(worker);

      // Now try it out.
      worker.onMessage({
        data: {
          message: 'initialize',
          model: 'en-x-dummy'
        }
      });
      assert(fakePostMessage.calledOnce);
    });
  });

  describe('#onMessage()', function() {
    it('should fail if not given the `message` attribute', function () {
      var worker = new LMLayerWorker({postMessage: sinon.fake()});
      // Every message is a discriminated union with the tag being `message`.
      // If it doesn't see message, something is deeply wrong,
      // and it should loudly let us know.
      assert.throws(function () {
        worker.onMessage({
          data: {
            model: 'dummy attribute'
          }
        });
      })
    });
  });

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
