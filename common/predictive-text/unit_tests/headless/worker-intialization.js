var assert = require('chai').assert;
var sinon = require('sinon');

let {LMLayerWorker} = require('../../worker');

describe('LMLayerWorker', function() {

  it('should allow for the mocking of postMessage()', function () {
    var fakePostMessage = sinon.fake();
    var worker = new LMLayerWorker({postMessage: fakePostMessage});
    
    // Sending it the initialize it should notify us that it's initialized!
    worker.onMessage({
      message: 'initialize',
      model: 'en-x-dummy'
    });
    assert(fakePostMessage.called);
  });

  describe('#constructor()', function() {
    it('should construct with zero arguments', function() {
      // Create a new "sandboxed" LMLayer. It's only "sandboxed", in that it believes it's in a WebWorker.
      var fs = require('fs');
      var sourceCode = fs.readFileSync(require.resolve('../../worker'));
      var sandbox = `(function (self) {
        var exports = {}; // TypeScript CommonJS code generation will attempt to write to this.
        var postMessage = self.postMessage;
        ${sourceCode}
        return LMLayerWorker;
      })`;
      var createSandbox = eval(sandbox);
      var fakeSelf = {
        postMessage: sinon.fake()
      };
      var WorkerInSandbox = createSandbox(fakeSelf);
      let worker = new WorkerInSandbox();
      assert.isOk(worker);
      console.log(sandbox);

      // Now try it out.
      worker.onMessage({
        message: 'initialize',
        model: 'en-x-dummy'
      });
      assert(fakeSelf.postMessage.calledOnce);
    });
  });
});
