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
    it.skip('should construct with zero arguments', function() {
      // TODO: This is broken, because the LMLayerWorker needs
      // to believe it's in a DedicatedWorkerGlobalScope.
      assert.isOk(new LMLayerWorker);
    });
  });
});
