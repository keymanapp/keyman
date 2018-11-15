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
      assert.isOk(new LMLayerWorker);
    });
  });
});
