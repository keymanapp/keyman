var assert = require('chai').assert;

var worker = require('../../worker');

describe('LMLayerWorker', function() {
  describe('#constructor()', function() {
    it('should construct with zero arguments', function() {
      assert.isOk(new worker.LMLayerWorker);
    });
  });
});
