var assert = require('chai').assert;
var sinon = require('sinon');

let LMLayer = require('../../');

describe('LMLayer', function() {
  describe('[[constructor]]', function () {
    it('should be given a Worker class to instantiate', function () {
      let FakeWorker = sinon.fake();
      new LMLayer(FakeWorker);
      
      assert.strictEqual(FakeWorker.callCount, 1);
    });
  });
});
