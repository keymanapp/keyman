var assert = require('chai').assert;
var sinon = require('sinon');

let LMLayer = require('../../');

describe('LMLayer', function() {
  describe('[[constructor]]', function () {
    it('should be given a WorkerFactory to instantiate', function () {
      let createWorker = sinon.fake();
      new LMLayer(createWorker);
      assert.strictEqual(createWorker.callCount, 1);
    });

    it('should give the worker factory a blob URI', function () {
      let createWorker = sinon.fake();
      new LMLayer(createWorker);
      assert.match(createWorker.lastArg, /^blob:/);
    });
  });
});
