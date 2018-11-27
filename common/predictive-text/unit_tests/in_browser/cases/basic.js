var assert = chai.assert;
describe('LMLayerWorker', function () {
  describe('LMLayerWorkerCode', function() {
    it('should exist!', function() {
      assert.isFunction(LMLayerWorkerCode,
        'Could not find LMLayerWorkerCode! Does embedded_worker.js exist?'
      );
    });
  });

  describe('Usage within a Web Worker', function () {
    it('should install itself in the worker context', function (done) {
      let code = LMLayer.unwrap(LMLayerWorkerCode);
      assert.isString(code);

      let blob = new Blob([code], { type: 'text/javascript' });
      let uri = URL.createObjectURL(blob);

      let worker = new Worker(uri);
      worker.onmessage = function thisShouldBeCalled(message) {
        done();
      };
      worker.postMessage({
        message: 'initialize',
        model: "return {model: {}, configuration: {}}"
      });
    });
  });
})
