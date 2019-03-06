var assert = chai.assert;
var LMLayer = com.keyman.text.prediction.LMLayer;

describe('LMLayerWorker', function () {
  this.timeout(5000);
  describe('LMLayerWorkerCode', function() {
    it('should exist!', function() {
      assert.isFunction(LMLayerWorkerCode,
        'Could not find LMLayerWorkerCode! Does embedded_worker.js exist?'
      );
    });
  });

  describe('Usage within a Web Worker', function () {
    it('should install itself in the worker context', function (done) {
      let uri = LMLayer.asBlobURI(LMLayerWorkerCode);
      let worker = new Worker(uri);
      worker.onmessage = function thisShouldBeCalled(message) {
        done();
        worker.terminate();
      };
      // While the config message doesn't trigger a reply message, we have to send it a configuration message first.
      worker.postMessage({
        message: 'config',
        capabilities: helpers.defaultCapabilities
      })
      worker.postMessage({
        message: 'load',
        // Since the worker's based in a blob, it's not on the 'same domain'.  We need to absolute-path the model file.
        model: document.location.protocol + '//' + document.location.host + "/resources/models/simple-dummy.js"
      });
    });
  });
})
