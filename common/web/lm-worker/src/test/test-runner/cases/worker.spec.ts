import { assert } from 'chai';
import { LMLayerWorkerCode } from "@keymanapp/lm-worker/worker-main.wrapped.js";

describe('LMLayerWorker', function () {
  // This one makes multiple subsequent calls across the WebWorker boundary, so we should be generous here.
  this.timeout(5000);

  describe('LMLayerWorkerCode', function() {
    it('should exist!', function() {
      assert.isString(LMLayerWorkerCode);
    });
  });

  describe('Usage within a Web Worker', function () {
    it('should install itself in the worker context', function (done) {
      let blob = new Blob([LMLayerWorkerCode], { type: 'text/javascript' });
      let uri = URL.createObjectURL(blob);
      let worker = new Worker(uri);
      worker.onmessage = function thisShouldBeCalled(message) {
        done();
        worker.terminate();
      };
      // While the config message doesn't trigger a reply message, we have to send it a configuration message first.
      worker.postMessage({
        message: 'config',
        capabilities: {
          maxLeftContextCodeUnits: 64
        }
      });
      worker.postMessage({
        message: 'load',
        // Since the worker's based in a blob, it's not on the 'same domain'.  We need to absolute-path the model file.
        source: {
          type: 'file',
          file: document.location.protocol + '//' + document.location.host + "/resources/models/simple-dummy.js"
        }
      });
    });
  });
});
