import { DEFAULT_BROWSER_TIMEOUT } from '@keymanapp/common-test-resources/test-timeouts.mjs';

describe('LMLayerWorker', function () {
  // This one makes multiple subsequent calls across the WebWorker boundary, so we should be generous here.
  this.timeout(DEFAULT_BROWSER_TIMEOUT);

  describe('Usage within a Web Worker', function () {
    it('should install itself in the worker context', function (done) {
      let worker = new Worker(document.location.protocol + '//' + document.location.host + "/worker-main.js");
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
        source: {
          type: 'file',
          file: "./resources/models/simple-dummy.js"
        }
      });
    });
  });
});
