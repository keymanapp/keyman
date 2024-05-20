import { assert } from '../../../../../node_modules/chai/chai.js';

import { Worker as WorkerBuilder }   from "../../../build/lib/web/index.mjs";
import { LMLayerWorkerCode } from "/base/common/web/lm-worker/build/lib/worker-main.wrapped.min.js";
import * as helpers from "../helpers.mjs";

describe('LMLayerWorker', function () {
  // This one makes multiple subsequent calls across the WebWorker boundary, so we should be generous here.
  this.timeout(Math.max(testconfig.timeouts.standard, 5000));

  describe('LMLayerWorkerCode', function() {
    it('should exist!', function() {
      // assert.isFunction(LMLayerWorkerCode,
      //   'Could not find LMLayerWorkerCode! Does embedded_worker.js exist?'
      // );
      assert.isString(LMLayerWorkerCode);
    });
  });

  describe('Usage within a Web Worker', function () {
    it('should install itself in the worker context', function (done) {
      let uri = WorkerBuilder.asBlobURI(LMLayerWorkerCode);
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
        source: {
          type: 'file',
          file: document.location.protocol + '//' + document.location.host + "/resources/models/simple-dummy.js"
        }
      });
    });
  });
})
