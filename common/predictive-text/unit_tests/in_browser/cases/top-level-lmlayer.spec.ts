import { assert } from 'chai';

import { LMLayer, Worker as WorkerBuilder }  from "@keymanapp/lexical-model-layer/web";
import { defaultCapabilities } from '../helpers.mjs';

describe('LMLayer', function () {
  this.timeout(5000);

  describe('[[constructor]]', function () {
    it('should construct with a single argument', function () {
      let lmLayer = new LMLayer(defaultCapabilities, WorkerBuilder.constructInstance(), true);
      assert.instanceOf(lmLayer, LMLayer);
      lmLayer.shutdown();
    });
  });

  describe('#asBlobURI()', function () {
    // #asBlobURI() requires browser APIs, hence why it cannot be tested headless in Node.
    it('should take a function and convert it into a blob function', function (done) {
      function dummyHandler() {
        // Post something weird, so we can be reasonably certain the Web Worker is...
        // well, working.
        // WARNING: Do NOT refactor this string as a variable. It **MUST** remain a string
        // in this function body, because the code in this function's body gets
        // stringified!
        postMessage('fhqwhgads');
      }

      // Note:  the full declaration exists; the code we want is wrapped within the func.
      // So... let's just call the func.
      const workerSrc = dummyHandler.toString() + "\ndummyHandler()";
      let uri = WorkerBuilder.asBlobURI(workerSrc);
      assert.match(uri, /^blob:/);

      let worker = new Worker(uri);
      worker.onmessage = function thisShouldBeCalled(event) {
        assert.propertyVal(event, 'data', 'fhqwhgads');
        worker.terminate();
        done();
      };

      worker.postMessage('test');
    })
  })
});
