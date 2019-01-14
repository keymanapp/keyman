var assert = chai.assert;

describe('LMLayer', function () {
  describe('[[constructor]]', function () {
    it('should construct with zero arguments', function () {
      let lmLayer = new LMLayer();
      assert.instanceOf(lmLayer, LMLayer);
    });
  });

  describe('#asBlobURI()', function () {
    // #asBlobURI() requires browser APIs, hence why it cannot be tested headless in Node.
    it('should take a function and convert it into a blob function', function (done) {
      let uri = LMLayer.asBlobURI(function dummyHandler() {
        // Post something weird, so we can be reasonably certain the Web Worker is...
        // well, working.
        // WARNING: Do NOT refactor this string as a variable. It **MUST** remain a string
        // in this function body, because the code in this function's body gets
        // stringified!
        postMessage('fhqwhgads');
      });
      assert.match(uri, /^blob:/);

      let worker = new Worker(uri);
      worker.onmessage = function thisShouldBeCalled(event) {
        assert.propertyVal(event, 'data', 'fhqwhgads');
        done();
      };
    })
  })
});
