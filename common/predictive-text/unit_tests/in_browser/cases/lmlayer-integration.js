var assert = chai.assert;

describe('LMLayer', function () {
  describe('[[constructor]]', function () {
      it('should construct with zero arguments', function () {
        let lmlayer = new LMLayer();
        assert.instanceOf(lmlayer, LMLayer);
      });
  });

  describe('#asBlobURI', function () {
    it('should take a function and convert it into a blob function', function (done) {
      let uri = LMLayer.asBlobURI(function dummyHandler() {
        // Post something weird, so we can be reasonably certain it's not a fluke.
        // WARNING: Do NOT factor out this string as a variable.
        // It MUST remain a string in this function body, because it gets stringified!
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
