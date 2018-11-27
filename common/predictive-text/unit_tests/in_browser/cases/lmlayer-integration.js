var assert = chai.assert;

describe('LMLayer', function () {
  describe('[[constructor]]', function () {
      it('should construct with zero arguments', function () {
        let lmlayer = new LMLayer();
        assert.instanceOf(lmlayer, LMLayer);
      });
  });

  describe('#asBlobURI', function () {
    it('should take a function and convert it into a blob function', function () {
      let uri = LMLayer.asBlobURI(function dummyHandler() {
        var dummy;
      });
      assert.match(uri, /^blob:/);
    })
  })
});
