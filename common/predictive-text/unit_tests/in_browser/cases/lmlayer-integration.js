
var assert = chai.assert;
describe('LMLayer', function () {
  describe('[[constructor]]', function () {
      it('should construct with zero arguments', function () {
        let lmlayer = new LMLayer();
        assert.instanceOf(lmlayer, LMLayer);
      });
  });
});
