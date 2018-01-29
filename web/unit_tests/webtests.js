var assert = chai.assert;

describe('Array', function() {
  describe('indexOf()', function() {
    it('Should return -1 when the value is not present', function() {
      assert.equal([1,2,3].indexOf(4), -1);
    });
	it('Should return 2 for example data.', function() {
	  assert.equal([1,2,3].indexOf(3), 2);
	});
  });
});
