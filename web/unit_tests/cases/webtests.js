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

describe('Hello World', function() {
  before(function() {
    fixture.setBase('fixtures');
  });
  
  beforeEach(function() {
    this.result = fixture.load('fixturetest.html');
  });
  
  afterEach(function() {
    fixture.cleanup();
  });
	
  describe('Fixture Loading', function() {
    it('Should have the input element \'test\' with text "Initial text."', function() {
      var input = document.getElementById('test');
      assert.isNotNull(input, "The element doesn't exist!");
      assert.equal(input.value, "Initial text.", "The text does not match.");
    });
  });
});
