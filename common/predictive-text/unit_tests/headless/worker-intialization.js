var assert = require('assert');

var worker = require('../../worker');

describe('Dummy worker', function() {
  describe('#hello()', function() {
    it('should return "hello"', function() {
      assert.equal(worker.hello(), 'hello');
    });
  });
});
