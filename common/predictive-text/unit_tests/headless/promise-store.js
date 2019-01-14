var assert = require('chai').assert;
var sinon = require('sinon');

let PromiseStore = require('../../build').PromiseStore;

describe('PromiseStore', function () {
  describe('.make()', function () {
    it("should track a promise's callbacks", function () {
      var promises = new PromiseStore();
      
      assert.lengthOf(promises, 0);
    });
  });
});
