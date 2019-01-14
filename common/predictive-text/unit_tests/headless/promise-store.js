var assert = require('chai').assert;
var sinon = require('sinon');

let PromiseStore = require('../../build').PromiseStore;

describe('PromiseStore', function () {
  describe('.make()', function () {
    it("should track a promise's callbacks", function () {
      var promises = new PromiseStore();
      // There should be no tracked promises.
      assert.lengthOf(promises, 0);

      // Add one promise.
      new Promise(function (resolve, reject) {
        promises.make(resolve, reject);
      });
      assert.lengthOf(promises, 1);
    });
  });
});
