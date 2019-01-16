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
        promises.make(randomToken(), resolve, reject);
      });
      assert.lengthOf(promises, 1);
    });

    it('should reject the promise when a token is reused', function () {
      var promises = new PromiseStore();
      
      var duplicatedToken = randomToken();

      // Add a promise, and an unrelated promise.
      new Promise(function (resolve, reject) {
        promises.make(duplicatedToken, resolve, reject);
      });
      new Promise(function (resolve, reject) {
        promises.make(randomToken(), resolve, reject);
      });
      assert.lengthOf(promises, 2);

      // Now, reuse the token of the first (unresolved) promise:
      (new Promise(function (resolve, reject) {
        promises.make(duplicatedToken, resolve, reject);
      })).then(
        function (resolve) { return promise.reject('Should not have gotten here!'); },
        // It SHOULD reject.
        function (error) {
          assert.isObject(error);
          // It did NOT track a new promise.
          assert.lengthOf(promises, 2);
        }
      );
    });
  });

  describe('.keep()', function () {
    it('should call the resolve() function', function () {
      var promises = new PromiseStore();
      var token = randomToken();
      var promise = new Promise(function (resolve, reject) {
        promises.make(token, resolve, reject);
      });

      // Resolve the promise asynchronously.
      var randomPayload = randomToken();
      doLater(function () {
        assert.lengthOf(promises, 1);
        promises.keep(token, randomPayload);
      });

      return promise.then(function (actual) {
        assert.strictEqual(actual, randomPayload);
        assert.lengthOf(promises, 0);
      });
    });
  });

  describe('.break()', function () {
    it('should call the reject() function', function () {
      var promises = new PromiseStore();
      var token = randomToken();
      var promise = new Promise(function (resolve, reject) {
        promises.make(token, resolve, reject);
      });

      // Resolve the promise asynchronously.
      var error = new Error();
      doLater(function () {
        assert.lengthOf(promises, 1);
        promises.break(token, error);
      });

      // inspired by:
      // https://gist.github.com/haroldtreen/5f1055eee5fcf01da3e0e15b8ec86bf6#gistcomment-2623170
      return promise.then(
        function () { return Promise.reject('should not be called'); },
        function (actualError) {
          assert.strictEqual(actualError, error);
          assert.lengthOf(promises, 0);
        }
      );
    });
  });

  // Do something asynchronously.
  function doLater(callback) {
    return setTimeout(callback, 0);
  }
});
