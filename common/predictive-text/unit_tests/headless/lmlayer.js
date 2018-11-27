var assert = require('chai').assert;
var sinon = require('sinon');

let LMLayer = require('../../');

describe('LMLayer', function() {
  describe('[[constructor]]', function () {
    it('should be take a URI to instantiate', function () {
      new LMLayer(uri);
    });
  });

  /**
   * .unwrap() is a static function that unwraps some function code.
   */
  describe('.unwrap', function () {
    it('should return the inner code of a function', function () {
      let text = LMLayer.unwrap(function hello() {
        var hello;
        var world;
      });

      assert.match(text, /^\s*var\s+hello;\s*var\s+world;\s*$/);
    });
  });
});
