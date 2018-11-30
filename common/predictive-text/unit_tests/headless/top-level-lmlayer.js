var assert = require('chai').assert;
var sinon = require('sinon');

let LMLayer = require('../../build');

// Test the top-level LMLayer interface.
// Note: these tests can only be run after BOTH stages of compilation are completed.
describe('LMLayer', function() {
  describe('[[constructor]]', function () {
    it('should accept a Worker to instantiate', function () {
      let fakeWorker = {
        postMessage: sinon.fake(),
        onmessage: null
      };
      new LMLayer(fakeWorker);
    });
  });

  describe('#initialize()', function () {
    it('should accept capabilities and model description', function () {
      let fakeWorker = {
        postMessage: sinon.fake(),
        onmessage: null
      };

      let lmLayer = new LMLayer(fakeWorker);
      lmLayer.initialize(
        {
          maxLeftContextCodeUnits: 32,
        },
        {
          kind: 'wordlist',
          words: ['foo', 'bar', 'baz', 'quux']
        }
      );

      assert.isFunction(fakeWorker.onmessage, 'LMLayer failed to set a callback!');
      assert.propertyVal(fakeWorker.postMessage, 'callCount', 1);
    });
  });

  // Since the Blob API is limited to browsers, look for those
  // tests for .asBlobURI() in the in_browser tests.
  describe('.unwrap', function () {
    it('should return the inner code of a function', function () {
      // Create a multi-line function body we can match in a RegExp.
      let text = LMLayer.unwrap(function hello() {
        var hello;
        var world;
      });
      // Unwrap should give us back ONLY the body. Whitespace isn't really important.
      assert.match(text, /^\s*var\s+hello;\s*var\s+world;\s*$/);
    });
  });
});
