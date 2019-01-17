/*
 * Unit tests for the Dummy prediction model.
 */

var assert = require('chai').assert;

var WordListModel = require('../../build/intermediate').models.WordListModel;

describe('LMLayerWorker word list model', function() {
  describe('instantiation', function () {
    it('can be instantiated with capabilities', function () {
      var model = new WordListModel(defaultCapabilities, []);
      assert.isObject(model);
    });
  });

  describe('prediction', function () {
      // TODO
  });
});

