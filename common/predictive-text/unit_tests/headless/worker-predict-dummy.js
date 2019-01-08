/*
 * Unit tests for the Dummy prediction model.
 */

var assert = require('chai').assert;
var sinon = require('sinon');

var DummyModel = require('../../build/intermediate').models.DummyModel;

describe('LMLayerWorker dummy model', function() {
  it('can be instantiated with capabilities', function () {
    var model = new DummyModel({
      maxLeftContextCodeUnits: 64,
    });
    assert.isObject(model);
  });
});
