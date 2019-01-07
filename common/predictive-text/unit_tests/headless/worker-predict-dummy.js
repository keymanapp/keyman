/*
 * Unit tests for the Dummy prediction model.
 */

var assert = require('chai').assert;
var sinon = require('sinon');

var DummyModel = require('../../build/intermediate').DummyModel;

describe('LMLayerWorker dummy model', function() {
  it('can be instantiated with no arguments', function () {
    var model = new DummyModel;
    assert.isObject(model);
    assert.isFunction(model.suggest)
  });
});
