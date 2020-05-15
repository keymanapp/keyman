const assert = require("chai").assert;
const models = require('../').models;

describe('Mocha', function () {
  it('should work', function () {
    assert.isOk(new models.MyModel());
  });
});
