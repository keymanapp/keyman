const assert = require("chai").assert;

require('../');

describe('Mocha', function () {
  it('should work', function () {
    assert.isOk(new Models.MyModel());
  });
});
