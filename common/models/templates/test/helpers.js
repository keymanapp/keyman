const path = require('path');

var _ = global;

/**
 * Load JSON fixtures from a well-known place.
 */
_.jsonFixture = function (name) {
  return require(path.join(__dirname, 'fixtures', `${name}.json`));
}
