const path = require('path');

var _ = global;

// TODO: move this to its own package; e.g., @keymanapp/models-test-helpers
// TODO: then mocha invocation is as follows:
// TODO:     mocha -r @keymanapp/models-test-helpers test/

/**
 * Load JSON fixtures from a well-known place.
 */
_.jsonFixture = function (name) {
  return require(path.join(__dirname, 'fixtures', `${name}.json`));
}

/**
 * Returns the Context of an empty buffer; no text, at both the start and
 * end of the buffer.
 * 
 * @returns {Context}
 */
_.emptyContext = function emptyContext() {
  return {
    left: '',
    startOfBuffer: true,
    endOfBuffer: true
  };
}

/**
 * Returns a Transform that, when applied, makes no changes to the buffer.
 *
 * @returns {Transform}
 */
_.zeroTransform = function zeroTransform() {
  return {
    insert: '',
    deleteLeft: 0,
  };
}
