import path from 'path';
import { assert } from 'chai';

var _ = global;

// TODO: move this to its own package; e.g., @keymanapp/models-test-helpers
// TODO: then mocha invocation is as follows:
// TODO:     mocha -r @keymanapp/models-test-helpers test/

import { extendString } from '@keymanapp/web-utils';

import { createRequire } from "module";
import { fileURLToPath } from 'url';

extendString();
assert.ok('💩'.kmwLength);

/**
 * Load JSON fixtures from a well-known place.
 */
_.jsonFixture = function (name) {
  // The most straight-forward way... is to use CommonJS-style require to load JSON.
  // Fortunately, Node provides the tools needed to recreate it.
  const require = createRequire(import.meta.url);

  // ES-module mode also leaves out `__dirname`, so we rebuild that too.
  const __filename = fileURLToPath(import.meta.url);
  const __dirname = path.dirname(__filename);
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
