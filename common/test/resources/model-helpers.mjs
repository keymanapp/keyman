import path from 'path';
import { assert } from 'chai';
import fs from 'fs';
import vm from 'vm';

/**
 * @file model-helpers.mjs
 *
 * Defines a common set of helper functions for use with predictive-text-related
 * unit tests.
 */

// Ensure that we can successfully load the module & apply kmwLength, as it's
// needed for some of the unit tests.

// // Verify that the KMW string extensions are loaded via side-effect.

import { extendString } from '@keymanapp/web-utils';

import { createRequire } from "module";
import { fileURLToPath } from 'url';

// Ensure that our KMW string-extensions activate.
extendString();
assert.ok('💩'.kmwLength);

/**
 * Creates a MessageEvent (for inter-worker communication), with the given data payload.
 *
 * @param {*} data
 */
export function createMessageEventWithData(data) {
  return { data };
}

/**
 * Creates a simple, default capabilities object for standard-case LMLayer init.
 */
export function capabilities() {
  return {
    maxLeftContextCodePoints: 64
  }
}

/**
 * Mimics a message from the outer LMLayer shell with a simple, default config object.
 * Used for Worker tests.
 */
export function configWorker(worker) {
  worker.onMessage(createMessageEventWithData({
    message: 'config',
    capabilities: capabilities()
  }));
}

/**
 * A valid model that suggests exactly what you want it to suggest.
 *
 * @returns {ModelDescription}
 */
export function dummyModel(futureSuggestions) {
  return {
    type: 'dummy',
    futureSuggestions: futureSuggestions || []
  };
}
/**
 * Capabilities of a keyboard that will ONLY send left-sided capabilities.
 * The keyboard does not support deleting to the right.
 *
 * @returns {Capabilities}
 */
export function defaultCapabilities() {
  return {
    maxLeftContextCodeUnits: 64
  };
}

/**
 * Returns the Context of an empty buffer; no text, at both the start and
 * end of the buffer.
 *
 * @returns {Context}
 */
export function emptyContext() {
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
export function zeroTransform() {
  return {
    insert: '',
    deleteLeft: 0,
  };
}

/**
 * Returns a random token. NOT guaranteed to be unique.
 *
 * @returns {Token}
 */
export function randomToken() {
  var range =  Number.MAX_SAFE_INTEGER - Number.MIN_SAFE_INTEGER;
  return Math.random() * range + Number.MIN_SAFE_INTEGER;
}

export function iGotDistractedByHazel() {
  return jsonFixture('models/future_suggestions/i_got_distracted_by_hazel');
}

export function jsonFixture(name, root, import_root) {
  // Assuming this file structure:
  // root
  //  └── json
  //       ├── future_suggestions
  //       │     └── ...
  //       └── wordlists
  //             └── ...

  // Default root:  this folder.
  if(!root) {
    root = 'json';
  }

  if(!import_root) {
    import_root = import.meta.url;
  }

  // The most straight-forward way... is to use CommonJS-style require to load JSON.
  // Fortunately, Node provides the tools needed to recreate it.
  const require = createRequire(import_root);

  // ES-module mode also leaves out `__dirname`, so we rebuild that too.
  const __filename = fileURLToPath(import_root);
  const __dirname = path.dirname(__filename);
  return require(path.join(__dirname, root, `${name}.json`));
}

export function importScriptsWith(context) {
    return function() { // the constructed context's importScripts method.

    /* Use of vm.createContext and script.runInContext allow us to avoid
      * polluting the global scope with imports.  When we throw away the
      * context object, imported scripts will be automatically GC'd.
      */
    for(var i=0; i < arguments.length; i++) {
      context = vm.createContext(context);
      var script = new vm.Script(fs.readFileSync(arguments[i]));
      script.runInContext(context);
    }
  }
}
