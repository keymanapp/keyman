import path from 'node:path';
import { createRequire } from "node:module";
import { fileURLToPath } from 'node:url';

import { LexicalModelTypes } from '@keymanapp/common-types';

// TODO: move this to its own package; e.g., @keymanapp/models-test-helpers
// TODO: then mocha invocation is as follows:
// TODO:     mocha -r @keymanapp/models-test-helpers test/

import { KMWString } from 'keyman/common/web-utils';

// The predictive-text engine always keeps these enabled.
KMWString.enableSupplementaryPlane(true);

/**
 * Load JSON fixtures from a well-known place.
 */
export function jsonFixture(name: string): any {
  // The most straight-forward way... is to use CommonJS-style require to load JSON.
  // Fortunately, Node provides the tools needed to recreate it.
  const require = createRequire(import.meta.url);

  // ES-module mode also leaves out `__dirname`, so we rebuild that too.
  const __dirname = fileURLToPath(import.meta.url + '../../../../../../../../src/test/auto/resources/');

  // Lands in /web/build/test/headless/engine/predictive-text/templates.
  return require(path.join(__dirname, 'fixtures', `${name}.json`));
}

/**
 * Returns the Context of an empty buffer; no text, at both the start and
 * end of the buffer.
 *
 * @returns {Context}
 */
export function emptyContext(): LexicalModelTypes.Context {
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
export function zeroTransform(): LexicalModelTypes.Transform {
  return {
    insert: '',
    deleteLeft: 0,
  };
}
