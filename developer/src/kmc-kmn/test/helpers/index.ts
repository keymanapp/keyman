/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Helpers and utilities for the Mocha tests.
 */
import * as path from 'node:path';
import { fileURLToPath } from 'node:url';
import { assert } from 'chai';
import { CompilerCallbacks } from '@keymanapp/developer-utils';
import { KmnCompiler } from '../../src/compiler/compiler.js';

/**
 * Builds a path to the fixture with the given path components.
 *
 * e.g., makePathToFixture('example.qaa.trivial')
 * e.g., makePathToFixture('example.qaa.trivial', 'model.ts')
 *
 * @param components One or more path components.
 */
 export function makePathToFixture(...components: string[]): string {
  return fileURLToPath(new URL(path.join('..', '..', '..', 'test', 'fixtures', ...components), import.meta.url));
}

export async function compileTestKeyboard(callbacks: CompilerCallbacks, fixture: string[]) {
  const compiler = new KmnCompiler();
  assert(await compiler.init(callbacks, {saveDebug: true, shouldAddCompilerVersion: false}));
  assert(compiler.verifyInitialized());

  const kmnPath = makePathToFixture(...fixture);

  return await compiler.run(kmnPath, null);
}