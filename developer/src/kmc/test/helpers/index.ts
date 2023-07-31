/**
 * Helpers and utilities for the Mocha tests.
 */
import * as path from 'path';
import { fileURLToPath } from 'url';

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
