/**
 * Helpers and utilities for the Mocha tests.
 */
import * as path from 'path';

/**
 * Builds a path to the fixture with the given path components.
 *
 * e.g., makePathToFixture('basic.xml')
 *
 * @param components One or more path components.
 */
export function makePathToFixture(...components: string[]): string {
  return path.join(__dirname, '..', '..', '..', 'test', 'fixtures', ...components);
}

