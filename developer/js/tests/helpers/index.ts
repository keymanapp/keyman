/**
 * Helpers and utilities for the Mocha tests.
 */
import * as path from 'path';

export function makePathToFixture(name: string): string {
  return path.join(__dirname, '..', 'fixtures', name)
}