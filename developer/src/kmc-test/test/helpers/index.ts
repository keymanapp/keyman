/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import * as path from "node:path";
import { fileURLToPath } from "node:url";

/**
 * Builds a path to the fixture with the given path components.
 *
 * e.g., makePathToFixture('basic.xml')
 *
 * @param components One or more path components.
 */
export function makePathToFixture(...components: string[]): string {
  return fileURLToPath(new URL(path.join('..', '..', '..', 'test', 'fixtures', ...components), import.meta.url));
}
