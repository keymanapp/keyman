import * as path from "path";
import { fileURLToPath } from "url";

/**
 * Builds a path to the common/web/types fixture with the given path components.
 *
 * e.g., makePathToFixture('basic.xml')
 *
 * @param components One or more path components.
 */
export function makePathToFixture(...components: string[]): string {
  return fileURLToPath(new URL(path.join('..', '..', '..', 'tests', 'fixtures', ...components), import.meta.url));
}

/**
 * Builds a path to the /common/test file with the given path components. Note
 * that this links to the base of /common/test, not /common/test/fixtures,
 * because the /common/test folder currently has a mix of paths.
 *
 * e.g., makePathToFixture('basic.xml')
 *
 * @param components One or more path components.
 */
export function makePathToCommonFixture(...components: string[]): string {
  return fileURLToPath(new URL(path.join('..', '..', '..', '..', '..', 'test', ...components), import.meta.url));
}
