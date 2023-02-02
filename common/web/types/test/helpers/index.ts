import path from "path";
import fs from "fs";
import { fileURLToPath } from "url";

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

export function loadKvksJsonSchema(): Buffer {
  return fs.readFileSync(new URL(path.join('..', '..', 'src', 'kvks.schema.json'), import.meta.url));
}

export function loadKeymanTouchLayoutCleanJsonSchema(): Buffer {
  return fs.readFileSync(new URL(path.join('..', '..', 'src', 'keyman-touch-layout.clean.spec.json'), import.meta.url));
}

export function loadFile(baseFilename: string, filename: string | URL): Buffer {
  // TODO: translate filename based on the baseFilename
  return fs.readFileSync(filename);
}

export function loadLdmlKeyboardSchema(): Buffer {
  return fs.readFileSync(new URL(path.join('..', '..', 'src', 'ldml-keyboard.schema.json'), import.meta.url));
}
