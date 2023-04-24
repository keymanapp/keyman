import path from "path";
import fs from "fs";
import { fileURLToPath } from "url";
import { CompilerSchema } from "../../src/util/compiler-interfaces.js";

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

export function loadKeymanTouchLayoutCleanJsonSchema(): Buffer {
  // TODO: this has the Wrong Name Pattern!
  return fs.readFileSync(new URL(path.join('..', '..', 'src', 'keyman-touch-layout.clean.spec.json'), import.meta.url));
}

export function loadFile(baseFilename: string, filename: string | URL): Buffer {
  // TODO: translate filename based on the baseFilename
  return fs.readFileSync(filename);
}

export function loadSchema(schema: CompilerSchema): Buffer {
  return fs.readFileSync(new URL(path.join('..', '..', 'src', schema + '.schema.json'), import.meta.url));
}
