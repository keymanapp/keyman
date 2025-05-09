/**
 * Helpers and utilities for the Mocha tests.
 */
import * as path from 'node:path';
import * as fs from "node:fs";
import { fileURLToPath } from 'node:url';

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

export function loadFile(filename: string | URL): Buffer {
  return fs.readFileSync(filename);
}

export function resolveFilename(baseFilename: string, filename: string) {
  const basePath =
    baseFilename.endsWith('/') || baseFilename.endsWith('\\') ?
    baseFilename :
    path.dirname(baseFilename);
  // Transform separators to platform separators -- we are agnostic
  // in our use here but path prefers files may use
  // either / or \, although older kps files were always \.
  if(path.sep == '/') {
    filename = filename.replace(/\\/g, '/');
  } else {
    filename = filename.replace(/\//g, '\\');
  }
  if(!path.isAbsolute(filename)) {
    filename = path.resolve(basePath, filename);
  }
  return filename;
}
