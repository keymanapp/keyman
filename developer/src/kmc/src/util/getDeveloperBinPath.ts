import { fileURLToPath } from 'url';
import * as path from 'path';
import * as fs from 'fs';

/**
 * Locates the Keyman Developer bin folder, checking first if this is running
 * from the source repository (using the presence of the KEYMAN_ROOT environment
 * variable), and if not, checking for the presence of kmconvert.exe in each
 * parent folder recursively until we reach the root of the filesystem.
 * @returns string | null
 */
export function getDeveloperBinPath(): string {
  // if running in source repo, then we always look in developer/bin/
  const keymanRoot = process.env['KEYMAN_ROOT'];
  if (keymanRoot) {
    // https://stackoverflow.com/a/45242825/1836776 (imperfect due to
    // possibility of '..foo', but good enough)
    const rel = path.relative(keymanRoot, import.meta.url);
    if (rel && !rel.startsWith('..') && !path.isAbsolute(rel)) {
      return path.join(keymanRoot, 'developer', 'bin');
    }
  }

  // Otherwise, we will look in parent folders until we find kmconvert.exe
  // TODO: if we eliminate kmconvert.exe, we'll need to do this some other way?
  let p = fileURLToPath(import.meta.url);
  const root = path.parse(p).root.toLowerCase(); // lower-case for drive letter on Windows
  p = path.dirname(p);
  do {
    if (fs.existsSync(path.join(p, 'kmconvert.exe'))) {
      return p;
    }
    p = path.dirname(p);
  } while (p.toLowerCase() != root);

  // kmconvert.exe was not found on the path
  return null;
}
