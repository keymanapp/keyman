/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import fs from 'node:fs';

import { getKeymanRoot } from 'keyman/test/resources';

const KEYMAN_ROOT = getKeymanRoot();

export function loadKeyboardBlob(filename: string) {
  const data = fs.readFileSync(`${KEYMAN_ROOT}${filename}`, null);
  return new Uint8Array(data);
}
