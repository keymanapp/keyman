/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import fs from 'node:fs';
import { pathToFileURL } from 'node:url';

const KEYMAN_ROOT = process.env['KEYMAN_ROOT'] ?? '.';
export const coreurl = pathToFileURL(`${KEYMAN_ROOT}/web/build/engine/obj/core-adapter/import/core`).toString();

export function loadKeyboardBlob(filename: string) {
  const data = fs.readFileSync(`${KEYMAN_ROOT}${filename}`, null);
  return new Uint8Array(data);
}
