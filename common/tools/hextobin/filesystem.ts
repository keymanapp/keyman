/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * hextobin filesystem interfaces
 */

import * as fs from 'node:fs';
import { hextobin, HexToBinOptions } from './main.js';

export function hextobinFromFile(inputFilename: string, outputFilename?: string, options?: HexToBinOptions): Uint8Array {
  const result = hextobin(fs.readFileSync(inputFilename, 'utf-8'), options);
  if(result && outputFilename) {
    fs.writeFileSync(outputFilename, result);
  }
  return result;
}
