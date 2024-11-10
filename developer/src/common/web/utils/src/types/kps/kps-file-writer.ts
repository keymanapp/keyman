/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2024-10-16
 *
 * XML writer for .kps file format
 */

import { KeymanXMLWriter } from '../../index.js';
import { KpsPackage } from './kps-file.js';

export class KpsFileWriter {
  public write(kpsFile: KpsPackage): string {
    const result = new KeymanXMLWriter('kps').write(kpsFile);
    return result;
  }
}
