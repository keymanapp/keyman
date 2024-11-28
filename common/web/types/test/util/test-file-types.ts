/*
 * Keyman is copyright (C) SIL Global. MIT License.
 * 
 * Created by Dr Mark C. Sinclair on 2024-11-28
 * 
 * Test code for file-types.ts
 */

import 'mocha';
import { assert } from 'chai';
import { removeExtension } from '../../src/util/file-types.js';

const sourceExts = [ // can't use Source, as it is const enum
  '.model.ts',
  '.kpj',
  '.kmn',
  '.xml',
  '.kvks',
  '.keyman-touch-layout',
];

describe('Test of File-Types', () => {
  describe('Test of removeExtension()', () => {
    it('can remove Source file extension', () => {
      sourceExts.forEach((ext) => {
        const filename = `file${ext}`;
        const actual   = removeExtension(filename);
        assert.deepEqual(actual, "file");
      });
    });
    it('can handle no file extension', () => {
      const filename = removeExtension("file");
      assert.deepEqual(filename, "file");
    });
  });
});