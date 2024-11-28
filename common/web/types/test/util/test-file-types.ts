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

describe('Test of File-Types', () => {
  describe('Test of removeExtension()', () => {
    it('can remove file extension', () => {
      const filename = removeExtension("file.kmn");
      assert.deepEqual(filename, "file");
    });
    it('can handle no file extension', () => {
      const filename = removeExtension("file");
      assert.deepEqual(filename, "file");
    });
  });
});