/*
 * Keyman is copyright (C) SIL Global. MIT License.
 * 
 * Created by Dr Mark C. Sinclair on 2024-11-28
 * 
 * Test code for file-types.ts
 */

import 'mocha';
import { assert } from 'chai';
import {
  ALL,
  ALL_SOURCE,
  ALL_BINARY,
  removeExtension,
  sourceOrBinaryTypeFromFilename
} from '../../src/util/file-types.js';

describe('Test of File-Types', () => {
  describe('Test of removeExtension()', () => {
    it('can remove Source file extension', () => {
      ALL_SOURCE.forEach((ext) => {
        const filename = `file${ext}`;
        const actual   = removeExtension(filename);
        assert.deepEqual(actual, "file");
      });
    });
    it('can remove Binary file extension', () => {
      ALL_BINARY.forEach((ext) => {
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
  describe('Test of sourceOrBinaryTypeFromFilename()', () => {
    it('can extract Source file extension', () => {
      ALL_SOURCE.forEach((ext) => {
        const filename = `file${ext}`;
        const actual   = sourceOrBinaryTypeFromFilename(filename);
        assert.deepEqual(actual, ext);
      });
    });
    it('can extract Binary file extension', () => {
      ALL_BINARY.forEach((ext) => {
        const filename = `file${ext}`;
        const actual   = sourceOrBinaryTypeFromFilename(filename);
        assert.deepEqual(actual, ext);
      });
    });
    it('returns null for unmatched file extension', () => {
      const ext = ".cpp";
      assert.isFalse((Object.values(ALL) as string[]).includes(ext));
      const filename = `file${ext}`;
      const actual   = sourceOrBinaryTypeFromFilename(filename);
      assert.isNull(actual);
    });
    it('can extract upper case file extension', () => {
      const ext          = ALL[0];
      const upperCaseExt = ext.toUpperCase();
      const filename     = `file${upperCaseExt}`;
      const actual       = sourceOrBinaryTypeFromFilename(filename);
      assert.deepEqual(actual, ext);
    });
  });
});