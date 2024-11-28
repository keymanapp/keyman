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
  Binary,
  removeExtension,
  sourceOrBinaryTypeFromFilename,
  sourceTypeFromFilename,
  binaryTypeFromFilename,
  filenameIs,
  replaceExtension,
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
  describe('Test of sourceTypeFromFilename()', () => {
    it('can extract Source file extension', () => {
      ALL_SOURCE.forEach((ext) => {
        const filename = `file${ext}`;
        const actual   = sourceTypeFromFilename(filename);
        assert.deepEqual(actual, ext);
      });
    });
    it('returns null for a Binary file extension', () => {
      ALL_BINARY.forEach((ext) => {
        const filename = `file${ext}`;
        const actual   = sourceTypeFromFilename(filename);
        assert.isNull(actual);
      });
    });
    it('returns null for unmatched file extension', () => {
      const ext = ".cpp";
      assert.isFalse((Object.values(ALL_SOURCE) as string[]).includes(ext));
      const filename = `file${ext}`;
      const actual   = sourceTypeFromFilename(filename);
      assert.isNull(actual);
    });
    it('can extract upper case file extension', () => {
      const ext          = ALL_SOURCE[0];
      const upperCaseExt = ext.toUpperCase();
      const filename     = `file${upperCaseExt}`;
      const actual       = sourceTypeFromFilename(filename);
      assert.deepEqual(actual, ext);
    });
  });
  describe('Test of binaryTypeFromFilename()', () => {
    it('returns null for a Source file extension', () => {
      ALL_SOURCE.forEach((ext) => {
        const filename = `file${ext}`;
        const actual   = binaryTypeFromFilename(filename);
        assert.isNull(actual);
      });
    });
    it('can extract Binary file extension', () => {
      ALL_BINARY.forEach((ext) => {
        const filename = `file${ext}`;
        const actual   = binaryTypeFromFilename(filename);
        assert.deepEqual(actual, ext);
      });
    });
    it('returns null for unmatched file extension', () => {
      const ext = ".cpp";
      assert.isFalse((Object.values(ALL_BINARY) as string[]).includes(ext));
      const filename = `file${ext}`;
      const actual   = binaryTypeFromFilename(filename);
      assert.isNull(actual);
    });
    it('can extract upper case file extension', () => {
      const ext          = ALL_BINARY[0];
      const upperCaseExt = ext.toUpperCase();
      const filename     = `file${upperCaseExt}`;
      const actual       = binaryTypeFromFilename(filename);
      assert.deepEqual(actual, ext);
    });
  });
  describe('Test of filenameIs()', () => {
    it('can identify Source file extension', () => {
      ALL_SOURCE.forEach((ext) => {
        const filename = `file${ext}`;
        const actual   = filenameIs(filename, ext);
        assert.isTrue(actual);
      });
    });
    it('can identify Binary file extension', () => {
      ALL_BINARY.forEach((ext) => {
        const filename = `file${ext}`;
        if (ext == Binary.Model) { // Special case for .model.js
          const actual = filenameIs(filename, Binary.WebKeyboard);
          assert.isFalse(actual);
        }
        const actual = filenameIs(filename, ext);
        assert.isTrue(actual);
      });
    });
    it('can identify upper case file extension', () => {
      const ext          = ALL[0];
      const upperCaseExt = ext.toUpperCase();
      const filename     = `file${upperCaseExt}`;
      const actual       = filenameIs(filename, ext);
      assert.isTrue(actual);
    });
  });
  describe('Test of replaceExtension()', () => {
    it('can replace an extension', () => {
      const oldExt      = ".cpp";
      const newExt      = ".js";
      const oldFilename = `file${oldExt}`;
      const newFilename = `file${newExt}`;
      const actual      = replaceExtension(oldFilename, oldExt, newExt);
      assert.deepEqual(actual, newFilename);
    });
    it('should return null for incorrect old extension (too short)', () => {
      const oldExt      = ".ts";
      const newExt      = ".js";
      const oldFilename = `file.c`;
      const actual      = replaceExtension(oldFilename, oldExt, newExt);
      assert.isNull(actual);
    });
    it('should return null for incorrect old extension (too long)', () => {
      const oldExt      = ".ts";
      const newExt      = ".js";
      const oldFilename = `file.cpp`;
      const actual      = replaceExtension(oldFilename, oldExt, newExt);
      assert.isNull(actual);
    });
    // it('should return null for null old extension', () => {
    //   const newExt      = ".js";
    //   const oldFilename = `file.ts`;
    //   const actual      = replaceExtension(oldFilename, null, newExt);
    //   assert.isNull(actual);
    // });
    // it('should return null for null new extension', () => {
    //   const oldExt      = ".ts";
    //   const oldFilename = `file.ts`;
    //   const actual      = replaceExtension(oldFilename, oldExt, null);
    //   assert.isNull(actual);
    // });
  });
});