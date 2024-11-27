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