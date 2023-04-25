import 'mocha';
import {assert} from 'chai';
import {basename} from '../../src/util/util.js';

describe('test basename()', function() {
  it("should correctly extract a base name", function() {
    assert.equal(basename("file.ext", ".ext"), "file");
    assert.equal(basename("/tmp/file.ext", ".ext"), "file");
    assert.equal(basename("..\\myfile.xml", ".xml"), "myfile");
    assert.equal(basename("C:\\temp.ext\\file.ext", ".ext"), "file");
  });

  it("should return null if ext cannot be matched", function() {
    assert.isNull(basename("file.ext", ".xml"));
    assert.isNull(basename("file.xml.ext", ".xml"));
  });
});
