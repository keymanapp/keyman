import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { assert } from 'chai';
import 'mocha';
import { makePathToFixture } from './helpers/index.js';
import { expandFileList, expandFileLists } from '../src/util/fileLists.js';

const callbacks = new TestCompilerCallbacks();

// expandFileList expands each file name relative to the file list's supplied
// filename, so we need to compare against the full path.
const expectedFileList = [
  makePathToFixture('file-lists', 'file1.kmn'),
  makePathToFixture('file-lists', 'file2.kmn'),
  makePathToFixture('file-lists', 'file with space.kps')
];

const expectedFiles = [
  'file0.kmn',
  ...expectedFileList,
  'file4.kmn',
  'file5.kmn'
];

beforeEach(function() {
  callbacks.clear();
});

describe('expandFileList', function () {
  it('should report a missing filelist correctly', async function() {
    const path = makePathToFixture('file-lists', 'does-not-exist.txt');

    const fileList = expandFileList(path, callbacks);
    assert.isNull(fileList);
    assert.equal(callbacks.messages.length, 1);
  });

  it('should expand a list of files correctly', async function() {
    const path = makePathToFixture('file-lists', 'files.txt');

    const fileList = expandFileList(path, callbacks);
    assert.isNotNull(fileList);
    assert.equal(callbacks.messages.length, 0);
    assert.deepEqual(fileList, expectedFileList);
  });
});

describe('expandFileLists', function () {
  it('should splice a filelist in correctly', async function() {
    // We just use this to test the splicing so no path resolution is made
    const files = [
      'file0.kmn',
      '@' + makePathToFixture('file-lists', 'files.txt'),
      'file4.kmn',
      'file5.kmn'
    ];

    assert.isTrue(expandFileLists(files, callbacks));
    assert.equal(callbacks.messages.length, 0);
    assert.deepEqual(files, expectedFiles);
  });
});
