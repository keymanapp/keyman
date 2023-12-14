import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { KeyboardInfoCompiler } from '../src/index.js';

const callbacks = new TestCompilerCallbacks();

beforeEach(function() {
  callbacks.clear();
});

describe('keyboard-info-compiler', function () {
  it('compile a .keyboard_info file correctly', async function() {
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');
    const buildKeyboardInfoFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.keyboard_info');

    const compiler = new KeyboardInfoCompiler(callbacks);
    let data = null;
    try {
      data = await compiler.writeKeyboardInfoFile({
        kmpFilename,
        sourcePath: 'release/k/khmer_angkor',
        kpsFilename,
        jsFilename: jsFilename,
        forPublishing: true,
      });
    } catch(e) {
      callbacks.printMessages();
      throw e;
    }
    if(data == null) {
      callbacks.printMessages();
    }
    assert.isNotNull(data);

    const actual = JSON.parse(new TextDecoder().decode(data));
    const expected = JSON.parse(fs.readFileSync(buildKeyboardInfoFilename, 'utf-8'));

    // `lastModifiedDate` is dependent on time of run (not worth mocking)
    delete actual['lastModifiedDate'];
    delete expected['lastModifiedDate'];

    assert.deepEqual(actual, expected);
  });
});
