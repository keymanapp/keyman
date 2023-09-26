import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { KeyboardInfoCompiler } from '../src/index.js';
import { KmpCompiler } from '@keymanapp/kmc-package';

const callbacks = new TestCompilerCallbacks();

beforeEach(function() {
  callbacks.clear();
});

describe('keyboard-info-compiler', function () {
  it('compile a .keyboard_info file correctly', function() {
    const path = makePathToFixture('khmer_angkor', 'khmer_angkor.keyboard_info');
    const jsFileName = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFileName = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFileName = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');
    const buildKeyboardInfoFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.keyboard_info');

    const kmpCompiler = new KmpCompiler(callbacks);
    const kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsFileName);

    const compiler = new KeyboardInfoCompiler(callbacks);
    const data = compiler.writeMergedKeyboardInfoFile(path, {
      kmpFileName,
      kmpJsonData,
      keyboard_id: 'khmer_angkor',
      sourcePath: 'release/k/khmer_angkor',
      kpsFileName,
      helpLink: 'https://help.keyman.com/keyboard/khmer_angkor',
      keyboardFileNameJs: jsFileName,
    });
    assert.isNotNull(data);

    const actual = JSON.parse(new TextDecoder().decode(data));
    const expected = JSON.parse(fs.readFileSync(buildKeyboardInfoFilename, 'utf-8'));

    // `lastModifiedDate` is dependent on time of run (not worth mocking)
    delete actual['lastModifiedDate'];
    delete expected['lastModifiedDate'];

    assert.deepEqual(actual, expected);
  });

  it('compile a .keyboard_info file correctly when no source .keyboard_info exists', function() {
    this.skip(); // TODO: support keyboard_info when no source file exists (determine license from LICENSE.md)
  });
});
