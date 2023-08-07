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
    const licenseFilename = makePathToFixture('khmer_angkor', 'LICENSE.md');
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');
    const buildKeyboardInfoFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.keyboard_info');

    const kmpCompiler = new KmpCompiler(callbacks);
    const kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsFilename);

    const compiler = new KeyboardInfoCompiler(callbacks);
    const data = compiler.writeMergedKeyboardInfoFile(path, {
      kmpFilename,
      kmpJsonData,
      keyboard_id: 'khmer_angkor',
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      helpLink: 'https://help.keyman.com/keyboard/khmer_angkor',
      keyboardFilenameJs: jsFilename,
      licenseFilename
    });
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

  it('compile a .keyboard_info file correctly when no source .keyboard_info exists', function() {
    // Note that this file should not exist:
    const path = makePathToFixture('khmer_angkor', 'khmer_angkor_(missing).keyboard_info');

    const licenseFilename = makePathToFixture('khmer_angkor', 'LICENSE.md');
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');

    // This file mirrors the real .keyboard_info, but strips fields that cannot
    // currently be constructed from the package metadata
    const buildKeyboardInfoFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor_(no_source).keyboard_info');

    const kmpCompiler = new KmpCompiler(callbacks);
    const kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsFilename);

    const compiler = new KeyboardInfoCompiler(callbacks);
    const data = compiler.writeMergedKeyboardInfoFile(path, {
      kmpFilename,
      kmpJsonData,
      keyboard_id: 'khmer_angkor',
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      helpLink: 'https://help.keyman.com/keyboard/khmer_angkor',
      keyboardFilenameJs: jsFilename,
      licenseFilename,
    });
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
