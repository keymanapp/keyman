import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { KeyboardInfoCompiler, KeyboardInfoCompilerResult } from '../src/keyboard-info-compiler.js';
import { KeyboardInfoCompilerMessages } from '../src/keyboard-info-compiler-messages.js';
import { KeymanFileTypes, KmpJsonFile } from '@keymanapp/common-types';

const callbacks = new TestCompilerCallbacks();

beforeEach(function() {
  callbacks.clear();
});

describe('keyboard-info-compiler', function () {
  it('compile a .keyboard_info file correctly', async function() {
    const kpjFilename = makePathToFixture('khmer_angkor', 'khmer_angkor.kpj');
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');
    const buildKeyboardInfoFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.keyboard_info');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    let result: KeyboardInfoCompilerResult = null;
    try {
      result = await compiler.run(kpjFilename, null);
    } catch(e) {
      callbacks.printMessages();
      throw e;
    }
    if(result == null) {
      callbacks.printMessages();
    }
    assert.isNotNull(result);

    const actual = JSON.parse(new TextDecoder().decode(result.artifacts.keyboard_info.data));
    const expected = JSON.parse(fs.readFileSync(buildKeyboardInfoFilename, 'utf-8'));

    // `lastModifiedDate` is dependent on time of run (not worth mocking)
    delete actual['lastModifiedDate'];
    delete expected['lastModifiedDate'];

    assert.deepEqual(actual, expected);
  });

  // ERROR_FileDoesNotExist (loadJsFile)

  it('should generate FileDoesNotExist error if .js file does not exist', async function() {
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'xxx.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    let result: KeyboardInfoCompilerResult = null;
    try {
      result = await compiler.run(kmpFilename, null);
    } catch(e) {
      callbacks.printMessages();
      throw e;
    }
    assert.isNull(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist),
      `ERROR_FileDoesNotExist not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist).includes(KeymanFileTypes.Binary.WebKeyboard),
      KeymanFileTypes.Binary.WebKeyboard+' not found in the message');
  });

  // ERROR_FileDoesNotExist (font file in package)

  it('should generate FileDoesNotExist error if font file is missing from package', async function() {
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const kmpJsonData: KmpJsonFile.KmpJsonFile = {system: {fileVersion: "7.0", keymanDeveloperVersion: "17.0.204"},
      options: {},
      files: []}
    const source = ["Mondulkiri-R.ttf"]
    const result = await compiler['fontSourceToKeyboardInfoFont'](kpsFilename, kmpJsonData, source)
    assert.isNull(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist),
      `ERROR_FileDoesNotExist not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
  });
});

function nodeCompilerMessage(ncb: TestCompilerCallbacks, code: number): string {
  return ncb.messages.find((item) => item.code == code).message ?? '';
}