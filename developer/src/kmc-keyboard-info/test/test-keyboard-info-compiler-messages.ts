import { assert } from 'chai';
import 'mocha';
import { KeyboardInfoCompilerMessages } from '../src/keyboard-info-compiler-messages.js';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace, KmpJsonFile } from '@keymanapp/common-types';
import { makePathToFixture } from './helpers/index.js';
import { KeyboardInfoCompiler } from '../src/keyboard-info-compiler.js';

const callbacks = new TestCompilerCallbacks();

beforeEach(function() {
  callbacks.clear();
});

describe('KeyboardInfoCompilerMessages', function () {
  it('should have a valid KeyboardInfoCompilerMessages object', function() {
    return verifyCompilerMessagesObject(KeyboardInfoCompilerMessages, CompilerErrorNamespace.KeyboardInfoCompiler);
  });

  // ERROR_FontFileCannotBeRead

  it('should generate ERROR_FontFileCannotBeRead error if font family cannot be obtained from file', async function() {
    const jsFilename = makePathToFixture('font-file-cannot-be-read', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('font-file-cannot-be-read', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('font-file-cannot-be-read', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/font-file-cannot-be-read',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const kmpJsonData: KmpJsonFile.KmpJsonFile = {system: {fileVersion: "7.0", keymanDeveloperVersion: "17.0.204"},
      options: {},
      files: [{name: "../shared/fonts/khmer/mondulkiri/font_file_cannot_be_read.ttf", description: ""}]}
    const source = ["font_file_cannot_be_read.ttf"]
    const result = await compiler['fontSourceToKeyboardInfoFont'](kpsFilename, kmpJsonData, source)
    assert.isNull(result);
    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_FontFileCannotBeRead),
      `ERROR_FontFileCannotBeRead not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_FontFileCannotBeRead).includes(kmpJsonData.files[0].name),
      kmpJsonData.files[0].name+' not found in the message');
  });
});

function nodeCompilerMessage(ncb: TestCompilerCallbacks, code: number): string {
  return ncb.messages.find((item) => item.code == code).message ?? '';
}
