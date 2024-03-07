import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { KeyboardInfoCompiler, KeyboardInfoCompilerResult } from '../src/keyboard-info-compiler.js';
import { KeyboardInfoCompilerMessages } from '../src/keyboard-info-compiler-messages.js';
import { KeymanFileTypes, KmpJsonFile } from '@keymanapp/common-types';
import { KeyboardInfoFile } from './keyboard-info-file.js';

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

  it('should generate ERROR_FileDoesNotExist error if .js file does not exist', async function() {
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
  
  // ERROR_FileDoesNotExist (.kmp fileSize)

  it('should generate FileDoesNotExist error if .kmp file does not exist', async function() {
    const jsFilename = makePathToFixture('no-kmp', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('no-kmp', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('no-kmp', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/no-kmp',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    // stubbing fillLanguages internal function to avoid pulling in font resources in fixture
    compiler['fillLanguages'] = async (_kpsFilename: string, _keyboard_info: KeyboardInfoFile, _kmpJsonData:  KmpJsonFile.KmpJsonFile): Promise<boolean> => true;
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
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist).includes(KeymanFileTypes.Binary.Package),
      KeymanFileTypes.Binary.Package+' not found in the message');
  });

  // ERROR_FileDoesNotExist (font file not in package)

  it('should generate ERROR_FileDoesNotExist error if font file is missing from package', async function() {
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
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist).includes(source[0]),
      source[0]+' not found in the message');
  });

  // ERROR_FileDoesNotExist (font file not on disk)

  it('should generate ERROR_FileDoesNotExist error if font file is missing from disk', async function() {
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
      files: [{name: "../shared/fonts/khmer/mondulkiri/xxx.ttf", description: "Font not on disk"}]}
    const source = ["xxx.ttf"]
    const result = await compiler['fontSourceToKeyboardInfoFont'](kpsFilename, kmpJsonData, source)
    assert.isNull(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist),
      `ERROR_FileDoesNotExist not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist).includes(kmpJsonData.files[0].name),
    kmpJsonData.files[0].name+' not found in the message');
  });

  // ERROR_LicenseFileIsMissing

  it('should generate ERROR_LicenseFileIsMissing error if license file is missing from disk', async function() {
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
    const licenseFilename = makePathToFixture('khmer_angkor', 'xxx.md');
    const result = compiler['isLicenseMIT'](licenseFilename)
    assert.isFalse(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_LicenseFileIsMissing),
      `ERROR_LicenseFileIsMissing not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_LicenseFileIsMissing).includes(licenseFilename),
      licenseFilename+' not found in the message');
  });
  
  // ERROR_LicenseFileIsDamaged (error on decode)

  it('should generate ERROR_LicenseFileIsDamaged error if license file throws error on decode', async function() {
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
    const licenseFilename = makePathToFixture('khmer_angkor', 'LICENSE.md');
    const originalDecode = TextDecoder.prototype.decode
    TextDecoder.prototype.decode = () => { throw new Error() }
    const result = compiler['isLicenseMIT'](licenseFilename)
    TextDecoder.prototype.decode = originalDecode
    assert.isFalse(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_LicenseFileIsDamaged),
      `ERROR_LicenseFileIsDamaged not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_LicenseFileIsDamaged).includes(licenseFilename),
      licenseFilename+' not found in the message');
  });

  // ERROR_LicenseFileIsDamaged (null on decode)

  it('should generate ERROR_LicenseFileIsDamaged error if license file returns null on decode', async function() {
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
    const licenseFilename = makePathToFixture('khmer_angkor', 'LICENSE.md');
    const originalDecode = TextDecoder.prototype.decode
    TextDecoder.prototype.decode = () => { return null }
    const result = compiler['isLicenseMIT'](licenseFilename)
    TextDecoder.prototype.decode = originalDecode
    assert.isFalse(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_LicenseFileIsDamaged),
      `ERROR_LicenseFileIsDamaged not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_LicenseFileIsDamaged).includes(licenseFilename),
      licenseFilename+' not found in the message');
  });

  // ERROR_LicenseIsNotValid

  it('should generate ERROR_LicenseIsNotValid error if license file is invalid', async function() {
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/invalid-license',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const licenseFilename = makePathToFixture('invalid-license', 'invalid_license.md');
    const result = compiler['isLicenseMIT'](licenseFilename)
    assert.isFalse(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_LicenseIsNotValid),
      `ERROR_LicenseIsNotValid not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_LicenseIsNotValid).includes(licenseFilename),
      licenseFilename+' not found in the message');
  });  
});

function nodeCompilerMessage(ncb: TestCompilerCallbacks, code: number): string {
  return ncb.messages.find((item) => item.code == code).message ?? '';
}
