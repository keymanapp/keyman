import { assert } from 'chai';
import 'mocha';
import { KeyboardInfoCompilerMessages } from '../src/keyboard-info-compiler-messages.js';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { KmpJsonFile, KeymanFileTypes } from '@keymanapp/common-types';
import { CompilerErrorNamespace } from '@keymanapp/developer-utils';
import { makePathToFixture } from './helpers/index.js';
import { KeyboardInfoCompiler } from '../src/keyboard-info-compiler.js';
import { KeyboardInfoFile } from '../src/keyboard-info-file.js';

const callbacks = new TestCompilerCallbacks();

const KHMER_ANGKOR_JS  = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
const KHMER_ANGKOR_KPS = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
const KHMER_ANGKOR_KMP = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');

const KHMER_ANGKOR_SOURCES = {
  kmpFilename: KHMER_ANGKOR_KMP,
  sourcePath: 'release/k/khmer_angkor',
  kpsFilename: KHMER_ANGKOR_KPS,
  jsFilename: KHMER_ANGKOR_JS,
  forPublishing: true,
};

describe('KeyboardInfoCompilerMessages', function () {

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  })

  it('should have a valid KeyboardInfoCompilerMessages object', function() {
    return verifyCompilerMessagesObject(KeyboardInfoCompilerMessages, CompilerErrorNamespace.KeyboardInfoCompiler);
  });

  // ERROR_FileDoesNotExist (loadJsFile)

  it('should generate ERROR_FileDoesNotExist error if .js file does not exist', async function() {
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'xxx.js');

    const sources = {
      ...KHMER_ANGKOR_SOURCES,
      jsFilename,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const result = await compiler.run(KHMER_ANGKOR_KMP, null);
    assert.isNull(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist),
      `ERROR_FileDoesNotExist not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist).includes(KeymanFileTypes.Binary.WebKeyboard),
      KeymanFileTypes.Binary.WebKeyboard+' not found in the message');
  });

  // ERROR_FileDoesNotExist (.kmp fileSize)

  it('should generate ERROR_FileDoesNotExist error if .kmp file does not exist', async function() {
    const jsFilename = makePathToFixture('no-kmp', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('no-kmp', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('no-kmp', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/no-kmp',
      kpsFilename,
      jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    // stubbing fillLanguages internal function to avoid pulling in font resources in fixture
    compiler['fillLanguages'] = async (_kpsFilename: string, _keyboard_info: KeyboardInfoFile, _kmpJsonData:  KmpJsonFile.KmpJsonFile): Promise<boolean> => true;
    const result = await compiler.run(kmpFilename, null);
    assert.isNull(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist),
      `ERROR_FileDoesNotExist not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist).includes(KeymanFileTypes.Binary.Package),
      KeymanFileTypes.Binary.Package+' not found in the message');
  });

  // ERROR_FileDoesNotExist (font file not in package)

  it('should generate ERROR_FileDoesNotExist error if font file is missing from package', async function() {
    const sources = KHMER_ANGKOR_SOURCES;
    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const kmpJsonData: KmpJsonFile.KmpJsonFile = {system: {fileVersion: "7.0", keymanDeveloperVersion: "17.0.204"},
      options: {},
      files: []}
    const source = ["Mondulkiri-R.ttf"]
    const result = await compiler['fontSourceToKeyboardInfoFont'](KHMER_ANGKOR_KPS, kmpJsonData, source)
    assert.isNull(result);
    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist),
      `ERROR_FileDoesNotExist not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist).includes(source[0]),
      source[0]+' not found in the message');
  });

  // ERROR_FileDoesNotExist (font file not on disk)

  it('should generate ERROR_FileDoesNotExist error if font file is missing from disk', async function() {
    const sources = KHMER_ANGKOR_SOURCES;
    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const kmpJsonData: KmpJsonFile.KmpJsonFile = {system: {fileVersion: "7.0", keymanDeveloperVersion: "17.0.204"},
      options: {},
      files: [{name: "../shared/fonts/khmer/mondulkiri/xxx.ttf", description: "Font not on disk"}]}
    const source = ["xxx.ttf"]
    const result = await compiler['fontSourceToKeyboardInfoFont'](KHMER_ANGKOR_KPS, kmpJsonData, source)
    assert.isNull(result);
    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist),
      `ERROR_FileDoesNotExist not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_FileDoesNotExist).includes(kmpJsonData.files[0].name),
    kmpJsonData.files[0].name+' not found in the message');
  });

  // ERROR_LicenseFileIsMissing

  it('should generate ERROR_LicenseFileIsMissing error if license file is missing from disk', async function() {
    const sources = KHMER_ANGKOR_SOURCES;
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
    const sources = KHMER_ANGKOR_SOURCES;
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
    const sources = KHMER_ANGKOR_SOURCES;
    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const licenseFilename = makePathToFixture('khmer_angkor', 'LICENSE.md');
    const originalDecode = TextDecoder.prototype.decode
    TextDecoder.prototype.decode = () => null;
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
    const sources = {
      ...KHMER_ANGKOR_SOURCES,
      sourcePath: 'release/k/invalid-license',
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

  // ERROR_CannotBuildWithoutKmpFile

  it('should generate ERROR_CannotBuildWithoutKmpFile error if .kmp file is not in sources', async function() {
    const jsFilename = makePathToFixture('no-kmp', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('no-kmp', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('no-kmp', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename: '',
      sourcePath: 'release/k/no-kmp',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const result = await compiler.run(kmpFilename, null);
    assert.isNull(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_CannotBuildWithoutKmpFile),
      `ERROR_CannotBuildWithoutKmpFile not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
  });

  // ERROR_NoLicenseFound

  it('should generate ERROR_NoLicenseFound error if licence file is not in .kps options', async function() {
    const jsFilename = makePathToFixture('no-license-in-kps-sources', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('no-license-in-kps-sources', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('no-license-in-kps-sources', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/no-license-in-kps-sources',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const result = await compiler.run(kmpFilename, null);
    assert.isNull(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_NoLicenseFound),
      `ERROR_NoLicenseFound not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
  });

  // ERROR_FontFileMetaDataIsInvalid

  it('should generate ERROR_FontFileMetaDataIsInvalid error if font file meta data throws an error', async function() {
    const jsFilename = makePathToFixture('font-meta-data-is-invalid', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('font-meta-data-is-invalid', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('font-meta-data-is-invalid', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/font-meta-data-is-invalid',
      kpsFilename,
      jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const kmpJsonData: KmpJsonFile.KmpJsonFile = {system: {fileVersion: "7.0", keymanDeveloperVersion: "17.0.204"},
      options: {},
      files: [{name: "../shared/fonts/khmer/mondulkiri/font-meta-data-is-invalid.ttf", description: ""}]}
    const source = ["font-meta-data-is-invalid.ttf"]
    const result = await compiler['fontSourceToKeyboardInfoFont'](kpsFilename, kmpJsonData, source)
    assert.isNull(result);
    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_FontFileMetaDataIsInvalid),
      `ERROR_FontFileMetaDataIsInvalid not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
    assert.isTrue(nodeCompilerMessage(callbacks, KeyboardInfoCompilerMessages.ERROR_FontFileMetaDataIsInvalid).includes(kmpJsonData.files[0].name),
      kmpJsonData.files[0].name+' not found in the message');
  });

  // ERROR_InvalidAuthorEmail

  it('should generate ERROR_InvalidAuthorEmail error if multiple email addresses are listed in .kps', async function() {
    const jsFilename = makePathToFixture('multiple-email-addresses', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('multiple-email-addresses', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('multiple-email-addresses', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/multiple-email-addresses',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const result = await compiler.run(kmpFilename, null);
    assert.isNull(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.ERROR_InvalidAuthorEmail),
      `ERROR_InvalidAuthorEmail not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
  });

  // HINT_ScriptDoesNotMatch

  it('should generate HINT_ScriptDoesNotMatch if there is a mismatching language script in .kps', async function() {
    const jsFilename = makePathToFixture('hint_script_does_not_match', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('hint_script_does_not_match', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('hint_script_does_not_match', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/h/hint_script_does_not_match',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const result = await compiler.run(kmpFilename, null);
    assert.isNotNull(result);

    assert.isTrue(callbacks.hasMessage(KeyboardInfoCompilerMessages.HINT_ScriptDoesNotMatch));
  });
});

function nodeCompilerMessage(ncb: TestCompilerCallbacks, code: number): string {
  return ncb.messages.find((item) => item.code == code).message ?? '';
}
