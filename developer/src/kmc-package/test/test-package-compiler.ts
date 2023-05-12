import 'mocha';
import * as fs from 'fs';
import { assert } from 'chai';
import JSZip from 'jszip';

import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { KmpJsonFile } from '@keymanapp/common-types';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

import { makePathToFixture } from './helpers/index.js';

import { KmpCompiler } from '../src/compiler/kmp-compiler.js';
import { PackageValidation } from '../src/compiler/package-validation.js';
import { CompilerMessages } from '../src/compiler/messages.js';

const debug = false;

describe('KmpCompiler', function () {
  const MODELS : string[] = [
    'example.qaa.sencoten',
    'withfolders.qaa.sencoten',
  ];

  const callbacks = new TestCompilerCallbacks();
  let kmpCompiler = new KmpCompiler(callbacks);

  for (let modelID of MODELS) {
    const kpsPath = modelID.includes('withfolders') ?
      makePathToFixture(modelID, 'source', `${modelID}.model.kps`) : makePathToFixture(modelID, `${modelID}.model.kps`);
    const jsPath = modelID.includes('withfolders') ?
      makePathToFixture(modelID, 'source', `${modelID}.model.js`) : makePathToFixture(modelID, `${modelID}.model.js`);
    const kmpJsonIntermediatePath = makePathToFixture(modelID, `${modelID}.model.kmp.intermediate.json`);
    const kmpJsonZippedPath = makePathToFixture(modelID, `${modelID}.model.kmp.zipped.json`);
    const kmpJsonIntermediateFixture = JSON.parse(fs.readFileSync(kmpJsonIntermediatePath, 'utf-8'));
    const kmpJsonZippedFixture = JSON.parse(fs.readFileSync(kmpJsonZippedPath, 'utf-8'));

    // We override the fixture version so that we can compare with the compiler output
    kmpJsonIntermediateFixture.system.keymanDeveloperVersion = KEYMAN_VERSION.VERSION;

    //
    // Test just the transform from kps to kmp.json
    //
    it(`should transform ${modelID}.model.kps to kmp.json`, function () {
      let kmpJson: KmpJsonFile.KmpJsonFile;

      assert.doesNotThrow(() => {
        kmpJson = kmpCompiler.transformKpsToKmpObject(kpsPath);
      });

      // Test that the kmp.json data is identical
      assert.deepEqual(kmpJson, kmpJsonIntermediateFixture);

      // Note that in-memory kmp.json still contains paths in the files array.
      // However, when building the .kmp, the final written kmp.json data is
      // modified to strip paths.

      // This was used when building initial test data
      //fs.writeFileSync(kmpJsonPath, JSON.stringify(kmpJson), 'utf-8');
    });
    it(`should build a full .kmp for ${modelID}`, async function() {
      const zip = JSZip();
      // Build kmp.json in memory
      const kmpJson: KmpJsonFile.KmpJsonFile = kmpCompiler.transformKpsToKmpObject(kpsPath);
      // Build file.kmp in memory
      const promise = kmpCompiler.buildKmpFile(kpsPath, kmpJson);
      promise.then(data => {
        // Check that file.kmp contains just 2 files - kmp.json and file.model.js,
        // and that they match exactly what we expect
        return zip.loadAsync(data, {checkCRC32: true}).then(zipFile => {
          assert.equal(zipFile.length, 2);
          return Promise.all([
            zipFile.file("kmp.json").async('uint8array').then(kmpJsonOutput => {
              assert.deepEqual(kmpJsonOutput, kmpJsonZippedFixture);
            }),
            zipFile.file(`${modelID}.model.js`).async('uint8array').then(modelJsFile => {
              assert.deepEqual(modelJsFile, fs.readFileSync(jsPath));
            })
          ]);
        });
      });

      return promise;
    });
  }

  it('should generates a valid .kmp (zip) file', async function() {
    this.timeout(10000); // building a zip file can sometimes be slow

    const kpsPath = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpJsonRefPath = makePathToFixture('khmer_angkor', 'ref', 'kmp.json');

    const kmpCompiler = new KmpCompiler(callbacks);
    const kmpJsonFixture: KmpJsonFile.KmpJsonFile = JSON.parse(fs.readFileSync(kmpJsonRefPath, 'utf-8'));

    // We override the fixture version so that we can compare with the compiler output
    kmpJsonFixture.system.keymanDeveloperVersion = KEYMAN_VERSION.VERSION;

    let kmpJson = null;
    assert.doesNotThrow(() => {
      kmpJson = kmpCompiler.transformKpsToKmpObject(kpsPath);
    });

    const kmpData = await kmpCompiler.buildKmpFile(kpsPath, kmpJson);

    const zip = JSZip();

    let jszip = await zip.loadAsync(kmpData);
    assert.isNotNull(jszip.file('kmp.json')); // kmp.json should be present
    // kmp file should contain the following files
    const expectedFiles = [
      'FONTLOG.txt', 'image002.png', 'KAK_Documentation_EN.pdf', 'KAK_Documentation_KH.pdf',
      'keyboard_layout.png', 'khmer_angkor.js', 'khmer_angkor.kmx', 'khmer_angkor.kvk',
      'khmer_busra_kbd.ttf', 'Mondulkiri-R.ttf', 'OFL.txt', 'OFL-FAQ.txt', 'readme.htm',
      'splash.gif', 'welcome.htm',
      'kmp.json', // standard .kmp metadata file
    ];

    assert.sameMembers(Object.entries(jszip.files).map(([s, o]) => o.name).sort(),
      expectedFiles.sort(),
      'khmer_angkor.kmp file should have exactly the expected files');

    let kmpJsonData = JSON.parse(await jszip.file('kmp.json').async('string'));
    assert.deepEqual(kmpJsonData, kmpJsonFixture);
  });

  /*
   * Testing Warnings and Errors
   */

  it('should warn on absolute paths', async function() {
    this.timeout(10000); // building a zip file can sometimes be slow

    callbacks.clear();

    const kpsPath = makePathToFixture('absolute_path', 'source', 'absolute_path.kps');
    const kmpCompiler = new KmpCompiler(callbacks);

    let kmpJson: KmpJsonFile.KmpJsonFile = null;

    assert.doesNotThrow(() => {
      kmpJson = kmpCompiler.transformKpsToKmpObject(kpsPath);
    });

    await assert.isNull(kmpCompiler.buildKmpFile(kpsPath, kmpJson));

    if(debug) callbacks.printMessages();

    assert.lengthOf(callbacks.messages, 2);
    assert.deepEqual(callbacks.messages[0].code, CompilerMessages.WARN_AbsolutePath);
    assert.deepEqual(callbacks.messages[1].code, CompilerMessages.ERROR_FileDoesNotExist);
  });

  //
  // Message tests
  //

  function testForMessage(context: Mocha.Context, fixture: string[], messageId?: number) {
    context.timeout(10000);

    callbacks.clear();

    const kpsPath = makePathToFixture(...fixture);
    const kmpCompiler = new KmpCompiler(callbacks);

    let kmpJson = kmpCompiler.transformKpsToKmpObject(kpsPath);
    if(kmpJson && callbacks.messages.length == 0) {
      const validator = new PackageValidation(callbacks);
      validator.validate(kpsPath, kmpJson); // we'll ignore return value and rely on the messages
    }

    if(kmpJson && callbacks.messages.length == 0) {
      // We'll try building the package if we have not yet received any messages
      kmpCompiler.buildKmpFile(kpsPath, kmpJson)
    }

    if(debug) callbacks.printMessages();

    if(messageId) {
      assert.lengthOf(callbacks.messages, 1);
      assert.isTrue(callbacks.hasMessage(messageId));
    } else {
      assert.lengthOf(callbacks.messages, 0);
    }
  }

  // WARN_FileIsNotABinaryKvkFile

  it('should generate WARN_FileIsNotABinaryKvkFile if a non-binary kvk file is included', async function() {
    testForMessage(this, ['xml_kvk_file', 'source', 'xml_kvk_file.kps'], CompilerMessages.WARN_FileIsNotABinaryKvkFile);
  });

  it('should not warn if a binary kvk file is included', async function() {
    testForMessage(this, ['binary_kvk_file', 'source', 'binary_kvk_file.kps']);
  });

  // ERROR_FollowKeyboardVersionNotAllowedForModelPackages

  it('should generate ERROR_FollowKeyboardVersionNotAllowedForModelPackages if <FollowKeyboardVersion> is set for model packages', async function() {
    testForMessage(this, ['invalid', 'followkeyboardversion.qaa.sencoten.model.kps'], CompilerMessages.ERROR_FollowKeyboardVersionNotAllowedForModelPackages);
  });

  // ERROR_FollowKeyboardVersionButNoKeyboards

  it('should generate ERROR_FollowKeyboardVersionButNoKeyboards if <FollowKeyboardVersion> is set for a package with no keyboards', async function() {
    testForMessage(this, ['invalid', 'followkeyboardversion.empty.kps'], CompilerMessages.ERROR_FollowKeyboardVersionButNoKeyboards);
  });

  // ERROR_KeyboardContentFileNotFound

  it('should generate ERROR_KeyboardContentFileNotFound if a <Keyboard> is listed in a package but not found in <Files>', async function() {
    testForMessage(this, ['invalid', 'keyboardcontentfilenotfound.kps'], CompilerMessages.ERROR_KeyboardContentFileNotFound);
  });

  // ERROR_KeyboardFileNotValid

  it('should generate ERROR_KeyboardFileNotValid if a .kmx is not valid in <Files>', async function() {
    testForMessage(this, ['invalid', 'keyboardfilenotvalid.kps'], CompilerMessages.ERROR_KeyboardFileNotValid);
  });

  // WARN_KeyboardFileHasNoKeyboardVersion

  it('should generate WARN_KeyboardFileHasNoKeyboardVersion if <FollowKeyboardVersion> is set but keyboard has no version', async function() {
    testForMessage(this, ['invalid', 'nokeyboardversion.kps'], CompilerMessages.WARN_KeyboardFileHasNoKeyboardVersion);
  });

  // ERROR_PackageCannotContainBothModelsAndKeyboards

  it('should generate ERROR_PackageCannotContainBothModelsAndKeyboards if package has both keyboards and models', async function() {
    testForMessage(this, ['invalid', 'ERROR_PackageCannotContainBothModelsAndKeyboards.kps'], CompilerMessages.ERROR_PackageCannotContainBothModelsAndKeyboards);
  });

  // WARN_PackageShouldNotRepeatLanguages (models)

  it('should generate WARN_PackageShouldNotRepeatLanguages if model has same language repeated', async function() {
    testForMessage(this, ['invalid', 'keyman.en.warn_package_should_not_repeat_languages.model.kps'], CompilerMessages.WARN_PackageShouldNotRepeatLanguages);
  });

  // WARN_PackageShouldNotRepeatLanguages (keyboards)

  it('should generate WARN_PackageShouldNotRepeatLanguages if keyboard has same language repeated', async function() {
    testForMessage(this, ['invalid', 'warn_package_should_not_repeat_languages.kps'], CompilerMessages.WARN_PackageShouldNotRepeatLanguages);
  });

  // WARN_PackageNameDoesNotFollowLexicalModelConventions

  it('should generate WARN_PackageNameDoesNotFollowLexicalModelConventions if filename has wrong conventions', async function() {
    testForMessage(this, ['invalid', 'WARN_PackageNameDoesNotFollowLexicalModelConventions.kps'], CompilerMessages.WARN_PackageNameDoesNotFollowLexicalModelConventions);
  });

  // WARN_PackageNameDoesNotFollowKeyboardConventions

  it('should generate WARN_PackageNameDoesNotFollowKeyboardConventions if filename has wrong conventions', async function() {
    testForMessage(this, ['invalid', 'WARN_PackageNameDoesNotFollowKeyboardConventions.kps'], CompilerMessages.WARN_PackageNameDoesNotFollowKeyboardConventions);
  });

  // WARN_FileInPackageDoesNotFollowFilenameConventions

  it('should generate WARN_FileInPackageDoesNotFollowFilenameConventions if content filename has wrong conventions', async function() {
    testForMessage(this, ['invalid', 'warn_file_in_package_does_not_follow_filename_conventions.kps'], CompilerMessages.WARN_FileInPackageDoesNotFollowFilenameConventions);
    testForMessage(this, ['invalid', 'warn_file_in_package_does_not_follow_filename_conventions_2.kps'], CompilerMessages.WARN_FileInPackageDoesNotFollowFilenameConventions);
  });

  // ERROR_PackageNameCannotBeBlank

  it('should generate ERROR_PackageNameCannotBeBlank if package info has empty name', async function() {
    testForMessage(this, ['invalid', 'error_package_name_cannot_be_blank.kps'], CompilerMessages.ERROR_PackageNameCannotBeBlank); // blank field
    testForMessage(this, ['invalid', 'error_package_name_cannot_be_blank_2.kps'], CompilerMessages.ERROR_PackageNameCannotBeBlank); // missing field
    testForMessage(this, ['invalid', 'error_package_name_cannot_be_blank_3.kps'], CompilerMessages.ERROR_PackageNameCannotBeBlank); // missing info section
  });

  // ERROR_KeyboardFileNotFound

  it('should generate ERROR_KeyboardFileNotFound if a <Keyboard> is listed in a package but not found in <Files>', async function() {
    testForMessage(this, ['invalid', 'keyboardfilenotfound.kps'], CompilerMessages.ERROR_KeyboardFileNotFound);
  });

});
