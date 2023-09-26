import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerMessages } from '../src/compiler/messages.js';
import { makePathToFixture } from './helpers/index.js';
import { KmpCompiler } from '../src/compiler/kmp-compiler.js';
import { PackageValidation } from '../src/compiler/package-validation.js';
import { CompilerErrorNamespace, CompilerOptions } from '@keymanapp/common-types';

const debug = false;
const callbacks = new TestCompilerCallbacks();

describe('CompilerMessages', function () {
  it('should have a valid CompilerMessages object', function() {
    return verifyCompilerMessagesObject(CompilerMessages, CompilerErrorNamespace.PackageCompiler);
  });


  //
  // Message tests
  //

  function testForMessage(context: Mocha.Context, fixture: string[], messageId?: number, options?: CompilerOptions) {
    context.timeout(10000);

    callbacks.clear();

    const kpsPath = makePathToFixture(...fixture);
    const kmpCompiler = new KmpCompiler(callbacks);

    let kmpJson = kmpCompiler.transformKpsToKmpObject(kpsPath);
    if(kmpJson && callbacks.messages.length == 0) {
      const validator = new PackageValidation(callbacks, options ?? {});
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

  // INFO_KeyboardFileHasNoKeyboardVersion

  it('should generate INFO_KeyboardFileHasNoKeyboardVersion if <FollowKeyboardVersion> is set but keyboard has no version', async function() {
    testForMessage(this, ['invalid', 'nokeyboardversion.kps'], CompilerMessages.INFO_KeyboardFileHasNoKeyboardVersion);
  });

  // ERROR_PackageCannotContainBothModelsAndKeyboards

  it('should generate ERROR_PackageCannotContainBothModelsAndKeyboards if package has both keyboards and models', async function() {
    testForMessage(this, ['invalid', 'error_package_cannot_contain_both_models_and_keyboards.kps'], CompilerMessages.ERROR_PackageCannotContainBothModelsAndKeyboards);
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
    testForMessage(this, ['invalid', 'warn_file_in_package_does_not_follow_filename_conventions.kps'],
      CompilerMessages.WARN_FileInPackageDoesNotFollowFilenameConventions, {checkFilenameConventions: true});
    testForMessage(this, ['invalid', 'warn_file_in_package_does_not_follow_filename_conventions_2.kps'],
      CompilerMessages.WARN_FileInPackageDoesNotFollowFilenameConventions, {checkFilenameConventions: true});
  });

  // ERROR_PackageNameCannotBeBlank

  it('should generate ERROR_PackageNameCannotBeBlank if package info has empty name', async function() {
    testForMessage(this, ['invalid', 'error_package_name_cannot_be_blank.kps'], CompilerMessages.ERROR_PackageNameCannotBeBlank); // blank field
    testForMessage(this, ['invalid', 'error_package_name_cannot_be_blank_2.kps'], CompilerMessages.ERROR_PackageNameCannotBeBlank); // missing field
  });

  // ERROR_KeyboardFileNotFound

  it('should generate ERROR_KeyboardFileNotFound if a <Keyboard> is listed in a package but not found in <Files>', async function() {
    testForMessage(this, ['invalid', 'keyboardfilenotfound.kps'], CompilerMessages.ERROR_KeyboardFileNotFound);
  });

  // WARN_KeyboardVersionsDoNotMatch

  it('should generate WARN_KeyboardVersionsDoNotMatch if two <Keyboards> have different versions', async function() {
    testForMessage(this, ['invalid', 'warn_keyboard_versions_do_not_match.kps'], CompilerMessages.WARN_KeyboardVersionsDoNotMatch);
  });

  // ERROR_LanguageTagIsNotValid

  it('should generate ERROR_LanguageTagIsNotValid if keyboard has an invalid language tag', async function() {
    testForMessage(this, ['invalid', 'error_language_tag_is_not_valid.kps'], CompilerMessages.ERROR_LanguageTagIsNotValid);
  });

  // WARN_LanguageTagIsNotMinimal

  it('should generate WARN_LanguageTagIsNotMinimal if keyboard has a non-minimal language tag', async function() {
    testForMessage(this, ['invalid', 'warn_language_tag_is_not_minimal.kps'], CompilerMessages.WARN_LanguageTagIsNotMinimal);
  });

  // ERROR_ModelMustHaveAtLeastOneLanguage

  it('should generate ERROR_MustHaveAtLeastOneLanguage if model has zero language tags', async function() {
    testForMessage(this, ['invalid', 'keyman.en.error_model_must_have_at_least_one_language.model.kps'],
      CompilerMessages.ERROR_ModelMustHaveAtLeastOneLanguage);
  });

  // WARN_RedistFileShouldNotBeInPackage

  it('should generate WARN_RedistFileShouldNotBeInPackage if package contains a redist file', async function() {
    testForMessage(this, ['invalid', 'warn_redist_file_should_not_be_in_package.kps'],
      CompilerMessages.WARN_RedistFileShouldNotBeInPackage);
  });

  // WARN_DocFileDangerous

  it('should generate WARN_DocFileDangerous if package contains a .doc file', async function() {
    testForMessage(this, ['invalid', 'warn_doc_file_dangerous.kps'],
      CompilerMessages.WARN_DocFileDangerous);
  });

  // ERROR_PackageMustContainAPackageOrAKeyboard

  it('should generate ERROR_PackageMustContainAModelOrAKeyboard if package contains a .doc file', async function() {
    testForMessage(this, ['invalid', 'error_package_must_contain_a_model_or_a_keyboard.kps'],
      CompilerMessages.ERROR_PackageMustContainAModelOrAKeyboard);
  });

  // WARN_JsKeyboardFileIsMissing

  it('should generate WARN_JsKeyboardFileIsMissing if package is missing corresponding .js for a touch .kmx', async function() {
    testForMessage(this, ['invalid', 'warn_js_keyboard_file_is_missing.kps'],
      CompilerMessages.WARN_JsKeyboardFileIsMissing);
  });

  // WARN_KeyboardShouldHaveAtLeastOneLanguage

  it('should generate WARN_KeyboardShouldHaveAtLeastOneLanguage if keyboard has zero language tags', async function() {
    testForMessage(this, ['invalid', 'warn_keyboard_should_have_at_least_one_language.kps'],
      CompilerMessages.WARN_KeyboardShouldHaveAtLeastOneLanguage);
  });

});
