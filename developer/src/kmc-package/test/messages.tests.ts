/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { PackageCompilerMessages } from '../src/compiler/package-compiler-messages.js';
import { makePathToFixture } from './helpers/index.js';
import { KmpCompiler } from '../src/compiler/kmp-compiler.js';
import { CompilerErrorNamespace, CompilerOptions } from '@keymanapp/developer-utils';

const callbacks = new TestCompilerCallbacks();

describe('PackageCompilerMessages', function () {

  this.afterEach(function() {
    if(this.currentTest.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('should have a valid PackageCompilerMessages object', function() {
    return verifyCompilerMessagesObject(PackageCompilerMessages, CompilerErrorNamespace.PackageCompiler);
  });


  //
  // Message tests
  //

  async function testForMessage(context: Mocha.Context, fixture: string[], messageId?: number, options?: CompilerOptions) {
    context.timeout(10000);

    callbacks.clear();

    const kpsPath = makePathToFixture(...fixture);
    const kmpCompiler = new KmpCompiler();
    assert.isTrue(await kmpCompiler.init(callbacks, options ?? {}));

    await kmpCompiler.run(kpsPath);

    if(messageId) {
      assert.lengthOf(callbacks.messages, 1);
      assert.isTrue(callbacks.hasMessage(messageId));
    } else {
      assert.lengthOf(callbacks.messages, 0);
    }
  }

  // WARN_FileIsNotABinaryKvkFile

  it('should generate WARN_FileIsNotABinaryKvkFile if a non-binary kvk file is included', async function() {
    await testForMessage(this, ['xml_kvk_file', 'source', 'xml_kvk_file.kps'], PackageCompilerMessages.WARN_FileIsNotABinaryKvkFile);
  });

  it('should not warn if a binary kvk file is included', async function() {
    await testForMessage(this, ['binary_kvk_file', 'source', 'binary_kvk_file.kps']);
  });

  // ERROR_FollowKeyboardVersionNotAllowedForModelPackages

  it('should generate ERROR_FollowKeyboardVersionNotAllowedForModelPackages if <FollowKeyboardVersion> is set for model packages', async function() {
    await testForMessage(this, ['invalid', 'followkeyboardversion.qaa.sencoten.model.kps'], PackageCompilerMessages.ERROR_FollowKeyboardVersionNotAllowedForModelPackages);
  });

  // ERROR_FollowKeyboardVersionButNoKeyboards

  it('should generate ERROR_FollowKeyboardVersionButNoKeyboards if <FollowKeyboardVersion> is set for a package with no keyboards', async function() {
    await testForMessage(this, ['invalid', 'followkeyboardversion.empty.kps'], PackageCompilerMessages.ERROR_FollowKeyboardVersionButNoKeyboards);
  });

  // ERROR_KeyboardContentFileNotFound

  it('should generate ERROR_KeyboardContentFileNotFound if a <Keyboard> is listed in a package but not found in <Files>', async function() {
    await testForMessage(this, ['invalid', 'keyboardcontentfilenotfound.kps'], PackageCompilerMessages.ERROR_KeyboardContentFileNotFound);
  });

  // ERROR_KeyboardFileNotValid

  it('should generate ERROR_KeyboardFileNotValid if a .kmx is not valid in <Files>', async function() {
    await testForMessage(this, ['invalid', 'keyboardfilenotvalid.kps'], PackageCompilerMessages.ERROR_KeyboardFileNotValid);
  });

  // INFO_KeyboardFileHasNoKeyboardVersion

  it('should generate INFO_KeyboardFileHasNoKeyboardVersion if <FollowKeyboardVersion> is set but keyboard has no version', async function() {
    await testForMessage(this, ['invalid', 'nokeyboardversion.kps'], PackageCompilerMessages.INFO_KeyboardFileHasNoKeyboardVersion);
  });

  // ERROR_PackageCannotContainBothModelsAndKeyboards

  it('should generate ERROR_PackageCannotContainBothModelsAndKeyboards if package has both keyboards and models', async function() {
    await testForMessage(this, ['invalid', 'error_package_cannot_contain_both_models_and_keyboards.kps'], PackageCompilerMessages.ERROR_PackageCannotContainBothModelsAndKeyboards);
  });

  // HINT_PackageShouldNotRepeatLanguages (models)

  it('should generate HINT_PackageShouldNotRepeatLanguages if model has same language repeated', async function() {
    await testForMessage(this, ['invalid', 'keyman.en.hint_package_should_not_repeat_languages.model.kps'], PackageCompilerMessages.HINT_PackageShouldNotRepeatLanguages);
  });

  // HINT_PackageShouldNotRepeatLanguages (keyboards)

  it('should generate HINT_PackageShouldNotRepeatLanguages if keyboard has same language repeated', async function() {
    await testForMessage(this, ['invalid', 'hint_package_should_not_repeat_languages.kps'], PackageCompilerMessages.HINT_PackageShouldNotRepeatLanguages);
  });

  // WARN_PackageNameDoesNotFollowLexicalModelConventions

  it('should generate WARN_PackageNameDoesNotFollowLexicalModelConventions if filename has wrong conventions', async function() {
    await testForMessage(this, ['invalid', 'WARN_PackageNameDoesNotFollowLexicalModelConventions.kps'], PackageCompilerMessages.WARN_PackageNameDoesNotFollowLexicalModelConventions);
  });

  // WARN_PackageNameDoesNotFollowKeyboardConventions

  it('should generate WARN_PackageNameDoesNotFollowKeyboardConventions if filename has wrong conventions', async function() {
    await testForMessage(this, ['invalid', 'WARN_PackageNameDoesNotFollowKeyboardConventions.kps'], PackageCompilerMessages.WARN_PackageNameDoesNotFollowKeyboardConventions);
  });

  // WARN_FileInPackageDoesNotFollowFilenameConventions

  it('should generate WARN_FileInPackageDoesNotFollowFilenameConventions if content filename has wrong conventions', async function() {
    await testForMessage(this, ['invalid', 'warn_file_in_package_does_not_follow_filename_conventions.kps'],
      PackageCompilerMessages.WARN_FileInPackageDoesNotFollowFilenameConventions, {checkFilenameConventions: true});
    await testForMessage(this, ['invalid', 'warn_file_in_package_does_not_follow_filename_conventions_2.kps'],
      PackageCompilerMessages.WARN_FileInPackageDoesNotFollowFilenameConventions, {checkFilenameConventions: true});
  });

  // Test the inverse -- no warning generated if checkFilenameConventions is false

  it('should not generate WARN_FileInPackageDoesNotFollowFilenameConventions if content filename has wrong conventions but checkFilenameConventions is false', async function() {
    await testForMessage(this, ['invalid', 'warn_file_in_package_does_not_follow_filename_conventions.kps'], null, {checkFilenameConventions: false});
    await testForMessage(this, ['invalid', 'warn_file_in_package_does_not_follow_filename_conventions_2.kps'], null, {checkFilenameConventions: false});
  });

  // ERROR_PackageNameCannotBeBlank

  it('should generate ERROR_PackageNameCannotBeBlank if package info has empty name', async function() {
    await testForMessage(this, ['invalid', 'error_package_name_cannot_be_blank.kps'], PackageCompilerMessages.ERROR_PackageNameCannotBeBlank); // blank field
    await testForMessage(this, ['invalid', 'error_package_name_cannot_be_blank_2.kps'], PackageCompilerMessages.ERROR_PackageNameCannotBeBlank); // missing field
  });

  // ERROR_KeyboardFileNotFound

  it('should generate ERROR_KeyboardFileNotFound if a <Keyboard> is listed in a package but not found in <Files>', async function() {
    await testForMessage(this, ['invalid', 'keyboardfilenotfound.kps'], PackageCompilerMessages.ERROR_KeyboardFileNotFound);
  });

  // WARN_KeyboardVersionsDoNotMatch

  it('should generate WARN_KeyboardVersionsDoNotMatch if two <Keyboards> have different versions', async function() {
    await testForMessage(this, ['invalid', 'warn_keyboard_versions_do_not_match.kps'], PackageCompilerMessages.WARN_KeyboardVersionsDoNotMatch);
  });

  // ERROR_LanguageTagIsNotValid

  it('should generate ERROR_LanguageTagIsNotValid if keyboard has an invalid language tag', async function() {
    testForMessage(this, ['invalid', 'error_language_tag_is_not_valid.kps'], PackageCompilerMessages.ERROR_LanguageTagIsNotValid);
  });

  // HINT_LanguageTagIsNotMinimal

  it('should generate HINT_LanguageTagIsNotMinimal if keyboard has a non-minimal language tag', async function() {
    await testForMessage(this, ['invalid', 'hint_language_tag_is_not_minimal.kps'], PackageCompilerMessages.HINT_LanguageTagIsNotMinimal);
  });

  // ERROR_ModelMustHaveAtLeastOneLanguage

  it('should generate ERROR_MustHaveAtLeastOneLanguage if model has zero language tags', async function() {
    await testForMessage(this, ['invalid', 'keyman.en.error_model_must_have_at_least_one_language.model.kps'],
      PackageCompilerMessages.ERROR_ModelMustHaveAtLeastOneLanguage);
  });

  // WARN_RedistFileShouldNotBeInPackage

  it('should generate WARN_RedistFileShouldNotBeInPackage if package contains a redist file', async function() {
    await testForMessage(this, ['invalid', 'warn_redist_file_should_not_be_in_package.kps'],
      PackageCompilerMessages.WARN_RedistFileShouldNotBeInPackage);
  });

  // WARN_DocFileDangerous

  it('should generate WARN_DocFileDangerous if package contains a .doc file', async function() {
    await testForMessage(this, ['invalid', 'warn_doc_file_dangerous.kps'],
      PackageCompilerMessages.WARN_DocFileDangerous);
  });

  // ERROR_PackageMustContainAPackageOrAKeyboard

  it('should generate ERROR_PackageMustContainAModelOrAKeyboard if package contains no keyboard or model', async function() {
    await testForMessage(this, ['invalid', 'error_package_must_contain_a_model_or_a_keyboard.kps'],
      PackageCompilerMessages.ERROR_PackageMustContainAModelOrAKeyboard);
  });

  // WARN_JsKeyboardFileIsMissing

  it('should generate WARN_JsKeyboardFileIsMissing if package is missing corresponding .js for a touch .kmx', async function() {
    await testForMessage(this, ['invalid', 'warn_js_keyboard_file_is_missing.kps'],
      PackageCompilerMessages.WARN_JsKeyboardFileIsMissing);
  });

  // WARN_KeyboardShouldHaveAtLeastOneLanguage

  it('should generate WARN_KeyboardShouldHaveAtLeastOneLanguage if keyboard has zero language tags', async function() {
    await testForMessage(this, ['invalid', 'warn_keyboard_should_have_at_least_one_language.kps'],
      PackageCompilerMessages.WARN_KeyboardShouldHaveAtLeastOneLanguage);
  });

  // HINT_JsKeyboardFileHasNoTouchTargets

  it('should generate HINT_JsKeyboardFileHasNoTouchTargets if keyboard has no touch targets', async function() {
    await testForMessage(this, ['invalid', 'hint_js_keyboard_file_has_no_touch_targets.kps'],
      PackageCompilerMessages.HINT_JsKeyboardFileHasNoTouchTargets);
  });

  it('should not generate HINT_JsKeyboardFileHasNoTouchTargets if keyboard has a touch target', async function() {
    await testForMessage(this, ['khmer_angkor', 'source', 'khmer_angkor.kps'], null);
  });

  // HINT_PackageContainsSourceFile

  it('should generate HINT_PackageContainsSourceFile if package contains a source file', async function() {
    await testForMessage(this, ['invalid', 'hint_source_file_should_not_be_in_package.kps'],
      PackageCompilerMessages.HINT_PackageContainsSourceFile);
  });

  // ERROR_InvalidAuthorEmail

  it('should generate ERROR_InvalidAuthorEmail if author email address has multiple addresses', async function() {
    await testForMessage(this, ['invalid', 'error_invalid_author_email_multiple.kps'],
      PackageCompilerMessages.ERROR_InvalidAuthorEmail);
  });

  it('should generate ERROR_InvalidAuthorEmail if author email address is formatted incorrectly', async function() {
    await testForMessage(this, ['invalid', 'error_invalid_author_email.kps'],
      PackageCompilerMessages.ERROR_InvalidAuthorEmail);
  });

  it('should generate ERROR_PackageFileHasEmptyVersion if FollowKeyboardVersion is not present and Version is empty', async function() {
    await testForMessage(this, ['invalid', 'error_package_file_has_empty_version.kps'],
      PackageCompilerMessages.ERROR_PackageFileHasEmptyVersion);
  });

  it('should generate Warn_PackageVersionIsUnrecognizedFormat if package version field is not a valid format', async function() {
    await testForMessage(this, ['invalid', 'warn_package_version_is_unrecognized_format.kps'],
      PackageCompilerMessages.WARN_PackageVersionIsUnrecognizedFormat);
  });

  // ERROR_PackageMustNotContainItself test in package-compiler.tests.ts
});
