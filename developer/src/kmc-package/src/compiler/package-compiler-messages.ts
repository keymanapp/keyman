/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def, CompilerMessageSpecWithException } from "@keymanapp/developer-utils";

const Namespace = CompilerErrorNamespace.PackageCompiler;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
const SevHint = CompilerErrorSeverity.Hint | Namespace;
const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

/**
 * @internal
 */
export class PackageCompilerMessages {
  static FATAL_UnexpectedException = SevFatal | 0x0001;
  static Fatal_UnexpectedException = (o:{e: any}) => CompilerMessageSpecWithException(this.FATAL_UnexpectedException, null, o.e ?? 'unknown error');

  static WARN_AbsolutePath = SevWarn | 0x0002;
  static Warn_AbsolutePath = (o:{filename: string}) => m(this.WARN_AbsolutePath, `File ${def(o.filename)} has an absolute path, which is not portable.`);

  static ERROR_FileDoesNotExist = SevError | 0x0003;
  static Error_FileDoesNotExist = (o:{filename: string}) => m(
    this.ERROR_FileDoesNotExist, `File '${def(o.filename)}' does not exist.`
  );

  static ERROR_FileCouldNotBeRead = SevError | 0x0004;
  static Error_FileCouldNotBeRead = (o:{filename: string; e: any}) => m(
    this.ERROR_FileCouldNotBeRead,
    `File '${def(o.filename)}' could not be read: ${(o.e ?? 'unknown error').toString()}.`
  );

  static WARN_FileIsNotABinaryKvkFile = SevWarn | 0x0005;
  static Warn_FileIsNotABinaryKvkFile = (o:{filename: string}) => m(this.WARN_FileIsNotABinaryKvkFile,
    `File '${def(o.filename)}' does not appear to be a valid binary .kvk file; this may be an old package that includes an xml-format .kvk file. `+
    `You must update the package to include the compiled .kvk file in the package.`
  );

  static ERROR_FollowKeyboardVersionNotAllowedForModelPackages = SevError | 0x0006;
  static Error_FollowKeyboardVersionNotAllowedForModelPackages = () => m(this.ERROR_FollowKeyboardVersionNotAllowedForModelPackages,
    `FollowKeyboardVersion is not allowed in model packages`);

  static ERROR_FollowKeyboardVersionButNoKeyboards = SevError | 0x0007;
  static Error_FollowKeyboardVersionButNoKeyboards = () => m(this.ERROR_FollowKeyboardVersionButNoKeyboards,
    `FollowKeyboardVersion is set, but the package contains no keyboards`);

  static ERROR_KeyboardContentFileNotFound = SevError | 0x0008;
  static Error_KeyboardContentFileNotFound = (o:{id:string}) => m(this.ERROR_KeyboardContentFileNotFound,
    `Keyboard '${def(o.id)}' was listed in <Keyboards> but a corresponding .kmx file was not found in <Files>`
  );

  static ERROR_KeyboardFileNotValid = SevError | 0x0009;
  static Error_KeyboardFileNotValid = (o:{filename:string, e:any}) => m(this.ERROR_KeyboardFileNotValid,
    `Keyboard file '${def(o.filename)}' is not a valid .kmx file: ${(o.e ?? 'unknown error').toString()}`
  );

  static INFO_KeyboardFileHasNoKeyboardVersion = SevInfo | 0x000A;
  static Info_KeyboardFileHasNoKeyboardVersion = (o:{filename:string}) => m(this.INFO_KeyboardFileHasNoKeyboardVersion,
    `Keyboard file '${def(o.filename)}' has no &KeyboardVersion store, using default '0.0'`
  );

  static ERROR_PackageCannotContainBothModelsAndKeyboards = SevError | 0x000B;
  static Error_PackageCannotContainBothModelsAndKeyboards = () => m(this.ERROR_PackageCannotContainBothModelsAndKeyboards,
    `The package contains both lexical models and keyboards, which is not permitted.`);

  static HINT_PackageShouldNotRepeatLanguages = SevHint | 0x000C;
  static Hint_PackageShouldNotRepeatLanguages = (o:{resourceType: string, id: string, minimalTag: string, firstTag: string, secondTag: string}) => m(this.HINT_PackageShouldNotRepeatLanguages,
    `Two language tags in ${def(o.resourceType)} ${def(o.id)}, '${def(o.firstTag)}' and '${def(o.secondTag)}', reduce to the same minimal tag '${def(o.minimalTag)}'.`);

  static WARN_PackageNameDoesNotFollowLexicalModelConventions = SevWarn | 0x000D;
  static Warn_PackageNameDoesNotFollowLexicalModelConventions = (o:{filename: string}) => m(this.WARN_PackageNameDoesNotFollowLexicalModelConventions,
    `The package file '${def(o.filename)}' does not follow the recommended model filename conventions. The name should be all lower case, `+
    `include only alphanumeric characters and underscore (_), not start with a digit, and should have the structure `+
    `<author>.<bcp47>.<uniq>.model.kps.`
  );

  static WARN_PackageNameDoesNotFollowKeyboardConventions = SevWarn | 0x000E;
  static Warn_PackageNameDoesNotFollowKeyboardConventions = (o:{filename: string}) => m(this.WARN_PackageNameDoesNotFollowKeyboardConventions,
    `The package file '${def(o.filename)}' does not follow the recommended keyboard filename conventions. The name should be all lower case, `+
    `include only alphanumeric characters and underscore (_), and not start with a digit.`
  );

  static WARN_FileInPackageDoesNotFollowFilenameConventions = SevWarn | 0x000F;
  static Warn_FileInPackageDoesNotFollowFilenameConventions = (o:{filename: string}) => m(this.WARN_FileInPackageDoesNotFollowFilenameConventions,
    `The file '${def(o.filename)}' does not follow the recommended filename conventions. The extension should be all lower case, `+
    `and the filename should include only alphanumeric characters, -, _, + and .`
  );

  static ERROR_PackageNameCannotBeBlank = SevError | 0x0010;
  static Error_PackageNameCannotBeBlank = () => m(this.ERROR_PackageNameCannotBeBlank,
    `Package name cannot be an empty string.`);

  static ERROR_KeyboardFileNotFound = SevError | 0x0011;
  static Error_KeyboardFileNotFound = (o:{filename:string}) => m(this.ERROR_KeyboardFileNotFound,
    `Keyboard file '${def(o.filename)}' was not found. Has it been compiled?`
  );

  static WARN_KeyboardVersionsDoNotMatch = SevWarn | 0x0012;
  static Warn_KeyboardVersionsDoNotMatch = (o: {keyboard:string, version:string, firstKeyboard:string, firstVersion:string}) => m(this.WARN_KeyboardVersionsDoNotMatch,
    `Keyboard ${def(o.keyboard)} version ${def(o.version)} does not match keyboard ${def(o.firstKeyboard)} version ${def(o.firstVersion)}.`);

  // 0x0013 was WARN_KeyboardVersionsDoNotMatchPackageVersion

  static ERROR_LanguageTagIsNotValid = SevError | 0x0014;
  static Error_LanguageTagIsNotValid = (o: {resourceType: string, id:string, lang:string, e:any}) => m(this.ERROR_LanguageTagIsNotValid,
    `Language tag '${def(o.lang)}' in ${def(o.resourceType)} ${def(o.id)} is invalid.`);

  static HINT_LanguageTagIsNotMinimal = SevHint | 0x0015;
  static Hint_LanguageTagIsNotMinimal = (o: {resourceType: string, id:string, actual:string, expected:string}) => m(this.HINT_LanguageTagIsNotMinimal,
    `Language tag '${def(o.actual)}' in ${def(o.resourceType)} ${def(o.id)} is not minimal, and should be '${def(o.expected)}'.`);

  static ERROR_ModelMustHaveAtLeastOneLanguage = SevError | 0x0016;
  static Error_ModelMustHaveAtLeastOneLanguage = (o:{id:string}) => m(this.ERROR_ModelMustHaveAtLeastOneLanguage,
    `The lexical model ${def(o.id)} must have at least one language specified.`);

  static WARN_RedistFileShouldNotBeInPackage = SevWarn | 0x0017;
  static Warn_RedistFileShouldNotBeInPackage = (o:{filename:string}) => m(this.WARN_RedistFileShouldNotBeInPackage,
    `The Keyman system file '${def(o.filename)}' should not be compiled into the package.`);

  static WARN_DocFileDangerous = SevWarn | 0x0018;
  static Warn_DocFileDangerous = (o:{filename:string}) => m(this.WARN_DocFileDangerous,
    `Microsoft Word .doc or .docx files ('${def(o.filename)}') are not portable. You should instead use HTML or PDF format.`);

  static ERROR_PackageMustContainAModelOrAKeyboard = SevError | 0x0019;
  static Error_PackageMustContainAModelOrAKeyboard = () => m(this.ERROR_PackageMustContainAModelOrAKeyboard,
    `Package must contain a lexical model or a keyboard.`);

  static WARN_JsKeyboardFileIsMissing = SevWarn | 0x001A;
  static Warn_JsKeyboardFileIsMissing = (o:{id: string}) => m(this.WARN_JsKeyboardFileIsMissing,
    `Keyboard ${def(o.id)} targets touch devices but corresponding ${def(o.id)}.js file is not in the package.`);

  static WARN_KeyboardShouldHaveAtLeastOneLanguage = SevWarn | 0x001B;
  static Warn_KeyboardShouldHaveAtLeastOneLanguage = (o:{id:string}) => m(this.WARN_KeyboardShouldHaveAtLeastOneLanguage,
    `The keyboard ${def(o.id)} should have at least one language specified.`);

  static HINT_JsKeyboardFileHasNoTouchTargets = SevHint | 0x001C;
  static Hint_JsKeyboardFileHasNoTouchTargets = (o:{id:string}) => m(this.HINT_JsKeyboardFileHasNoTouchTargets,
    `The keyboard ${def(o.id)} has been included for touch platforms, but does not include a touch layout.`);

  static HINT_PackageContainsSourceFile = SevHint | 0x001D;
  static Hint_PackageContainsSourceFile = (o:{filename:string}) => m(this.HINT_PackageContainsSourceFile,
    `The source file '${def(o.filename)}' should not be included in the package; instead include the compiled result.`
  );

  // 0x001E was ERROR_InvalidPackageFile, now DeveloperUtilsMessages.Error_InvalidPackageFile

  static ERROR_FileRecordIsMissingName = SevError | 0x001F;
  static Error_FileRecordIsMissingName = (o:{description:string}) => m(this.ERROR_FileRecordIsMissingName,
    `File record in the package with description '${o.description}' is missing a filename.`);

  static ERROR_InvalidAuthorEmail = SevError | 0x0020;
  static Error_InvalidAuthorEmail = (o:{email:string}) => m(this.ERROR_InvalidAuthorEmail,
    `Invalid author email: ${def(o.email)}`);

  static ERROR_PackageFileHasEmptyVersion = SevError | 0x0021;
  static Error_PackageFileHasEmptyVersion = () => m(
    this.ERROR_PackageFileHasEmptyVersion,
    `Package version is not following keyboard version, but the package version field is blank.`
  );

  static ERROR_RequiredParameterMissing = SevError | 0x0022;
  static Error_RequiredParameterMissing = (o:{param: string}) => m(
    this.ERROR_RequiredParameterMissing, `Source parameter '${def(o.param)}' is required.`
  );

  static WARN_CannotFindMatchingKeyboardForVisualKeyboard = SevWarn | 0x0023;
  static Warn_CannotFindMatchingKeyboardForVisualKeyboard = (o:{filename: string}) => m(
    this.WARN_CannotFindMatchingKeyboardForVisualKeyboard, `Cannot find matching keyboard for visual keyboard file '${def(o.filename)}'.`
  );

  static WARN_CannotFindFontForKeyboard = SevWarn | 0x0024;
  static Warn_CannotFindFontForKeyboard = (o:{id: string, fontFilename: string}) => m(
    this.WARN_CannotFindFontForKeyboard, `Cannot find font file '${def(o.fontFilename)}' for keyboard '${def(o.id)}'.`
  );

  static WARN_CannotReadFont = SevWarn | 0x0025;
  static Warn_CannotReadFont = (o:{fontFilename: string}) => m(
    this.WARN_CannotReadFont, `Cannot parse font file '${def(o.fontFilename)}' to get facename.`
  );

  static WARN_VisualKeyboardFileIsInvalid = SevWarn | 0x0026;
  static Warn_VisualKeyboardFileIsInvalid = (o:{filename: string}) => m(
    this.WARN_VisualKeyboardFileIsInvalid, `Visual keyboard file '${def(o.filename)}' is invalid.`
  );

  static WARN_PackageVersionIsUnrecognizedFormat = SevWarn | 0x0027;
  static Warn_PackageVersionIsUnrecognizedFormat = (o:{version: string}) => m(
    this.WARN_PackageVersionIsUnrecognizedFormat, `Package version '${def(o.version)}' has an unrecognized format.`,
    `The format for version numbers should be number[.number[.number]]. Each
    number component should be an integer, without leading zeroes.`
  );

  static ERROR_PackageMustNotContainItself = SevError | 0x0028;
  static Error_PackageMustNotContainItself = (o:{outputFilename: string}) => m(
    this.ERROR_PackageMustNotContainItself, `The package may not include a .kmp file of the same name '${def(o.outputFilename)}'.`, `
    While it is possible for a package file to contain other .kmp package files,
    it is an error for a package file to contain a package with the same name as
    itself.

    **Note:** it is not recommended to include other package files within a
    package, as the user experience for installation and uninstallation is
    complex, and not consistent across all platforms.

    **Note**: Nested packages are not checked for validity or whether or not they
    may violate this rule transitively.
  `);

  //------------------------------------------------------------------------------|
  // max length of detail message lines (checked by verifyCompilerMessagesObject) |
  //------------------------------------------------------------------------------|
}

