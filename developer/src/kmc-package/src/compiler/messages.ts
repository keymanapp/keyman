import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.PackageCompiler;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

export class CompilerMessages {
  static Fatal_UnexpectedException = (o:{e: any}) => m(this.FATAL_UnexpectedException,
    `Unexpected exception: ${(o.e ?? 'unknown error').toString()}\n\nCall stack:\n${(o.e instanceof Error ? o.e.stack : (new Error()).stack)}`);
  static FATAL_UnexpectedException = SevFatal | 0x0001;

  static Warn_AbsolutePath = (o:{filename: string}) => m(this.WARN_AbsolutePath, `File ${o.filename} has an absolute path, which is not portable.`);
  static WARN_AbsolutePath = SevWarn | 0x0002;

  static Error_FileDoesNotExist = (o:{filename: string}) => m(this.ERROR_FileDoesNotExist, `File ${o.filename} does not exist.`);
  static ERROR_FileDoesNotExist = SevError | 0x0003;

  static Error_FileCouldNotBeRead = (o:{filename: string; e: any}) => m(this.ERROR_FileCouldNotBeRead,
    `File ${o.filename} could not be read: ${(o.e ?? 'unknown error').toString()}.`);
  static ERROR_FileCouldNotBeRead = SevError | 0x0004;

  static Warn_FileIsNotABinaryKvkFile = (o:{filename: string}) => m(this.WARN_FileIsNotABinaryKvkFile,
    `File ${o.filename} does not appear to be a valid binary .kvk file; this may be an old package that includes an xml-format .kvk file. `+
    `You must update the package to include the compiled .kvk file in the package.`);
  static WARN_FileIsNotABinaryKvkFile = SevWarn | 0x0005;

  static Error_FollowKeyboardVersionNotAllowedForModelPackages = () => m(this.ERROR_FollowKeyboardVersionNotAllowedForModelPackages,
    `FollowKeyboardVersion is not allowed in model packages`);
  static ERROR_FollowKeyboardVersionNotAllowedForModelPackages = SevError | 0x0006;

  static Error_FollowKeyboardVersionButNoKeyboards = () => m(this.ERROR_FollowKeyboardVersionButNoKeyboards,
    `FollowKeyboardVersion is set, but the package contains no keyboards`);
  static ERROR_FollowKeyboardVersionButNoKeyboards = SevError | 0x0007;

  static Error_KeyboardContentFileNotFound = (o:{id:string}) => m(this.ERROR_KeyboardContentFileNotFound,
    `Keyboard ${o.id} was listed in <Keyboards> but a corresponding .kmx file was not found in <Files>`);
  static ERROR_KeyboardContentFileNotFound = SevError | 0x0008;

  static Error_KeyboardFileNotValid = (o:{filename:string, e:any}) => m(this.ERROR_KeyboardFileNotValid,
    `Keyboard file ${o.filename} is not a valid .kmx file: ${(o.e ?? 'unknown error').toString()}`);
  static ERROR_KeyboardFileNotValid = SevError | 0x0009;

  static Info_KeyboardFileHasNoKeyboardVersion = (o:{filename:string}) => m(this.INFO_KeyboardFileHasNoKeyboardVersion,
    `Keyboard file ${o.filename} has no &KeyboardVersion store, using default '0.0'`);
  static INFO_KeyboardFileHasNoKeyboardVersion = SevInfo | 0x000A;

  static Error_PackageCannotContainBothModelsAndKeyboards = () => m(this.ERROR_PackageCannotContainBothModelsAndKeyboards,
    `The package contains both lexical models and keyboards, which is not permitted.`);
  static ERROR_PackageCannotContainBothModelsAndKeyboards = SevError | 0x000B;

  static Warn_PackageShouldNotRepeatLanguages = (o:{resourceType: string, id: string, minimalTag: string, firstTag: string, secondTag: string}) => m(this.WARN_PackageShouldNotRepeatLanguages,
    `Two language tags in ${o.resourceType} ${o.id}, '${o.firstTag}' and '${o.secondTag}', reduce to the same minimal tag '${o.minimalTag}'.`);
  static WARN_PackageShouldNotRepeatLanguages = SevWarn | 0x000C;

  static Warn_PackageNameDoesNotFollowLexicalModelConventions = (o:{filename: string}) => m(this.WARN_PackageNameDoesNotFollowLexicalModelConventions,
    `The package file ${o.filename} does not follow the recommended model filename conventions. The name should be all lower case, `+
    `include only alphanumeric characters and underscore (_), not start with a digit, and should have the structure `+
    `<author>.<bcp47>.<uniq>.model.kps.`);
  static WARN_PackageNameDoesNotFollowLexicalModelConventions = SevWarn | 0x000D;

  static Warn_PackageNameDoesNotFollowKeyboardConventions = (o:{filename: string}) => m(this.WARN_PackageNameDoesNotFollowKeyboardConventions,
    `The package file ${o.filename} does not follow the recommended keyboard filename conventions. The name should be all lower case, `+
    `include only alphanumeric characters and underscore (_), and not start with a digit.`);
  static WARN_PackageNameDoesNotFollowKeyboardConventions = SevWarn | 0x000E;

  static Warn_FileInPackageDoesNotFollowFilenameConventions = (o:{filename: string}) => m(this.WARN_FileInPackageDoesNotFollowFilenameConventions,
    `The file ${o.filename} does not follow the recommended filename conventions. The extension should be all lower case, `+
    `and the filename should include only alphanumeric characters, -, _, + and .`);
  static WARN_FileInPackageDoesNotFollowFilenameConventions = SevWarn | 0x000F;

  static Error_PackageNameCannotBeBlank = () => m(this.ERROR_PackageNameCannotBeBlank,
    `Package name cannot be an empty string.`);
  static ERROR_PackageNameCannotBeBlank = SevError | 0x0010;

  static Error_KeyboardFileNotFound = (o:{filename:string}) => m(this.ERROR_KeyboardFileNotFound,
    `Keyboard file ${o.filename} was not found. Has it been compiled?`);
  static ERROR_KeyboardFileNotFound = SevError | 0x0011;

  static Warn_KeyboardVersionsDoNotMatch = (o: {keyboard:string, version:string, firstKeyboard:string, firstVersion:string}) => m(this.WARN_KeyboardVersionsDoNotMatch,
    `Keyboard ${o.keyboard} version ${o.version} does not match keyboard ${o.firstKeyboard} version ${o.firstVersion}.`);
  static WARN_KeyboardVersionsDoNotMatch = SevWarn | 0x0012;

  // 0x0013 was WARN_KeyboardVersionsDoNotMatchPackageVersion

  static Error_LanguageTagIsNotValid = (o: {resourceType: string, id:string, lang:string, e:any}) => m(this.ERROR_LanguageTagIsNotValid,
    `Language tag '${o.lang}' in ${o.resourceType} ${o.id} is invalid.`);
  static ERROR_LanguageTagIsNotValid = SevError | 0x0014;

  static Warn_LanguageTagIsNotMinimal = (o: {resourceType: string, id:string, actual:string, expected:string}) => m(this.WARN_LanguageTagIsNotMinimal,
    `Language tag '${o.actual}' in ${o.resourceType} ${o.id} is not minimal, and should be '${o.expected}'.`);
  static WARN_LanguageTagIsNotMinimal = SevWarn | 0x0015;

  static Error_ModelMustHaveAtLeastOneLanguage = (o:{id:string}) => m(this.ERROR_ModelMustHaveAtLeastOneLanguage,
    `The lexical model ${o.id} must have at least one language specified.`);
  static ERROR_ModelMustHaveAtLeastOneLanguage = SevError | 0x0016;

  static Warn_RedistFileShouldNotBeInPackage = (o:{filename:string}) => m(this.WARN_RedistFileShouldNotBeInPackage,
    `The Keyman system file '${o.filename}' should not be compiled into the package.`);
  static WARN_RedistFileShouldNotBeInPackage = SevWarn | 0x0017;

  static Warn_DocFileDangerous = (o:{filename:string}) => m(this.WARN_DocFileDangerous,
    `Microsoft Word .doc or .docx files ('${o.filename}') are not portable. You should instead use HTML or PDF format.`);
  static WARN_DocFileDangerous = SevWarn | 0x0018;

  static Error_PackageMustContainAModelOrAKeyboard = () => m(this.ERROR_PackageMustContainAModelOrAKeyboard,
    `Package must contain a lexical model or a keyboard.`);
  static ERROR_PackageMustContainAModelOrAKeyboard = SevError | 0x0019;

  static Warn_JsKeyboardFileIsMissing = (o:{id: string}) => m(this.WARN_JsKeyboardFileIsMissing,
    `Keyboard ${o.id} targets touch devices but corresponding ${o.id}.js file is not in the package.`);
  static WARN_JsKeyboardFileIsMissing = SevWarn | 0x001A;

  static Warn_KeyboardShouldHaveAtLeastOneLanguage = (o:{id:string}) => m(this.WARN_KeyboardShouldHaveAtLeastOneLanguage,
    `The keyboard ${o.id} should have at least one language specified.`);
  static WARN_KeyboardShouldHaveAtLeastOneLanguage = SevWarn | 0x001B;

}

