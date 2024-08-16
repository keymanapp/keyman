import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def, CompilerMessageSpecWithException } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.KeyboardInfoCompiler;
// const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

/**
 * @internal
 */
export class KeyboardInfoCompilerMessages {
  static FATAL_UnexpectedException = SevFatal | 0x0001;
  static Fatal_UnexpectedException = (o:{e: any}) => CompilerMessageSpecWithException(this.FATAL_UnexpectedException, null, o.e ?? 'unknown error');

  static ERROR_FileDoesNotExist = SevError | 0x0002;
  static Error_FileDoesNotExist = (o:{filename: string}) => m(this.ERROR_FileDoesNotExist, `File ${def(o.filename)} does not exist.`);

  // 0x0003 unused, available for future messages
  // 0x0004 unused, available for future messages

  static ERROR_InvalidAuthorEmail = SevError | 0x0005;
  static Error_InvalidAuthorEmail = (o:{email:string}) => m(this.ERROR_InvalidAuthorEmail,
    `Invalid author email: ${def(o.email)}`);

  static ERROR_LicenseFileIsMissing = SevError | 0x0006;
  static Error_LicenseFileIsMissing = (o:{filename:string}) => m(this.ERROR_LicenseFileIsMissing,
    `License file ${def(o.filename)} does not exist.`);

  static ERROR_LicenseFileIsDamaged = SevError | 0x0007;
  static Error_LicenseFileIsDamaged = (o:{filename:string}) => m(this.ERROR_LicenseFileIsDamaged,
    `License file ${def(o.filename)} could not be loaded or decoded.`);

  static ERROR_LicenseIsNotValid = SevError | 0x0008;
  static Error_LicenseIsNotValid = (o:{filename:string,message:string}) => m(this.ERROR_LicenseIsNotValid,
    `An error was encountered parsing license file ${def(o.filename)}: ${def(o.message)}.`);

  static ERROR_CannotBuildWithoutKmpFile = SevError | 0x0009;
  static Error_CannotBuildWithoutKmpFile = () => m(this.ERROR_CannotBuildWithoutKmpFile,
    `Compiling the .keyboard_info file requires a .kmp file for metadata.`);

  static ERROR_NoLicenseFound = SevError | 0x000A;
  static Error_NoLicenseFound = () => m(this.ERROR_NoLicenseFound,
    `No license for the keyboard was found. MIT license is required for publication to Keyman keyboards repository.`);

  // 0x000B unused, available for future messages
  // 0x000C unused, available for future messages
  // 0x000D unused, available for future messages

  static ERROR_FontFileCannotBeRead = SevError | 0x000E;
  static Error_FontFileCannotBeRead = (o:{filename: string}) => m(this.ERROR_FontFileCannotBeRead,
    `Font ${def(o.filename)} could not be parsed to extract a font family.`);

  static ERROR_FontFileMetaDataIsInvalid = SevError | 0x000F;
  static Error_FontFileMetaDataIsInvalid = (o:{filename: string,message:string}) => m(
    this.ERROR_FontFileMetaDataIsInvalid,
    `Font ${def(o.filename)} meta data invalid: ${def(o.message)}.`);

  static ERROR_DescriptionIsMissing = SevError | 0x0010;
  static Error_DescriptionIsMissing = (o:{filename:string}) => m(
    this.ERROR_DescriptionIsMissing,
    `The Info.Description field in the package ${def(o.filename)} is required, but is missing or empty.`);
}

