import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageSpecWithException } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.KeyboardInfoCompiler;
// const SevInfo = CompilerErrorSeverity.Info | Namespace;
const SevHint = CompilerErrorSeverity.Hint | Namespace;
const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

export class KeyboardInfoCompilerMessages {
  static Fatal_UnexpectedException = (o:{e: any}) => CompilerMessageSpecWithException(this.FATAL_UnexpectedException, null, o.e ?? 'unknown error');
  static FATAL_UnexpectedException = SevFatal | 0x0001;

  static Error_FileDoesNotExist = (o:{filename: string}) => m(this.ERROR_FileDoesNotExist, `File ${o.filename} does not exist.`);
  static ERROR_FileDoesNotExist = SevError | 0x0002;

  static Error_FileIsNotValid = (o:{filename: string; e: any}) => m(this.ERROR_FileIsNotValid,
    `File ${o.filename} could not be parsed: ${(o.e ?? 'unknown error').toString()}.`);
  static ERROR_FileIsNotValid = SevError | 0x0003;

  static Warn_MetadataFieldInconsistent = (o:{field:string, value:any, expected:any}) => m(this.WARN_MetadataFieldInconsistent,
    `Warning: field ${o.field} value "${o.value}" does not match "${o.expected}" found in source file metadata.`);
  static WARN_MetadataFieldInconsistent = SevWarn | 0x0004;

  static Error_InvalidAuthorEmail = (o:{email:string}) => m(this.ERROR_InvalidAuthorEmail,
    `Invalid author email: ${o.email}`);
  static ERROR_InvalidAuthorEmail = SevError | 0x0005;

  static Error_LicenseFileDoesNotExist = (o:{filename:string}) => m(this.ERROR_LicenseFileIsMissing,
    `License file ${o.filename} does not exist.`);
  static ERROR_LicenseFileIsMissing = SevError | 0x0006;

  static Error_LicenseFileIsDamaged = (o:{filename:string}) => m(this.ERROR_LicenseFileIsDamaged,
    `License file ${o.filename} could not be loaded or decoded.`);
  static ERROR_LicenseFileIsDamaged = SevError | 0x0007;

  static Error_LicenseIsNotValid = (o:{filename:string,message:string}) => m(this.ERROR_LicenseIsNotValid,
    `An error was encountered parsing license file ${o.filename}: ${o.message}.`);
  static ERROR_LicenseIsNotValid = SevError | 0x0008;

  static Error_CannotBuildWithoutKmpFile = () => m(this.ERROR_CannotBuildWithoutKmpFile,
    `Compiling the .keyboard_info file requires a .kmp file for metadata.`);
  static ERROR_CannotBuildWithoutKmpFile = SevError | 0x0009;

  static Error_NoLicenseFound = () => m(this.ERROR_NoLicenseFound,
    `No license for the keyboard was found. MIT license is required for publication to Keyman keyboards repository.`);
  static ERROR_NoLicenseFound = SevError | 0x000A;

  static Hint_OutputValidation = (o:{message: any}) => m(this.HINT_OutputValidation,
    `Validating output: ${o.message}.`);
  static HINT_OutputValidation = SevHint | 0x000B;

  static Warn_OutputValidation = (o:{message: any}) => m(this.WARN_OutputValidation,
    `Validating output: ${o.message}.`);
  static WARN_OutputValidation = SevWarn | 0x000C;

  static Error_OutputValidation = (o:{message: any}) => m(this.ERROR_OutputValidation,
    `Validating output: ${o.message}.`);
  static ERROR_OutputValidation = SevError | 0x000D;

  static Error_FontFileCannotBeRead = (o:{filename: string}) => m(this.ERROR_FontFileCannotBeRead,
    `Font ${o.filename} could not be parsed to extract a font family.`);
  static ERROR_FontFileCannotBeRead = SevError | 0x000E;
}

