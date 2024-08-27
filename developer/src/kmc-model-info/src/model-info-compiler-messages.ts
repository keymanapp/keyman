import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def, CompilerMessageSpecWithException } from "@keymanapp/developer-utils";

const Namespace = CompilerErrorNamespace.ModelInfoCompiler;
// const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

/**
 * @internal
 */
export class ModelInfoCompilerMessages {
  static FATAL_UnexpectedException = SevFatal | 0x0001;
  static Fatal_UnexpectedException = (o:{e: any}) => CompilerMessageSpecWithException(this.FATAL_UnexpectedException, null, o.e ?? 'unknown error');

  static ERROR_FileDoesNotExist = SevError | 0x0002;
  static Error_FileDoesNotExist = (o:{filename: string}) => m(this.ERROR_FileDoesNotExist, `File ${def(o.filename)} does not exist.`);

  static ERROR_FileIsNotValid = SevError | 0x0003;
  static Error_FileIsNotValid = (o:{filename: string; e: any}) => m(this.ERROR_FileIsNotValid,
    `File ${def(o.filename)} could not be parsed: ${(o.e ?? 'unknown error').toString()}.`);

  static WARN_MetadataFieldInconsistent = SevWarn | 0x0004;
  static Warn_MetadataFieldInconsistent = (o:{field:string, value:any, expected:any}) => m(this.WARN_MetadataFieldInconsistent,
    `Warning: field ${def(o.field)} value "${def(o.value)}" does not match "${def(o.expected)}" found in source file metadata.`);

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

  static ERROR_NoLicenseFound = SevError | 0x0009;
  static Error_NoLicenseFound = () => m(this.ERROR_NoLicenseFound,
    `No license for the model was found. MIT license is required for publication to Keyman lexical-models repository.`);

  static ERROR_DescriptionIsMissing = SevError | 0x000A;
  static Error_DescriptionIsMissing = (o:{filename:string}) => m(
    this.ERROR_DescriptionIsMissing,
    `The Info.Description field in the package ${def(o.filename)} is required, but is missing or empty.`);
}

