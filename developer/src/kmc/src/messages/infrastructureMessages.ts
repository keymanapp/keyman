import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.Infrastructure;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

export class InfrastructureMessages {
  static Fatal_UnexpectedException = (o:{e: any}) => m(this.FATAL_UnexpectedException, null, o.e ?? 'unknown error');
  static FATAL_UnexpectedException = SevFatal | 0x0001;

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static Info_BuildingFile = (o:{filename:string}) => ({filename:o.filename, ...m(this.INFO_BuildingFile,
    `Building ${o.filename}`)});
  static INFO_BuildingFile = SevInfo | 0x0002;

  static Error_FileDoesNotExist = (o:{filename:string}) => m(this.ERROR_FileDoesNotExist,
    `File ${o.filename} does not exist`);
  static ERROR_FileDoesNotExist = SevError | 0x0003;

  static Error_FileTypeNotRecognized = (o:{filename: string, extensions: string}) => m(this.ERROR_FileTypeNotRecognized,
    `Unrecognised input file ${o.filename}, expecting ${o.extensions}, or project folder`);
  static ERROR_FileTypeNotRecognized = SevError | 0x0004;

  static Error_OutFileNotValidForProjects = () => m(this.ERROR_OutFileNotValidForProjects,
    `--out-file should not be specified for project builds`);
  static ERROR_OutFileNotValidForProjects = SevError | 0x0005;

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static Info_FileBuiltSuccessfully = (o:{filename:string}) => ({filename:o.filename, ...m(this.INFO_FileBuiltSuccessfully,
    `${o.filename} built successfully.`)});
  static INFO_FileBuiltSuccessfully = SevInfo | 0x0006;

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static Info_FileNotBuiltSuccessfully = (o:{filename:string}) => ({filename:o.filename, ...m(this.INFO_FileNotBuiltSuccessfully,
    `${o.filename} failed to build.`)});
  static INFO_FileNotBuiltSuccessfully = SevInfo | 0x0007;

  static Error_InvalidProjectFile = (o:{message:string}) => m(this.ERROR_InvalidProjectFile,
    `Project file is not valid: ${o.message}`);
  static ERROR_InvalidProjectFile = SevError | 0x0008;

  static Hint_FilenameHasDifferingCase = (o:{reference:string, filename:string}) => m(this.HINT_FilenameHasDifferingCase,
    `File on disk '${o.filename}' does not match case of '${o.reference}' in source file; this is an error on platforms with case-sensitive filesystems.`);
  static HINT_FilenameHasDifferingCase = SevHint | 0x0009;

  static Error_UnknownFileFormat = (o:{format:string}) => m(this.ERROR_UnknownFileFormat,
    `Unknown file format ${o.format}; only Markdown (.md), JSON (.json), and Text (.txt) are supported.`);
  static ERROR_UnknownFileFormat = SevError | 0x000A;

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static Info_ProjectBuiltSuccessfully = (o:{filename:string}) => ({filename:o.filename, ...m(this.INFO_ProjectBuiltSuccessfully,
    `Project ${o.filename} built successfully.`)});
  static INFO_ProjectBuiltSuccessfully = SevInfo | 0x000B;

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static Info_ProjectNotBuiltSuccessfully = (o:{filename:string}) => ({filename:o.filename, ...m(this.INFO_ProjectNotBuiltSuccessfully,
    `Project ${o.filename} failed to build.`)});
  static INFO_ProjectNotBuiltSuccessfully = SevInfo | 0x000C;

  static Info_TooManyMessages = (o:{count:number}) => m(this.INFO_TooManyMessages,
    `More than ${o.count} warnings or errors received; suppressing further messages.`);
  static INFO_TooManyMessages = SevInfo | 0x000D;

  static Error_FileTypeNotFound = (o:{ext:string}) => m(this.ERROR_FileTypeNotFound,
    `A file of type ${o.ext} was not found in the project.`);
  static ERROR_FileTypeNotFound = SevError | 0x000E;

  static Error_NotAProjectFile = (o:{filename:string}) => m(this.ERROR_NotAProjectFile,
    `File ${o.filename} must have a .kpj extension to be treated as a project.`);
  static ERROR_NotAProjectFile = SevError | 0x000F;

  static Info_WarningsHaveFailedBuild = () => m(this.INFO_WarningsHaveFailedBuild,
    `The build failed because option "treat warnings as errors" is enabled and there are one or more warnings.`);
  static INFO_WarningsHaveFailedBuild = SevInfo | 0x0010;
}

