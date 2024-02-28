import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageSpecWithException } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.Infrastructure;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

export class InfrastructureMessages {
  static Fatal_UnexpectedException = (o:{e: any}) => CompilerMessageSpecWithException(this.FATAL_UnexpectedException, null, o.e ?? 'unknown error');
  static FATAL_UnexpectedException = SevFatal | 0x0001;

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static Info_BuildingFile = (o:{filename:string,relativeFilename:string}) => ({filename:o.filename, ...m(this.INFO_BuildingFile,
    `Building ${o.relativeFilename}`)});
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
  static Info_FileBuiltSuccessfully = (o:{filename:string,relativeFilename:string}) => ({filename:o.filename, ...m(this.INFO_FileBuiltSuccessfully,
    `${o.relativeFilename} built successfully.`)});
  static INFO_FileBuiltSuccessfully = SevInfo | 0x0006;

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static Info_FileNotBuiltSuccessfully = (o:{filename:string,relativeFilename:string}) => ({filename:o.filename, ...m(this.INFO_FileNotBuiltSuccessfully,
    `${o.relativeFilename} failed to build.`)});
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
  static Info_ProjectBuiltSuccessfully = (o:{filename:string,relativeFilename:string}) => ({filename:o.filename, ...m(this.INFO_ProjectBuiltSuccessfully,
    `Project ${o.relativeFilename} built successfully.`)});
  static INFO_ProjectBuiltSuccessfully = SevInfo | 0x000B;

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static Info_ProjectNotBuiltSuccessfully = (o:{filename:string,relativeFilename:string}) => ({filename:o.filename, ...m(this.INFO_ProjectNotBuiltSuccessfully,
    `Project ${o.relativeFilename} failed to build.`)});
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

  static Error_CannotCreateFolder = (o:{folderName:string, e: any}) => CompilerMessageSpecWithException(this.ERROR_CannotCreateFolder, null,
    `Unable to create folder ${o.folderName}: ${o.e ?? 'unknown error'}`);
  static ERROR_CannotCreateFolder = SevError | 0x0011;

  static Error_InvalidProjectFolder = (o:{folderName:string}) => m(this.ERROR_InvalidProjectFolder,
    `The folder ${o.folderName} does not appear to be a Keyman Developer project.`);
  static ERROR_InvalidProjectFolder = SevError | 0x0012;

  static Error_UnsupportedProjectVersion = (o:{version:string}) => m(this.ERROR_UnsupportedProjectVersion,
    `Project version ${o.version} is not supported by this version of Keyman Developer.`);
  static ERROR_UnsupportedProjectVersion = SevError | 0x0013;

  static Hint_ProjectIsVersion10 = () => m(this.HINT_ProjectIsVersion10,
    `The project file is an older version and can be upgraded to version 17.0`);
  static HINT_ProjectIsVersion10 = SevHint | 0x0014;

  static Error_OutFileCanOnlyBeSpecifiedWithSingleInfile = () => m(this.ERROR_OutFileCanOnlyBeSpecifiedWithSingleInfile,
    `Parameter --out-file can only be used with a single input file.`);
  static ERROR_OutFileCanOnlyBeSpecifiedWithSingleInfile = SevError | 0x0015;

  static Error_InvalidMessageFormat = (o:{message:string}) => m(this.ERROR_InvalidMessageFormat,
    `Invalid parameter: --message ${o.message} must match format '[KM]#####[:Disable|Info|Hint|Warn|Error]'`);
  static ERROR_InvalidMessageFormat = SevError | 0x0016;

  static Error_MessageNamespaceNotFound = (o:{code: number}) => m(this.ERROR_MessageNamespaceNotFound,
    `Invalid parameter: --message KM${o.code?.toString(16)} does not have a recognized namespace`);
  static ERROR_MessageNamespaceNotFound = SevError | 0x0017;

  static Error_MessageCodeNotFound = (o:{code: number}) => m(this.ERROR_MessageCodeNotFound,
    `Invalid parameter: --message KM${o.code?.toString(16)} is not a recognized code`);
  static ERROR_MessageCodeNotFound = SevError | 0x0018;

  static Error_MessageCannotBeCoerced = (o:{code: number}) => m(this.ERROR_MessageCannotBeCoerced,
    `Invalid parameter: --message KM${o.code?.toString(16)} is not a info, hint or warn message type and cannot be coerced`);
  static ERROR_MessageCannotBeCoerced = SevError | 0x0019;
}

