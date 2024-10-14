import { CompilerError, CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def, CompilerMessageSpecWithException } from "@keymanapp/developer-utils";

const Namespace = CompilerErrorNamespace.Infrastructure;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

export class InfrastructureMessages {
  static FATAL_UnexpectedException = SevFatal | 0x0001;
  static Fatal_UnexpectedException = (o:{e: any}) => CompilerMessageSpecWithException(this.FATAL_UnexpectedException, null, o.e ?? 'unknown error');

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static INFO_BuildingFile = SevInfo | 0x0002;
  static Info_BuildingFile = (o:{filename:string,relativeFilename:string}) => ({filename:o.filename, ...m(this.INFO_BuildingFile,
    `Building ${def(o.relativeFilename)}`)});

  static ERROR_FileDoesNotExist = SevError | 0x0003;
  static Error_FileDoesNotExist = (o:{filename:string}) => m(this.ERROR_FileDoesNotExist,
    `File ${def(o.filename)} does not exist`);

  static ERROR_FileTypeNotRecognized = SevError | 0x0004;
  static Error_FileTypeNotRecognized = (o:{filename: string, extensions: string}) => m(this.ERROR_FileTypeNotRecognized,
    `Unrecognised input file ${def(o.filename)}, expecting ${def(o.extensions)}, or project folder`);

  static ERROR_OutFileNotValidForProjects = SevError | 0x0005;
  static Error_OutFileNotValidForProjects = () => m(this.ERROR_OutFileNotValidForProjects,
    `--out-file should not be specified for project builds`);

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static INFO_FileBuiltSuccessfully = SevInfo | 0x0006;
  static Info_FileBuiltSuccessfully = (o:{filename:string,relativeFilename:string}) => ({filename:o.filename, ...m(this.INFO_FileBuiltSuccessfully,
    `${def(o.relativeFilename)} built successfully.`)});

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static INFO_FileNotBuiltSuccessfully = SevInfo | 0x0007;
  static Info_FileNotBuiltSuccessfully = (o:{filename:string,relativeFilename:string}) => ({filename:o.filename, ...m(this.INFO_FileNotBuiltSuccessfully,
    `${def(o.relativeFilename)} failed to build.`)});

  static ERROR_InvalidProjectFile = SevError | 0x0008;
  static Error_InvalidProjectFile = (o:{message:string}) => m(this.ERROR_InvalidProjectFile,
    `Project file is not valid: ${def(o.message)}`);

  static HINT_FilenameHasDifferingCase = SevHint | 0x0009;
  static Hint_FilenameHasDifferingCase = (o:{reference:string, filename:string}) => m(this.HINT_FilenameHasDifferingCase,
    `File on disk '${def(o.filename)}' does not match case of '${def(o.reference)}' in source file; this is an error on platforms with case-sensitive filesystems.`);

  static ERROR_UnknownFileFormat = SevError | 0x000A;
  static Error_UnknownFileFormat = (o:{format:string}) => m(this.ERROR_UnknownFileFormat,
    `Unknown file format ${def(o.format)}; only Markdown (.md), JSON (.json), and Text (.txt) are supported.`);

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static INFO_ProjectBuiltSuccessfully = SevInfo | 0x000B;
  static Info_ProjectBuiltSuccessfully = (o:{filename:string,relativeFilename:string}) => ({filename:o.filename, ...m(this.INFO_ProjectBuiltSuccessfully,
    `Project ${def(o.relativeFilename)} built successfully.`)});

  // For this message, we override the filename with the passed-in file. A bit of a hack but does the job
  static INFO_ProjectNotBuiltSuccessfully = SevInfo | 0x000C;
  static Info_ProjectNotBuiltSuccessfully = (o:{filename:string,relativeFilename:string}) => ({filename:o.filename, ...m(this.INFO_ProjectNotBuiltSuccessfully,
    `Project ${def(o.relativeFilename)} failed to build.`)});

  static INFO_TooManyMessages = SevInfo | 0x000D;
  static Info_TooManyMessages = (o:{count:number}) => m(this.INFO_TooManyMessages,
    `More than ${def(o.count)} warnings or errors received; suppressing further messages.`);

  static ERROR_FileTypeNotFound = SevError | 0x000E;
  static Error_FileTypeNotFound = (o:{ext:string}) => m(this.ERROR_FileTypeNotFound,
    `A file of type ${def(o.ext)} was not found in the project.`);

  static ERROR_NotAProjectFile = SevError | 0x000F;
  static Error_NotAProjectFile = (o:{filename:string}) => m(this.ERROR_NotAProjectFile,
    `File ${def(o.filename)} must have a .kpj extension to be treated as a project.`);

  static INFO_WarningsHaveFailedBuild = SevInfo | 0x0010;
  static Info_WarningsHaveFailedBuild = () => m(this.INFO_WarningsHaveFailedBuild,
    `The build failed because option "treat warnings as errors" is enabled and there are one or more warnings.`);

  static ERROR_CannotCreateFolder = SevError | 0x0011;
  static Error_CannotCreateFolder = (o:{folderName:string, e: any}) => CompilerMessageSpecWithException(this.ERROR_CannotCreateFolder, null,
    `Unable to create folder ${def(o.folderName)}: ${o.e ?? 'unknown error'}`);

  static ERROR_InvalidProjectFolder = SevError | 0x0012;
  static Error_InvalidProjectFolder = (o:{folderName:string}) => m(this.ERROR_InvalidProjectFolder,
    `The folder ${def(o.folderName)} does not appear to be a Keyman Developer project.`);

  static ERROR_UnsupportedProjectVersion = SevError | 0x0013;
  static Error_UnsupportedProjectVersion = (o:{version:string}) => m(this.ERROR_UnsupportedProjectVersion,
    `Project version ${def(o.version)} is not supported by this version of Keyman Developer.`);

  static HINT_ProjectIsVersion10 = SevHint | 0x0014;
  static Hint_ProjectIsVersion10 = () => m(this.HINT_ProjectIsVersion10,
    `The project file is an older version and can be upgraded to version 17.0`);

  static ERROR_OutFileCanOnlyBeSpecifiedWithSingleInfile = SevError | 0x0015;
  static Error_OutFileCanOnlyBeSpecifiedWithSingleInfile = () => m(this.ERROR_OutFileCanOnlyBeSpecifiedWithSingleInfile,
    `Parameter --out-file can only be used with a single input file.`);

  static ERROR_InvalidMessageFormat = SevError | 0x0016;
  static Error_InvalidMessageFormat = (o:{message:string}) => m(this.ERROR_InvalidMessageFormat,
    `Invalid parameter: --message ${def(o.message)} must match format '[KM]#####[:Disable|Info|Hint|Warn|Error]'`);

  static ERROR_MessageNamespaceNotFound = SevError | 0x0017;
  static Error_MessageNamespaceNotFound = (o:{code: number}) => m(this.ERROR_MessageNamespaceNotFound,
    `Invalid parameter: --message ${def(o.code?CompilerError.formatCode(o.code):undefined)} does not have a recognized namespace`);

  static ERROR_MessageCodeNotFound = SevError | 0x0018;
  static Error_MessageCodeNotFound = (o:{code: number}) => m(this.ERROR_MessageCodeNotFound,
    `Invalid parameter: --message ${o.code?CompilerError.formatCode(o.code):undefined} is not a recognized code`);

  static ERROR_MessageCannotBeCoerced = SevError | 0x0019;
  static Error_MessageCannotBeCoerced = (o:{code: number}) => m(this.ERROR_MessageCannotBeCoerced,
    `Invalid parameter: --message ${def(o.code?CompilerError.formatCode(o.code):undefined)} is not of type 'info', 'hint' or 'warn', and cannot be coerced`);

  static ERROR_UnrecognizedMessageCode = SevError | 0x001a;
  static Error_UnrecognizedMessageCode = (o:{message:string}) => m(
    this.ERROR_UnrecognizedMessageCode,
    `Invalid parameter: message identifier '${def(o.message)}' must match format '[KM]#####' or be a search for a ...`);

  static ERROR_MustSpecifyMessageCode = SevError | 0x001b;
  static Error_MustSpecifyMessageCode = () => m(
    this.ERROR_MustSpecifyMessageCode,
    `Must specify at least one message code or -a for all messages`);

  static ERROR_MessagesCannotBeFilteredForMarkdownFormat = SevError | 0x001c;
  static Error_MessagesCannotBeFilteredForMarkdownFormat = () => m(
    this.ERROR_MessagesCannotBeFilteredForMarkdownFormat,
    `Messages cannot be filtered for markdown format`);

  static ERROR_OutputPathMustBeSpecifiedForMarkdownFormat = SevError | 0x001d;
  static Error_OutputPathMustBeSpecifiedForMarkdownFormat = () => m(
    this.ERROR_OutputPathMustBeSpecifiedForMarkdownFormat,
    `Output path must be specified with -o for markdown output format`);

  static ERROR_OutputPathMustExistAndBeADirectory = SevError | 0x001e;
  static Error_OutputPathMustExistAndBeADirectory = (o:{outPath:string}) => m(
    this.ERROR_OutputPathMustExistAndBeADirectory,
    `Output path ${def(o.outPath)} must exist and must be a folder`);

  static ERROR_MessageNamespaceNameNotFound = SevError | 0x001f;
  static Error_MessageNamespaceNameNotFound = (o:{message: string}) => m(
    this.ERROR_MessageNamespaceNameNotFound,
    `Invalid parameter: --message ${def(o.message)} does not have a recognized namespace`);

  static ERROR_GenerateRequiresId = SevError | 0x0020;
  static Error_GenerateRequiresId = () => m(
    this.ERROR_GenerateRequiresId,
    `The generate command requires a single 'id' parameter`);
 }

