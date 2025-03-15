/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Messages for kmc-copy
 */

import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def, CompilerMessageSpecWithException } from "@keymanapp/developer-utils";

const Namespace = CompilerErrorNamespace.Copier;
const SevVerbose = CompilerErrorSeverity.Verbose | Namespace;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

/**
 * @internal
 */
export class CopierMessages {
  static FATAL_UnexpectedException = SevFatal | 0x0001;
  static Fatal_UnexpectedException = (o:{e: any}) => CompilerMessageSpecWithException(
    this.FATAL_UnexpectedException,
    null,
    o.e ?? 'unknown error'
  );

  // unused 0x0002 (was INFO_CopyingProject, now in InfrastructureMessages)

  static ERROR_CannotCreateFolder = SevError | 0x0003;
  static Error_CannotCreateFolder = (o:{folderName:string, e: any}) => CompilerMessageSpecWithException(
    this.ERROR_CannotCreateFolder,
    null,
    `Unable to create folder ${def(o.folderName)}: ${o.e ?? 'unknown error'}`
  );

  static ERROR_OutputPathAlreadyExists = SevError | 0x0004;
  static Error_OutputPathAlreadyExists = (o:{outPath:string}) => m(
    this.ERROR_OutputPathAlreadyExists,
    `Output path ${def(o.outPath)} already exists, not overwriting`
  );

  static ERROR_CannotWriteOutputFile = SevError | 0x0005;
  static Error_CannotWriteOutputFile = (o:{filename:string, e: any}) => CompilerMessageSpecWithException(
    this.ERROR_CannotWriteOutputFile,
    null,
    `Unable to write file ${def(o.filename)}: ${o.e ?? 'unknown error'}`
  );

  // See also PackageCompilerMessages.WARN_PackageNameDoesNotFollowLexicalModelConventions
  static WARN_ModelIdDoesNotFollowLexicalModelConventions = SevWarn | 0x0006;
  static Warn_ModelIdDoesNotFollowLexicalModelConventions = (o:{id: string}) => m(
    this.WARN_ModelIdDoesNotFollowLexicalModelConventions,
    `The id ${def(o.id)} does not follow the recommended model id conventions. The id should be all lower case, `+
    `include only alphanumeric characters and underscore (_), not start with a digit, and should have the structure `+
    `<author>.<bcp47>.<uniq>`
  );

  static INFO_DryRunCreatingFolder = SevInfo | 0x0007;
  static Info_DryRunCreatingFolder = (o:{path: string}) => m(
    this.INFO_DryRunCreatingFolder,
    `Would create folder ${def(o.path)}`
  );

  static INFO_DryRunWritingFile = SevInfo | 0x0008;
  static Info_DryRunWritingFile = (o:{filename: string}) => m(
    this.INFO_DryRunWritingFile,
    `Would write file ${def(o.filename)}`
  );

  static ERROR_CannotFindInputProject = SevError | 0x0009;
  static Error_CannotFindInputProject = (o:{project:string}) => m(
    this.ERROR_CannotFindInputProject,
    `Could not find project file '${def(o.project)}'`
  );

  static WARN_FileNotFound = SevWarn | 0x000A;
  static Warn_FileNotFound = (o:{filename: string, newFilename: string}) => m(
    this.WARN_FileNotFound,
    `The file '${def(o.filename)}' could not be found, skipping file copy. Any references have been updated to '${def(o.newFilename)}'`
  );

  static WARN_NoWordlistsFound = SevWarn | 0x000B;
  static Warn_NoWordlistsFound = (o:{filename: string}) => m(
    this.WARN_NoWordlistsFound,
    `No wordlists could be found in the lexical model source file '${def(o.filename)}'. The file may be too complex for kmc-copy to parse`
  );

  static ERROR_PackageFileCouldNotBeRead = SevError | 0x000C;
  static Error_PackageFileCouldNotBeRead = (o:{filename: string}) => m(
    this.ERROR_PackageFileCouldNotBeRead,
    `The package source file '${def(o.filename)}' could not be loaded. The file may have an invalid format`
  );

  static ERROR_UnsupportedProjectVersion = SevError | 0x000D;
  static Error_UnsupportedProjectVersion = (o:{filename: string, version: string}) => m(this.ERROR_UnsupportedProjectVersion,
    `Project version ${def(o.version)} for '${def(o.filename)}' is not supported by this version of Keyman Developer`
  );

  static ERROR_InvalidProjectFile = SevError | 0x000E;
  static Error_InvalidProjectFile = (o:{filename: string, message: string}) => m(
    this.ERROR_InvalidProjectFile,
    `Project file '${def(o.filename)}' is not valid: ${def(o.message)}`
  );

  static ERROR_ProjectFileCouldNotBeRead = SevError | 0x000F;
  static Error_ProjectFileCouldNotBeRead = (o:{filename: string}) => m(
    this.ERROR_ProjectFileCouldNotBeRead,
    `Project file '${def(o.filename)}' could not be read`
  );

  static INFO_DryRun = SevInfo | 0x0010;
  static Info_DryRun = (o:{outPath: string}) => m(
    this.INFO_DryRun,
    `Dry run requested. No changes have been saved`
  );

  // 0x0011 unused

  static ERROR_CannotDownloadFolderFromGitHub = SevError | 0x0012;
  static Error_CannotDownloadFolderFromGitHub = (o:{ref: string, message?: string, cause?: string}) => m(
    this.ERROR_CannotDownloadFolderFromGitHub,
    `The folder '${def(o.ref)}' could not be downloaded: ${def(o.message)} ${def(o.cause)}`, `
    An error was encountered attempting to download a folder from GitHub API. Check
    the provided error details for the cause.
  `);

  static ERROR_FolderDownloadedFromGitHubIsNotAValidFolder = SevError | 0x0013;
  static Error_FolderDownloadedFromGitHubIsNotAValidFolder = (o:{ref: string}) => m(
    this.ERROR_FolderDownloadedFromGitHubIsNotAValidFolder,
    `The path '${def(o.ref)}' does not appear to be a folder on GitHub`, `
    The provided path may be a file or may not exist. Check the reference
    before trying again.
  `);

  static WARN_CannotDownloadFileFromGitHub = SevWarn | 0x0014;
  static Warn_CannotDownloadFileFromGitHub = (o:{ref: string, message?: string, cause?: string}) => m(
    this.WARN_CannotDownloadFileFromGitHub,
    `The file '${def(o.ref)}' could not be downloaded: ${def(o.message)} ${def(o.cause)}`, `
    An error was encountered attempting to download a file from GitHub. Check the
    provided error details for the cause.
  `);

  static ERROR_InvalidCloudKeyboardId = SevError | 0x0015;
  static Error_InvalidCloudKeyboardId = (o:{id: string}) => m(
    this.ERROR_InvalidCloudKeyboardId,
    `The keyboard identifier '${def(o.id)}' is not a valid keyboard identifier`, `
    Keyboard identifiers on Keyman Cloud can only use the characters
    \`a\`-\`z\`, \`0\`-\`9\`, and \`_\`.
  `);

  static ERROR_CouldNotRetrieveFromCloud = SevError | 0x0016;
  static Error_CouldNotRetrieveFromCloud = (o:{id: string, message?: string, cause?: string}) => m(
    this.ERROR_CouldNotRetrieveFromCloud,
    `Details for keyboard or model identified by '${def(o.id)}' could not be `+
    `downloaded: ${def(o.message)} ${def(o.cause)}`, `
    An error was encountered attempting to retrieve keyboard or model details from
    Keyman Cloud API. Check the provided error details for the cause.
  `);

  static ERROR_KeymanCloudReturnedInvalidData = SevError | 0x0017;
  static Error_KeymanCloudReturnedInvalidData = (o:{id: string}) => m(
    this.ERROR_KeymanCloudReturnedInvalidData,
    `Keyman Cloud API returned invalid data for keyboard or model identified by '${def(o.id)}'`, `
    There may be a network error or a server error. Retry your request later or
    contact Keyman Support for assistance.
  `);

  static ERROR_CloudDoesNotHaveSource = SevError | 0x0018;
  static Error_CloudDoesNotHaveSource = (o:{id: string}) => m(
    this.ERROR_CloudDoesNotHaveSource,
    `The keyboard or model identified by '${def(o.id)}' does not have source available`, `
    Legacy keyboards in Keyman Cloud do not have source available. Check the Keyman
    keyboard catalog at https://keyman.com/keyboards for further details. Some new
    keyboards or models may be available as binary-only.
  `);

  static ERROR_CannotDownloadRepoFromGitHub = SevError | 0x0019 ;
  static Error_CannotDownloadRepoFromGitHub = (o:{ref: string, message?: string, cause?: string}) => m(
    this.ERROR_CannotDownloadRepoFromGitHub,
    `The repository at '${def(o.ref)}' could not be accessed: ${def(o.message)} ${def(o.cause)}`, `
    An error was encountered attempting to download details about a repository
    from GitHub API. Check the provided error details for the cause.
  `);

  //------------------------------------------------------------------------------|
  // max length of detail message lines (checked by verifyCompilerMessagesObject) |
  //------------------------------------------------------------------------------|

  static ERROR_CouldNotFindDefaultBranchOnGitHub = SevError | 0x001A;
  static Error_CouldNotFindDefaultBranchOnGitHub = (o:{ref: string}) => m(
    this.ERROR_CouldNotFindDefaultBranchOnGitHub,
    `The default branch could not be found for the GitHub repository '${def(o.ref)}'`, `
    The repository may be private, or you may have a typo in the owner or
    repository name.
  `);

  static INFO_CannotDownloadBinaryFileFromGitHub = SevInfo | 0x001B;
  static Info_CannotDownloadBinaryFileFromGitHub = (o:{ref: string, message?: string, cause?: string}) => m(
    this.INFO_CannotDownloadBinaryFileFromGitHub,
    `The Keyman binary file '${def(o.ref)}' could not be downloaded: ${def(o.message)} `+
    `${def(o.cause)}. This is not normally a problem`, `
    In most repositories, Keyman binary files such as .kmx, .kmp, .js are not
    included. This is not normally a problem, as the files can be built from the
    source. Check the provided error details for more details.
  `);

  static VERBOSE_DownloadingFile = SevVerbose | 0x001C;
  static Verbose_DownloadingFile = (o:{filename: string, url: string}) => m(
    this.VERBOSE_DownloadingFile,
    `Downloading '${def(o.filename)}' from '${def(o.url)}'`,
  );

  static VERBOSE_DownloadingFolder = SevVerbose | 0x001D;
  static Verbose_DownloadingFolder = (o:{path: string, url: string}) => m(
    this.VERBOSE_DownloadingFolder,
    `Downloading folder '${def(o.path)}' from '${def(o.url)}'`,
  );

  // See also generator-messages.ERROR_InvalidKeymanKeyboardId
  static ERROR_InvalidKeyboardId = SevError | 0x001E;
  static Error_InvalidKeyboardId = (o:{id: string}) => m(
    this.ERROR_InvalidKeyboardId,
    `The specified keyboard id '${def(o.id)}' contains characters that are not permitted for a keyboard id or filename.`,
  );

  // See also generator-messages.ERROR_InvalidLexicalModelId
  static ERROR_InvalidLexicalModelId = SevError | 0x001F;
  static Error_InvalidLexicalModelId = (o:{id:string}) => m(
    this.ERROR_InvalidLexicalModelId,
    `The specified lexical model id '${def(o.id)}' contains characters that are not permitted or does not match the required pattern of 'author.bcp47.uniq'.`,
  );

  static WARN_FilenameCollides = SevWarn | 0x0020;
  static Warn_FilenameCollides = (o:{filename:string}) => m(
    this.WARN_FilenameCollides,
    `The output file '${def(o.filename)}' has two different possible source files.`,
  );

  static VERBOSE_CopyingFile = SevVerbose | 0x0021;
  static Verbose_CopyingFile = (o:{from: string, to: string}) => m(
    this.VERBOSE_CopyingFile,
    `Copying file '${def(o.from)}' to '${def(o.to)}'`,
  );

};
