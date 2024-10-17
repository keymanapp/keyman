/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Messages for kmc-copy
 */

import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def, CompilerMessageSpecWithException } from "@keymanapp/developer-utils";

const Namespace = CompilerErrorNamespace.Copier;
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
  static Fatal_UnexpectedException = (o:{e: any}) => m(
    this.FATAL_UnexpectedException,
    null, o.e ?? 'unknown error'
  );

  static INFO_CopyingProject = SevInfo | 0x0002;
  static Info_CopyingProject = (o:{type: string, id: string}) => m(
    this.INFO_CopyingProject,
    `Copying project of type ${def(o.type)} with id ${def(o.id)}`
  );

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

  static INFO_CreatingFolder = SevInfo | 0x0007;
  static Info_CreatingFolder = (o:{path: string}) => m(
    this.INFO_CreatingFolder,
    `Creating folder ${def(o.path)}`
  );

  static INFO_WritingFile = SevInfo | 0x0008;
  static Info_WritingFile = (o:{filename: string}) => m(
    this.INFO_WritingFile,
    `Writing file ${def(o.filename)}`
  );

  static ERROR_CannotFindInputProject = SevError | 0x0009;
  static Error_CannotFindInputProject = (o:{project:string}) => m(
    this.ERROR_CannotFindInputProject,
    `Could not find project file '${def(o.project)}'`
  );

  static WARN_FileNotFound = SevWarn | 0x000A;
  static Warn_FileNotFound = (o:{filename: string, newFilename: string}) => m(
    this.WARN_FileNotFound,
    `The file '${def(o.filename)}' could not be found, skipping file copy. Any references have been updated to '${def(o.newFilename)}'.`
  );

  static WARN_NoWordlistsFound = SevWarn | 0x000B;
  static Warn_NoWordlistsFound = (o:{filename: string}) => m(
    this.WARN_NoWordlistsFound,
    `No wordlists could be found in the lexical model source file '${def(o.filename)}'. The file may be too complex for kmc-copy to parse.`
  );

  static ERROR_PackageFileCouldNotBeRead = SevError | 0x000C;
  static Error_PackageFileCouldNotBeRead = (o:{filename: string}) => m(
    this.ERROR_PackageFileCouldNotBeRead,
    `The package source file '${def(o.filename)}' could not be loaded. The file may have an invalid format.`
  );

  static ERROR_UnsupportedProjectVersion = SevError | 0x000D;
  static Error_UnsupportedProjectVersion = (o:{filename: string, version: string}) => m(this.ERROR_UnsupportedProjectVersion,
    `Project version ${def(o.version)} for '${def(o.filename)}' is not supported by this version of Keyman Developer.`);

  static ERROR_InvalidProjectFile = SevError | 0x000E;
  static Error_InvalidProjectFile = (o:{filename: string, message: string}) => m(
    this.ERROR_InvalidProjectFile,
    `Project file '${def(o.filename)}' is not valid: ${def(o.message)}`);

  static ERROR_ProjectFileCouldNotBeRead = SevError | 0x000F;
  static Error_ProjectFileCouldNotBeRead = (o:{filename: string}) => m(
    this.ERROR_ProjectFileCouldNotBeRead,
    `Project file '${def(o.filename)}' could not be read`);
};
