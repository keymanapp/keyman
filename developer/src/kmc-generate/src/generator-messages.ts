/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Messages for kmc-generate
 */

import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def, CompilerMessageSpecWithException } from "@keymanapp/developer-utils";

const Namespace = CompilerErrorNamespace.Generator;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

/**
 * @internal
 */
export class GeneratorMessages {
  static FATAL_UnexpectedException = SevFatal | 0x0001;
  static Fatal_UnexpectedException = (o:{e: any}) => m(
    this.FATAL_UnexpectedException, null, o.e ?? 'unknown error'
  );

  static INFO_GeneratingProject = SevInfo | 0x0002;
  static Info_GeneratingProject = (o:{type: string, id: string}) => m(
    this.INFO_GeneratingProject,
    `Generating project of type ${def(o.type)} with id ${def(o.id)}`
  );

  static ERROR_CannotCreateFolder = SevError | 0x0003;
  static Error_CannotCreateFolder = (o:{folderName:string, e: any}) => CompilerMessageSpecWithException(
    this.ERROR_CannotCreateFolder, null,
    `Unable to create folder ${def(o.folderName)}: ${o.e ?? 'unknown error'}`
  );

  static ERROR_OutputPathAlreadyExists = SevError | 0x0004;
  static Error_OutputPathAlreadyExists = (o:{outPath:string}) => m(
    this.ERROR_OutputPathAlreadyExists,
    `Output path ${def(o.outPath)} already exists, not overwriting`
  );

  static ERROR_CannotWriteOutputFile = SevError | 0x0005;
  static Error_CannotWriteOutputFile = (o:{filename:string, e: any}) => CompilerMessageSpecWithException(
    this.ERROR_CannotWriteOutputFile, null,
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

  static ERROR_InvalidTarget = SevError | 0x0007;
  static Error_InvalidTarget = (o:{target:string}) => m(
    this.ERROR_InvalidTarget,
    `The specified target platform '${def(o.target)}' is not a valid target.`
  );

  static ERROR_InvalidKeymanKeyboardId = SevError | 0x0008;
  static Error_InvalidKeymanKeyboardId = (o:{id:string}) => m(
    this.ERROR_InvalidKeymanKeyboardId,
    `The specified keyboard id '${def(o.id)}' contains characters that are not permitted for a .kmn keyboard id or filename.`
  );

  static ERROR_InvalidLdmlKeyboardId = SevError | 0x0009;
  static Error_InvalidLdmlKeyboardId = (o:{id:string}) => m(
    this.ERROR_InvalidLdmlKeyboardId,
    `The specified keyboard id '${def(o.id)}' contains characters that are not permitted for an LDML .xml keyboard id or filename.`
  );

  static ERROR_InvalidLexicalModelId = SevError | 0x000A;
  static Error_InvalidLexicalModelId = (o:{id:string}) => m(
    this.ERROR_InvalidLexicalModelId,
    `The specified lexical model id '${def(o.id)}' contains characters that are not permitted or does not match the required pattern of 'author.bcp47.uniq'.`
  );
};
