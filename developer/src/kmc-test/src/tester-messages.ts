/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Messages for kmc-test
 */

import { CompilerErrorNamespace, CompilerErrorSeverity,
  // CompilerMessageSpec as m, CompilerMessageDef as def,
  CompilerMessageSpecWithException } from "@keymanapp/developer-utils";

const Namespace = CompilerErrorNamespace.Tester;
// const SevVerbose = CompilerErrorSeverity.Verbose | Namespace;
// const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
// const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

/**
 * @internal
 */
export class TesterMessages {
  static FATAL_UnexpectedException = SevFatal | 0x0001;
  static Fatal_UnexpectedException = (o:{e: any}) => CompilerMessageSpecWithException(
    this.FATAL_UnexpectedException,
    null,
    o.e ?? 'unknown error'
  );

  // unused 0x0002 (was INFO_CopyingProject, now in InfrastructureMessages)

  // static ERROR_OutputPathAlreadyExists = SevError | 0x0004;
  // static Error_OutputPathAlreadyExists = (o:{outPath:string}) => m(
  //   this.ERROR_OutputPathAlreadyExists,
  //   `Output path ${def(o.outPath)} already exists, not overwriting`,
  // );

};
