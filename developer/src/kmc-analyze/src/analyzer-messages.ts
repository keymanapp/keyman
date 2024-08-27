/// <reference types="@keymanapp/common-types" />
// above is required for now, seemingly https://github.com/microsoft/TypeScript/issues/42873
// probably addressed in ts 5.5.2
import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def, CompilerMessageSpecWithException, KeymanUrls } from "@keymanapp/developer-utils";

const Namespace = CompilerErrorNamespace.Analyzer;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
// const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

/**
 * @internal
 * Compiler messages for `kmc analyze`
 */
export class AnalyzerMessages {
  static readonly FATAL_UnexpectedException = SevFatal | 0x0001;
  static readonly Fatal_UnexpectedException = (o:{e: any}) => CompilerMessageSpecWithException(
    this.FATAL_UnexpectedException,
    null,
    o.e ?? 'unknown error',
    `Raised when an analysis components has an internal error. If you
    experience this error, it should be reported to the Keyman team for
    resolution via ${KeymanUrls.NEW_KEYMAN_ISSUE()}`
  );

  static readonly INFO_ScanningFile = SevInfo | 0x0002;
  static readonly Info_ScanningFile = (o:{type: string, name: string}) => m(
    this.INFO_ScanningFile,
    `Scanning ${def(o.type)} file ${def(o.name)}`,
    `Informative message reporting on the current file being scanned`
  );
};
