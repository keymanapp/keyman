import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageSpecWithException } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.Analyzer;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
// const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

/**
 * @public
 * Compiler messages for `kmc analyze`
 */
export class AnalyzerMessages {
  /** @internal */
  static Fatal_UnexpectedException = (o:{e: any}) => CompilerMessageSpecWithException(this.FATAL_UnexpectedException, null, o.e ?? 'unknown error');
  /**
   * Raised when an analysis components experiences an internal error. If you
   * experience this error, it should be reported to the Keyman team for
   * resolution via https://github.com/keymanapp/keyman/issues/new
   */
  static readonly FATAL_UnexpectedException = SevFatal | 0x0001;

  /** @internal */
  static Info_ScanningFile = (o:{type: string, name: string}) => m(this.INFO_ScanningFile,
    `Scanning ${o.type} file ${o.name}`);
  /**
   * Informative message reporting on the current file being scanned
   */
  static readonly INFO_ScanningFile = SevInfo | 0x0002;
};
