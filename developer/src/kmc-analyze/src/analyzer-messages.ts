/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def, CompilerMessageSpecWithException, KeymanUrls } from "@keymanapp/developer-utils";

const Namespace = CompilerErrorNamespace.Analyzer;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
const SevWarn = CompilerErrorSeverity.Warn | Namespace;
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

  static readonly WARN_PreviousMapFileCouldNotBeLoaded = SevWarn | 0x0003;
  static readonly Warn_PreviousMapFileCouldNotBeLoaded = (o:{filename: string}) => m(
    this.WARN_PreviousMapFileCouldNotBeLoaded,
    `The map file ${def(o.filename)} is missing or not a valid JSON map file`,
  );

  static readonly WARN_PreviousMapFileCouldNotBeLoadedDueToError = SevWarn | 0x0004;
  static readonly Warn_PreviousMapFileCouldNotBeLoadedDueToError = (o:{filename: string, e: any}) => m(
    this.WARN_PreviousMapFileCouldNotBeLoadedDueToError,
    `The map file ${def(o.filename)} could not be loaded due to ${def(o.e ?? 'unknown error')}`,
  );

  static readonly WARN_PreviousMapDidNotIncludeCounts = SevWarn | 0x0005;
  static readonly Warn_PreviousMapDidNotIncludeCounts = (o:{filename: string}) => m(
    this.WARN_PreviousMapDidNotIncludeCounts,
    `The map file ${def(o.filename)} did not include counts. Changing includeCounts option to 'false' to match`,
  );

  static readonly WARN_PreviousMapDidIncludeCounts = SevWarn | 0x0006;
  static readonly Warn_PreviousMapDidIncludeCounts = (o:{filename: string}) => m(
    this.WARN_PreviousMapDidIncludeCounts,
    `The map file ${def(o.filename)} did include counts. Changing includeCounts option to 'true' to match`,
  );

};
