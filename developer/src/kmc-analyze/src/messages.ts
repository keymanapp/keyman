import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, compilerExceptionToString as exc } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.Analyzer;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
// const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

export class AnalyzerMessages {
  static Fatal_UnexpectedException = (o:{e: any}) => m(this.FATAL_UnexpectedException, `Unexpected exception: ${exc(o.e)}`);
  static FATAL_UnexpectedException = SevFatal | 0x0001;

  static Info_ScanningFile = (o:{type: string, name: string}) => m(this.INFO_ScanningFile,
    `Scanning ${o.type} file ${o.name}`);
  static INFO_ScanningFile = SevInfo | 0x0002;
};
