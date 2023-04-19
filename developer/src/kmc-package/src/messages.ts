import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.PackageCompiler;
// const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
const SevWarn = CompilerErrorSeverity.Warn | Namespace;
// const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

export class CompilerMessages {
  static Fatal_UnexpectedException = (o:{e: any}) => m(this.FATAL_UnexpectedException, `Unexpected exception: ${(o.e ?? 'unknown error').toString()}\n\nCall stack:\n${(o.e instanceof Error ? o.e.stack : (new Error()).stack)}`);
  static FATAL_UnexpectedException = SevFatal | 0x0001;

  static Warn_AbsolutePath = (o:{filename: string}) => m(this.WARN_AbsolutePath, `File ${o.filename} has an absolute path, which is not portable`);
  static WARN_AbsolutePath = SevWarn | 0x0002;
}

