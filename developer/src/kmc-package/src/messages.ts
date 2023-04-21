import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.PackageCompiler;
// const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

export class CompilerMessages {
  static Fatal_UnexpectedException = (o:{e: any}) => m(this.FATAL_UnexpectedException,
    `Unexpected exception: ${(o.e ?? 'unknown error').toString()}\n\nCall stack:\n${(o.e instanceof Error ? o.e.stack : (new Error()).stack)}`);
  static FATAL_UnexpectedException = SevFatal | 0x0001;

  static Warn_AbsolutePath = (o:{filename: string}) => m(this.WARN_AbsolutePath, `File ${o.filename} has an absolute path, which is not portable.`);
  static WARN_AbsolutePath = SevWarn | 0x0002;

  static Error_FileDoesNotExist = (o:{filename: string}) => m(this.ERROR_FileDoesNotExist, `File ${o.filename} does not exist.`);
  static ERROR_FileDoesNotExist = SevError | 0x0003;

  static Error_FileCouldNotBeRead = (o:{filename: string; e: any}) => m(this.ERROR_FileCouldNotBeRead,
    `File ${o.filename} could not be read: ${(o.e ?? 'unknown error').toString()}.`);
  static ERROR_FileCouldNotBeRead = SevError | 0x0004;

  static Warn_FileIsNotABinaryKvkFile = (o:{filename: string}) => m(this.WARN_FileIsNotABinaryKvkFile,
    `File ${o.filename} does not appear to be a valid binary .kvk file; this may be an old package that includes an xml-format .kvk file. `+
    `You must update the package to include the compiled .kvk file in the package.`);
  static WARN_FileIsNotABinaryKvkFile = SevWarn | 0x0005;
}

