import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerEvent, CompilerMessageSpec, CompilerMessageDef as def, CompilerMessageSpecWithException } from "@keymanapp/developer-utils";

const Namespace = CompilerErrorNamespace.ModelCompiler;
// const SevInfo = CompilerErrorSeverity.Info | Namespace;
const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

const m = (code: number, message: string) : CompilerEvent => ({
  ...CompilerMessageSpec(code, message),
  line: ModelCompilerMessageContext.line,
  filename: ModelCompilerMessageContext.filename,
});

const m_e = (code: number, message: string, exceptionVar: any) : CompilerEvent => ({
  ...CompilerMessageSpecWithException(code, message, exceptionVar),
  line: ModelCompilerMessageContext.line,
  filename: ModelCompilerMessageContext.filename,
});

/**
 * @internal
 */
export class ModelCompilerMessageContext {
  // Context added to all messages
  static line: number;
  static filename: string;
}

/**
 * @internal
 */
export class ModelCompilerMessages {

  static FATAL_UnexpectedException = SevFatal | 0x0001;
  static Fatal_UnexpectedException = (o:{e: any}) => m_e(this.FATAL_UnexpectedException, null, o.e ?? 'unknown error');

  static HINT_MixedNormalizationForms = SevHint | 0x0002;
  static Hint_MixedNormalizationForms = (o:{wordform: string}) => m(this.HINT_MixedNormalizationForms,
    `“${def(o.wordform)}” is not in Unicode NFC. Automatically converting to NFC.`);

  static HINT_DuplicateWordInSameFile = SevHint | 0x0003;
  static Hint_DuplicateWordInSameFile = (o:{wordform: string}) => m(this.HINT_DuplicateWordInSameFile,
    `duplicate word “${def(o.wordform)}” found in same file; summing counts`);

  static ERROR_UnimplementedModelFormat = SevError | 0x0004;
  static Error_UnimplementedModelFormat = (o:{format: string}) => m(this.ERROR_UnimplementedModelFormat,
    `Unimplemented model format: ${def(o.format)}`);

  static ERROR_UnknownModelFormat = SevError | 0x0005;
  static Error_UnknownModelFormat = (o:{format: string}) => m(this.ERROR_UnknownModelFormat,
    `Unimplemented model format: ${def(o.format)}`);

  static ERROR_NoDefaultExport = SevError | 0x0006;
  static Error_NoDefaultExport = () => m(this.ERROR_NoDefaultExport,
    `Model source does have a default export. Did you remember to write \`export default source;\`?`);

  static ERROR_SearchTermToKeyMustBeExplicitlySpecified = SevError | 0x0007;
  static Error_SearchTermToKeyMustBeExplicitlySpecified = () => m(this.ERROR_SearchTermToKeyMustBeExplicitlySpecified,
    "searchTermToKey must be explicitly specified");

  static ERROR_UTF16BEUnsupported = SevError | 0x0008;
  static Error_UTF16BEUnsupported = () => m(this.ERROR_UTF16BEUnsupported, 'UTF-16BE is unsupported');

  static ERROR_UnknownWordBreaker = SevError | 0x0009;
  static Error_UnknownWordBreaker = (o:{spec:string}) => m(this.ERROR_UnknownWordBreaker,
    `Unknown word breaker: ${def(o.spec)}`);

  static ERROR_UnsupportedScriptOverride = SevError | 0x000A;
  static Error_UnsupportedScriptOverride = (o:{option:string}) => m(this.ERROR_UnsupportedScriptOverride,
    `Unsupported script override: ${def(o.option)}`);
};

/**
 * A ModelCompilerError should be thrown when an unrecoverable error occurs that
 * would block further compilation. It will be caught in the top-most compiler
 * API endpoint and converted into a callback message.
 */
export class ModelCompilerError extends Error {
  constructor(public event: CompilerEvent) {
    super(event.message);
  }
}
