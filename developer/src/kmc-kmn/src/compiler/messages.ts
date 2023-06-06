import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerEvent, CompilerMessageSpec as m, compilerExceptionToString as exc } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.KmnCompiler;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
const SevHint = CompilerErrorSeverity.Hint | Namespace;
const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

/**
 * LogLevel comes from kmn_compiler_errors.h, for legacy compiler error messages
 */
const enum LogLevel {
  LEVEL_MASK = 0xF000,
  CODE_MASK = 0x0FFF,
  CERR_FATAL = 0x8000,
  CERR_ERROR = 0x4000,
  CERR_WARNING = 0x2000,
  CERR_HINT = 0x1000,
  CERR_INFO = 0
};

/**
 * Translate the legacy compiler error messages to Severity codes
 */
const LogLevelToSeverity: Record<number,number> = {
  [LogLevel.CERR_FATAL]:   SevFatal,
  [LogLevel.CERR_ERROR]:   SevError,
  [LogLevel.CERR_WARNING]: SevWarn,
  [LogLevel.CERR_HINT]:    SevHint,
  [LogLevel.CERR_INFO]:    SevInfo
}

export const enum KmnCompilerMessageRanges {
  RANGE_KMN_COMPILER_MIN    = 0x0001, // from kmn_compiler_errors.h
  RANGE_KMN_COMPILER_MAX    = 0x07FF, // from kmn_compiler_errors.h
  RANGE_LEXICAL_MODEL_MIN   = 0x0800, // from kmn_compiler_errors.h, deprecated -- this range will not be used in future versions
  RANGE_LEXICAL_MODEL_MAX   = 0x08FF, // from kmn_compiler_errors.h, deprecated -- this range will not be used in future versions
  RANGE_CompilerMessage_Min = 0x1000, // All compiler messages listed here must be >= this value
}

/*
  The messages in this class share the namespace with messages from kmn_compiler_errors.h
  and the below ranges are reserved.
*/
export class CompilerMessages {
  static Fatal_UnexpectedException = (o:{e: any}) => m(this.FATAL_UnexpectedException, `Unexpected exception: ${exc(o.e)}`);
  static FATAL_UnexpectedException = SevFatal | 0x1000;

  static Fatal_MissingWasmModule = (o:{e?: any}) => m(this.FATAL_MissingWasmModule, `Could not instantiate WASM compiler module or initialization failed: ${exc(o.e)}`);
  static FATAL_MissingWasmModule = SevFatal | 0x1001;

  static Fatal_UnableToSetCompilerOptions = () => m(this.FATAL_UnableToSetCompilerOptions, `Unable to set compiler options`);
  static FATAL_UnableToSetCompilerOptions = SevFatal | 0x1002;

  static Fatal_CallbacksNotSet = () => m(this.FATAL_CallbacksNotSet, `Callbacks were not set with init`);
  static FATAL_CallbacksNotSet = SevFatal | 0x1003;

  static Fatal_UnicodeSetOutOfRange = () => m(this.FATAL_UnicodeSetOutOfRange, `UnicodeSet buffer was too small`);
  static FATAL_UnicodeSetOutOfRange = SevFatal | 0x1004;

  static Error_UnicodeSetHasStrings = () => m(this.ERROR_UnicodeSetHasStrings, `UnicodeSet contains strings, not allowed`);
  static ERROR_UnicodeSetHasStrings = SevError | 0x1005;

  static Error_UnicodeSetHasProperties = () => m(this.ERROR_UnicodeSetHasProperties, `UnicodeSet contains properties, not allowed`);
  static ERROR_UnicodeSetHasProperties = SevError | 0x1006;

  static Error_UnicodeSetSyntaxError = () => m(this.ERROR_UnicodeSetSyntaxError, `UnicodeSet had a Syntax Error while parsing`);
  static ERROR_UnicodeSetSyntaxError = SevError | 0x1007;

  static Error_InvalidKvksFile = (o:{filename: string, e: any}) => m(this.ERROR_InvalidKvksFile,
    `Error encountered parsing ${o.filename}: ${o.e}`);
  static ERROR_InvalidKvksFile = SevError | 0x1008;

  static Warn_InvalidVkeyInKvksFile = (o:{filename: string, invalidVkey: string}) => m(this.WARN_InvalidVkeyInKvksFile,
    `Invalid virtual key ${o.invalidVkey} found in ${o.filename}`);
  static WARN_InvalidVkeyInKvksFile = SevWarn | 0x1009;
}

export function mapErrorFromKmcmplib(line: number, code: number, msg: string): CompilerEvent {
  const severity = LogLevelToSeverity[code & LogLevel.LEVEL_MASK];
  const baseCode = code & LogLevel.CODE_MASK;
  const event: CompilerEvent = {
    line: line,
    code: severity | CompilerErrorNamespace.KmnCompiler | baseCode,
    message: msg
  };
  return event;
};
