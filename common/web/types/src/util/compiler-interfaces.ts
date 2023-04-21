/**
 * Abstract interface for compiler error and warning messages
 */
export interface CompilerEvent {
  line?: number;
  code: number;
  message: string;
};

export enum CompilerErrorSeverity {
  Info =          0x000000, // Informational, not necessarily a problem
  Hint =          0x100000, // Something the user might want to be aware of
  Warn =          0x200000, // Warning: Not great, but we can keep going.
  Error =         0x300000, // Severe error where we can't continue
  Fatal =         0x400000, // OOM or should-not-happen internal problem

  Severity_Mask = 0xF00000, // includes reserved bits
  Error_Mask =    0x0FFFFF,
};

export function compilerErrorSeverityName(code: number): string {
  let severity = code & CompilerErrorSeverity.Severity_Mask;
  switch(severity) {
    case CompilerErrorSeverity.Info: return 'INFO';
    case CompilerErrorSeverity.Hint: return 'HINT';
    case CompilerErrorSeverity.Warn: return 'WARN';
    case CompilerErrorSeverity.Error: return 'ERROR';
    case CompilerErrorSeverity.Fatal: return 'FATAL';
    /* istanbul ignore next */
    default: return 'UNKNOWN';
  }
}

/**
 * Defines the error code ranges for various compilers. Once defined, these
 * ranges must not be changed as external modules may depend on specific error
 * codes. Individual errors are defined at a compiler level, for example,
 * kmn-keyboard/src/compiler/messages.ts.
 */
export enum CompilerErrorNamespace {
  /**
   * kmc-keyboard errors between 0x0000…0x0FFF
   */
  KeyboardCompiler = 0x0000,
  /**
   * common/web/types errors between 0x1000…0x1FFF
   */
  CommonTypes = 0x1000,
  /**
   * kmc-kmn errors between 0x2000…0x2FFF; these map to
   * the base codes found in kmn_compiler_errors.h, exclusive severity flags
   */
  KmnCompiler = 0x2000,
  /**
   * kmc-model errors between 0x3000…0x3FFF
   */
  ModelCompiler = 0x3000,
  /**
   * kmc-package errors between 0x4000…0x4FFF
   */
  PackageCompiler = 0x4000,
  /**
   * kmc and related infrastructure errors between 0x5000…0x5FFF;
   */
  Infrastructure = 0x5000,
};

/**
 * Abstract interface for callbacks, to abstract out file i/o
 */
export interface CompilerCallbacks {
  /**
   * Attempt to load a file. Return falsy if not found.
   * @param baseFilename
   * @param filename
   */
  loadFile(baseFilename: string, filename: string | URL): Buffer;
  loadLdmlKeyboardSchema(): Buffer;
  loadLdmlKeyboardTestSchema(): Buffer;
  reportMessage(event: CompilerEvent): void;
  loadKvksJsonSchema(): Buffer;
  loadKpjJsonSchema(): Buffer;
};

/**
 * Convenience function for constructing CompilerEvents
 * @param code
 * @param message
 * @returns
 */
export const CompilerMessageSpec = (code: number, message: string) : CompilerEvent => { return { code, message } };
