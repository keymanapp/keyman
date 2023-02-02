/**
 * Abstract interface for compiler error and warning messages
 */
export interface CompilerEvent {
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

export enum CompilerErrorNamespace {
  /**
   * kmc-keyboard errors between 0x0000…0x0FFF
   */
  KeyboardCompiler = 0x0000,
  /**
   * common/web/types errors between 0x1000…0x1FFF
   */
  CommonTypes = 0x1000,
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
  reportMessage(event: CompilerEvent): void;
  loadKvksJsonSchema(): Buffer;
};

/**
 * Convenience function for constructing CompilerEvents
 * @param code
 * @param message
 * @returns
 */
export const CompilerMessageSpec = (code: number, message: string) : CompilerEvent => { return { code, message } };
