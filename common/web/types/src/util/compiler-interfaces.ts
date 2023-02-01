/**
 * Abstract interface for compiler error and warning messages
 */
export interface CompilerEvent {
  code: number;
  message: string;
};

export enum CompilerErrorSeverity {
  Info =          0x000000,
  Hint =          0x100000,
  Warn =          0x200000,
  Error =         0x300000,
  Fatal =         0x400000,
  Severity_Mask = 0xF00000, // includes reserved bits
  Error_Mask =    0x0FFFFF,
};

export enum CompilerErrorNamespace {
  /**
   * KMC errors between 0x0000…0x0FFF
   */
  Kmc = 0x0000,
  /**
   * common/web/types errors between 0x1000…0x1FFF
   */
  Types = 0x1000,
};

/**
 * Abstract interface for callbacks, to abstract out file i/o
 */
export interface CompilerCallbacks {
  loadFile(baseFilename: string, filename: string | URL): Buffer;
  loadLdmlKeyboardSchema(): Buffer;
  reportMessage(event: CompilerEvent): void;
  loadKvksJsonSchema(): Buffer;
};
