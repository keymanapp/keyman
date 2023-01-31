/**
 * Abstract interface for compiler error and warning messages
 */
export interface CompilerEvent {
  code: number;
  message: string;
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
