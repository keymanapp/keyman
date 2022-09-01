export interface CompilerEvent {
  code: number;
  message: string;
};

export default interface CompilerCallbacks {
  loadFile(baseFilename: string, filename: string): Buffer;
  loadLdmlKeyboardSchema(): Buffer;
  reportMessage(event: CompilerEvent): void;
};
