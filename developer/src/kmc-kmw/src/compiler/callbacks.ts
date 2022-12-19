export interface CompilerEvent {
  line: number;
  code: number;
  message: string;
};

export default interface CompilerCallbacks {
  loadFile(baseFilename: string, filename: string): Buffer;
  loadLdmlKeyboardSchema(): Buffer;
  loadKvksJsonSchema(): Buffer;
  reportMessage(event: CompilerEvent): void;
};
