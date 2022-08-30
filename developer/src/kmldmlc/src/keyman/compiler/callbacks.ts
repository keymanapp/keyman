export default interface CompilerCallbacks {
  loadFile(baseFilename: string, filename: string): Buffer;
  reportMessage(code: number, message: string): void;
};
