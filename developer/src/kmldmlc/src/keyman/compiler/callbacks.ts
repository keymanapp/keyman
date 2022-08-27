export default interface CompilerCallbacks {
  loadFile(baseFilename: string, filename: string): Buffer;
  reportMessage(severity: number, message: string): void;
};
