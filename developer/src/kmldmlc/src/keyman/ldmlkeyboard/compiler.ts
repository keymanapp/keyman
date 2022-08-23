
interface CompilerCallbacks {
  loadFile(baseFilename:string, filename: string): Buffer;
  reportMessage(severity: number, message: string): void;
};

export default class Compiler {
  private callbacks: CompilerCallbacks;
  constructor (callbacks: CompilerCallbacks) {
    this.callbacks = callbacks;

    //TEMP
    this.callbacks.reportMessage(0, "Instantiated compiler successfully");
  }

  public compile(filename: string): Buffer {
    // TODO
    return Buffer.from("KXTS..." + filename, 'utf8');
  }
}
