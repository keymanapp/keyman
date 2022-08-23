import KMXFile from '../kmx/kmx';
import KMXBuilder from '../kmx/kmx-builder';

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

  public compile(filename: string): Uint8Array {
    let kmx = new KMXFile();

    // Load the source XML file into an array

    /*let buf =*/ this.callbacks.loadFile(filename, filename);

    // TODO: create a LDMLXMLSourceFile from buf

    // TODO: transform LDMLXMLSourceFile to the kmx data

    // Use the builder to generate the binary output file
    let builder = new KMXBuilder(kmx);
    return builder.compile();
  }
}
