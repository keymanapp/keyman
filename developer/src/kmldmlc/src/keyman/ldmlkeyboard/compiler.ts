import KMXBuilder from '../kmx/kmx-builder';
import KMXPlusFile from '../kmx/kmx-plus';

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
    let kmx = new KMXPlusFile();

    // Load the source XML file into an array

    /*let buf =*/ this.callbacks.loadFile(filename, filename);

    // TODO: create a LDMLXMLSourceFile from buf

    /*
    kmx.kmxplus.meta.name = 'Sample Keyboard';
    kmx.kmxplus.meta.author = 'Marc Durdin';
    kmx.kmxplus.loca.locales.push('en');
    kmx.kmxplus.keys.keys.push({
      vkey: 65,
      mod: 0,
      to: "Hello!",
      flags: 0
    });
    */

    // TODO: transform LDMLXMLSourceFile to the kmx data

    // Use the builder to generate the binary output file
    let builder = new KMXBuilder(kmx, true);
    return builder.compile();
  }
}
