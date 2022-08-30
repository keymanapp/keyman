import KMXBuilder from '../kmx/kmx-builder';
import KMXPlusFile from '../kmx/kmx-plus';
import LDMLKeyboardXMLSourceFile from '../ldml-keyboard/ldml-keyboard-xml';
import LDMLKeyboardXMLSourceFileReader from '../ldml-keyboard/ldml-keyboard-xml-reader';
import CompilerCallbacks from './callbacks';
import { KeysCompiler } from './keys';
import { LocaCompiler } from './loca';
import { MetaCompiler } from './meta';

export default class Compiler {
  private callbacks: CompilerCallbacks;
  constructor (callbacks: CompilerCallbacks) {
    this.callbacks = callbacks;

    //TEMP
    this.callbacks.reportMessage(0, "Instantiated compiler successfully");
  }

  /**
   * Loads a LDML Keyboard xml file and compiles into in-memory kmxplus
   * structures.
   * @param filename  input filename, will use callback to load from disk
   * @returns
   */
  public load(filename: string): LDMLKeyboardXMLSourceFile {
    let buf = this.callbacks.loadFile(filename, filename);
    let reader = new LDMLKeyboardXMLSourceFileReader(this.callbacks);
    let source = reader.load(buf);
    if(!source) {
      return null;
    }

    // console.dir(source, {depth:15});

    return source;
  }

  /**
   * Validates that the LDML keyboard source file and lints. If this returns true,
   * it is safe to pass `source` to .compile.
   * @param source
   * @returns
   */
  public validate(source: LDMLKeyboardXMLSourceFile): boolean {
    /*
    let validator: LDMLKeyboardValidator = new LDMLKeyboardValidator(this.callbacks);
    if(!validator.validate(xml)) {
      return null;
    }*/
    return true;
  }

  /**
   * Transforms in-memory LDML keyboard xml file to an intermediate representation
   * of a .kmx file. Input source variable should have already passed validation.
   * @param   source  in-memory representation of LDML keyboard xml file
   * @returns         KMXPlusFile intermediate file
   */
  public compile(source: LDMLKeyboardXMLSourceFile): KMXPlusFile {
    let kmx = new KMXPlusFile();

    // Transform source xml structures to kmxplus

    (new MetaCompiler(kmx, source, this.callbacks)).execute();
    (new LocaCompiler(kmx, source, this.callbacks)).execute();
    (new KeysCompiler(kmx, source, this.callbacks)).execute();

    // TODO: generate vkey mapping for touch-only keys

    return kmx;
  }

  public write(kmx: KMXPlusFile): Uint8Array {
    // Use the builder to generate the binary output file
    let builder = new KMXBuilder(kmx, true);
    return builder.compile();
  }
}

