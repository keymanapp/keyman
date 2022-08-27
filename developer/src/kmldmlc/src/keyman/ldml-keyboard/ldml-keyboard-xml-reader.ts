import * as xml2js from 'xml2js';
import LDMLKeyboardXMLSourceFile from './ldml-keyboard-xml';
import CompilerCallbacks from '../compiler/callbacks';

export default class LDMLKeyboardXMLSourceFileReader {

  //private callbacks: CompilerCallbacks;
  constructor (callbacks: CompilerCallbacks) {
    //this.callbacks = callbacks;
  }

  /**
   * xml2js will not place single-entry objects into arrays.
   * Easiest way to fix this is to box them ourselves as needed
   * @param source any
   */
  private boxArrays(source: any) {
    let box = (o: any, x: string) => {
      if(typeof o == 'object' && !Array.isArray(o[x])) o[x] = [o[x]];
    }

    box(source?.keyboard, 'layerMaps');
    box(source?.keyboard?.names, 'name');
    box(source?.keyboard?.keys, 'key');
    if(source?.keyboard?.layerMaps) {
      for(let layerMaps of source?.keyboard?.layerMaps) {
        box(layerMaps, 'layerMap');
        if(layerMaps?.layerMap) {
          for(let layerMap of layerMaps?.layerMap) {
            box(layerMap, 'row');
          }
        }
      }
    }
    return source;
  }

  public load(file: Uint8Array): LDMLKeyboardXMLSourceFile {
    let source = (() => {
      let a: LDMLKeyboardXMLSourceFile;
      let parser = new xml2js.Parser({
        explicitArray: false,
        mergeAttrs: true,
      });
      parser.parseString(file, (e: unknown, r: unknown) => { a = r as LDMLKeyboardXMLSourceFile });
      return a;
    })();

    return this.boxArrays(source);
  }
}