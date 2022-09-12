import * as xml2js from 'xml2js';
import LDMLKeyboardXMLSourceFile from './ldml-keyboard-xml';
import CompilerCallbacks from '../compiler/callbacks';
import Ajv from 'ajv';
import { CompilerMessages } from '../compiler/messages';

export default class LDMLKeyboardXMLSourceFileReader {
  private readonly callbacks: CompilerCallbacks;

  constructor (callbacks: CompilerCallbacks) {
    this.callbacks = callbacks;
  }

  /**
   * xml2js will not place single-entry objects into arrays.
   * Easiest way to fix this is to box them ourselves as needed
   * @param source any
   */
  private boxArrays(source: any) {
    let box = (o: any, x: string) => {
      if(typeof o == 'object' && !Array.isArray(o[x])) {
        if(o[x] === null || o[x] === undefined) {
          o[x] = [];
        }
        else {
          o[x] = [o[x]];
        }
      }
    }

    box(source?.keyboard, 'layerMaps');
    box(source?.keyboard?.names, 'name');
    box(source?.keyboard?.vkeyMaps, 'vkeyMap');
    box(source?.keyboard?.keys, 'key');
    box(source?.keyboard?.locales, 'locale');
    box(source?.keyboard, 'transforms');
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
    if(source?.keyboard?.transforms) {
      for(let transform of source.keyboard.transforms)  {
        box(transform, 'transform');
      }
    }
    box(source?.keyboard?.reorders, 'reorder');
    box(source?.keyboard?.backspaces, 'backspace');
    return source;
  }

  public validate(source: LDMLKeyboardXMLSourceFile): LDMLKeyboardXMLSourceFile {
    const schema = JSON.parse(this.callbacks.loadLdmlKeyboardSchema().toString('utf8'));
    const ajv = new Ajv();
    if(!ajv.validate(schema, source)) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidFile(ajv.errorsText()));
      return null;
    }
    return source;
  }

  public loadFile(filename: string) {
    const buf = this.callbacks.loadFile(filename, filename);
    return this.load(buf);
  }

  public load(file: Uint8Array): LDMLKeyboardXMLSourceFile {
    let source = (() => {
      let a: LDMLKeyboardXMLSourceFile;
      let parser = new xml2js.Parser({
        explicitArray: false,
        mergeAttrs: true,
        emptyTag: {}
      });
      parser.parseString(file, (e: unknown, r: unknown) => { a = r as LDMLKeyboardXMLSourceFile });
      return a;
    })();

    return this.validate(this.boxArrays(source));
  }
}