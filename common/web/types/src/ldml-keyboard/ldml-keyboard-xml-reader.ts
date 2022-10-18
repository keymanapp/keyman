import * as xml2js from 'xml2js';
import { LDMLKeyboardXMLSourceFile } from './ldml-keyboard-xml.js';
import Ajv from 'ajv';
import { boxXmlArray } from '../util/util.js';

export default class LDMLKeyboardXMLSourceFileReader {
  /**
   * xml2js will not place single-entry objects into arrays.
   * Easiest way to fix this is to box them ourselves as needed
   * @param source any
   */
  private boxArrays(source: any) {
    boxXmlArray(source?.keyboard, 'layers');
    boxXmlArray(source?.keyboard?.names, 'name');
    boxXmlArray(source?.keyboard?.vkeys, 'vkey');
    boxXmlArray(source?.keyboard?.keys, 'key');
    boxXmlArray(source?.keyboard?.locales, 'locale');
    boxXmlArray(source?.keyboard, 'transforms');
    if(source?.keyboard?.layers) {
      for(let layers of source?.keyboard?.layers) {
        boxXmlArray(layers, 'layer');
        if(layers?.layer) {
          for(let layer of layers?.layer) {
            boxXmlArray(layer, 'row');
          }
        }
      }
    }
    if(source?.keyboard?.transforms) {
      for(let transform of source.keyboard.transforms)  {
        boxXmlArray(transform, 'transform');
      }
    }
    boxXmlArray(source?.keyboard?.reorders, 'reorder');
    return source;
  }

  public validate(source: LDMLKeyboardXMLSourceFile, schemaSource: Buffer): void {
    const schema = JSON.parse(schemaSource.toString('utf8'));
    const ajv = new Ajv();
    if(!ajv.validate(schema, source)) {
      throw new Error(ajv.errorsText());
    }
  }

  public load(file: Uint8Array): LDMLKeyboardXMLSourceFile {
    let source = (() => {
      let a: LDMLKeyboardXMLSourceFile;
      let parser = new xml2js.Parser({
        explicitArray: false,
        mergeAttrs: true,
        includeWhiteChars: false,
        emptyTag: {} as any
        // Why "as any"? xml2js is broken:
        // https://github.com/Leonidas-from-XIV/node-xml2js/issues/648 means
        // that an old version of `emptyTag` is used which doesn't support
        // functions, but DefinitelyTyped is requiring use of function or a
        // string. See also notes at
        // https://github.com/DefinitelyTyped/DefinitelyTyped/pull/59259#issuecomment-1254405470
        // An alternative fix would be to pull xml2js directly from github
        // rather than using the version tagged on npmjs.com.
      });
      parser.parseString(file, (e: unknown, r: unknown) => { a = r as LDMLKeyboardXMLSourceFile });
      return a;
    })();

    return this.boxArrays(source);
  }
}
