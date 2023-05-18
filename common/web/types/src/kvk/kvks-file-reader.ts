import * as xml2js from 'xml2js';
import KVKSourceFile from './kvks-file.js';
import { default as AjvModule } from 'ajv';
const Ajv = AjvModule.default; // The actual expected Ajv type.
import { boxXmlArray } from '../util/util.js';
import { VisualKeyboard, VisualKeyboardHeaderFlags, VisualKeyboardKey, VisualKeyboardKeyFlags, VisualKeyboardLegalShiftStates, VisualKeyboardShiftState } from './visual-keyboard.js';
import { USVirtualKeyCodes } from '../consts/virtual-key-constants.js';
import { BUILDER_KVK_HEADER_VERSION } from './kvk-file.js';

export enum KVKSParseErrorType { invalidVkey };
export class KVKSParseError extends Error {
  public type: KVKSParseErrorType;
  public vkey: string;
};

export default class KVKSFileReader {
  public read(file: Uint8Array): KVKSourceFile {
    let source: KVKSourceFile;

    const parser = new xml2js.Parser({
      explicitArray: false,
      mergeAttrs: false,
      includeWhiteChars: true,
      normalize: false,
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

    parser.parseString(file, (e: unknown, r: unknown) => { source = r as KVKSourceFile });
    source = this.boxArrays(source);
    this.cleanupUnderscore('visualkeyboard', source.visualkeyboard);
    return source;
  }

  /**
   * The only element that allows spaces is <key>. Remove
   * all other empty whitespace-only values.
   * @param root
   * @param source
   */
  private cleanupUnderscore(root: string, source: any) {
    if(root != 'key') {
      if(source?.['_']?.trim() === '') {
        delete source['_'];
      }
    }

    for(let key of Object.keys(source)) {
      if(Array.isArray(source[key])) {
        for(let item of source[key]) {
          if(typeof(item) === 'object') {
            this.cleanupUnderscore(key, item);
          }
        }
      } else if(typeof source[key] === 'object') {
        this.cleanupUnderscore(key, source[key]);
      }
    }
  }

  public validate(source: KVKSourceFile, schemaBuffer: Buffer): void {
    const schema = JSON.parse(schemaBuffer.toString('utf8'));
    const ajv = new Ajv();
    if(!ajv.validate(schema, source)) {
      throw new Error(ajv.errorsText());
    }
  }

  public transform(source: KVKSourceFile, errors?: KVKSParseError[]): VisualKeyboard {
    // NOTE: at this point, the xml should have been validated
    // and matched the schema result so we can assume properties exist
    let result: VisualKeyboard = {
      header: {
        version: BUILDER_KVK_HEADER_VERSION,
        flags: 0,
        ansiFont: { name: "Arial", size: -12, color: 0xFF000008 }, // TODO-LDML: consider defaults
        unicodeFont: { name: "Arial", size: -12, color: 0xFF000008 }, // TODO-LDML: consider defaults
        associatedKeyboard: source.visualkeyboard?.header?.kbdname,
        underlyingLayout: source.visualkeyboard?.header?.layout,
      },
      keys: []
    };

    if(source.visualkeyboard?.header?.flags?.displayunderlying !== undefined) {
      result.header.flags |= VisualKeyboardHeaderFlags.kvkhDisplayUnderlying;
    }
    if(source.visualkeyboard?.header?.flags?.key102 !== undefined) {
      result.header.flags |= VisualKeyboardHeaderFlags.kvkh102;
    }
    if(source.visualkeyboard?.header?.flags?.usealtgr !== undefined) {
      result.header.flags |= VisualKeyboardHeaderFlags.kvkhAltGr;
    }
    if(source.visualkeyboard?.header?.flags?.useunderlying !== undefined) {
      result.header.flags |= VisualKeyboardHeaderFlags.kvkhUseUnderlying;
    }

    for(let encoding of source.visualkeyboard.encoding) {
      let isUnicode = (encoding.$?.name == 'unicode'),
        font = isUnicode ? result.header.unicodeFont : result.header.ansiFont;
      font.name = encoding.$?.fontname;
      font.size = parseInt(encoding.$?.fontsize,10);
      for(let layer of encoding.layer) {
        let shift = this.kvksShiftToKvkShift(layer.$?.shift);
        for(let sourceKey of layer.key) {
          let vkey = (USVirtualKeyCodes as any)[sourceKey.$?.vkey];
          if(!vkey) {
            if(errors) {
              let e = new KVKSParseError();
              e.type = KVKSParseErrorType.invalidVkey;
              e.vkey = sourceKey.$?.vkey;
              errors.push(e);
            }
            continue;
          }
          let key: VisualKeyboardKey = {
            flags: isUnicode ? VisualKeyboardKeyFlags.kvkkUnicode : 0, // TODO-LDML: bitmap support
            shift: shift,
            text: sourceKey._ ?? '',
            vkey: vkey
          }
          result.keys.push(key);
        }
      }
    }

    return result;
  }

  /**
   * xml2js will not place single-entry objects into arrays.
   * Easiest way to fix this is to box them ourselves as needed
   * @param source KVKSourceFile
   */
  private boxArrays(source: KVKSourceFile) {
    boxXmlArray(source.visualkeyboard, 'encoding');
    for(let encoding of source.visualkeyboard.encoding) {
      boxXmlArray(encoding, 'layer');
      for(let layer of encoding.layer) {
        boxXmlArray(layer, 'key');
      }
    }
    return source;
  }


  public kvksShiftToKvkShift(shift: string): VisualKeyboardShiftState {
    shift = shift.toUpperCase();

    // TODO-LDML(lowpri): make a map of this?
    for(let state of VisualKeyboardLegalShiftStates) {
      if(state.name == shift) {
        return state.shift;
      }
    }
    return 0;
  }
}