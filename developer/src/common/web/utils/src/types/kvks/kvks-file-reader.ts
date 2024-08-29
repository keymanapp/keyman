import { SchemaValidators as SV, KvkFile, util, Constants } from '@keymanapp/common-types';
import { XMLParser } from 'fast-xml-parser'
import KVKSourceFile from './kvks-file.js';
const SchemaValidators = SV.default;
import boxXmlArray = util.boxXmlArray;
import USVirtualKeyCodes = Constants.USVirtualKeyCodes;
import { VisualKeyboard as VK } from '@keymanapp/common-types';
import DEFAULT_KVK_FONT = VK.DEFAULT_KVK_FONT;
import VisualKeyboard = VK.VisualKeyboard;
import VisualKeyboardHeaderFlags = VK.VisualKeyboardHeaderFlags;
import VisualKeyboardKey = VK.VisualKeyboardKey;
import VisualKeyboardKeyFlags = VK.VisualKeyboardKeyFlags;
import VisualKeyboardLegalShiftStates = VK.VisualKeyboardLegalShiftStates;
import VisualKeyboardShiftState = VK.VisualKeyboardShiftState;
import BUILDER_KVK_HEADER_VERSION = KvkFile.BUILDER_KVK_HEADER_VERSION;
// import KVK_HEADER_IDENTIFIER_BYTES = KvkFile.KVK_HEADER_IDENTIFIER_BYTES;


export default class KVKSFileReader {
  public read(file: Uint8Array): KVKSourceFile {
    let source: KVKSourceFile;

    const parser = new XMLParser({
      // explicitArray: false,
      // mergeAttrs: false,
      // includeWhiteChars: true,
      // normalize: false,
      // emptyTag: {} as any
      // Why "as any"? xml2js is broken:
      // https://github.com/Leonidas-from-XIV/node-xml2js/issues/648 means
      // that an old version of `emptyTag` is used which doesn't support
      // functions, but DefinitelyTyped is requiring use of function or a
      // string. See also notes at
      // https://github.com/DefinitelyTyped/DefinitelyTyped/pull/59259#issuecomment-1254405470
      // An alternative fix would be to pull xml2js directly from github
      // rather than using the version tagged on npmjs.com.
    });

    source = parser.parse(file.toString()) as KVKSourceFile;
    // parser.parseString(file, (e: unknown, r: unknown) => {
    //   if(e) {
    // TODO:
        // if(file.byteLength > 4 && file.subarray(0,3).every((v,i) => v == KVK_HEADER_IDENTIFIER_BYTES[i])) {
        //   throw new Error('File appears to be a binary .kvk file', {cause: e});
        // }
      //   throw e;
      // };
    // });
    if(source) {
      source = this.boxArrays(source);
      this.cleanupFlags(source);
      this.cleanupUnderscore('visualkeyboard', source.visualkeyboard);
    }
    return source;
  }

  private cleanupFlags(source: any) {
    if(source.visualkeyboard?.header?.flags === '') {
      source.visualkeyboard.header.flags = {};
    }
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
    } else {
      // If key text is pure whitespace, replace with empty string,
      // which matches kmcomp reader
      if(source?.['_']?.match(/^( +)$/)) {
        source['_'] = '';
      }
    }

    for(const key of Object.keys(source)) {
      if(Array.isArray(source[key])) {
        for(const item of source[key]) {
          if(typeof(item) === 'object') {
            this.cleanupUnderscore(key, item);
          }
        }
      } else if(typeof source[key] === 'object') {
        this.cleanupUnderscore(key, source[key]);
      }
    }
  }

  public validate(source: KVKSourceFile): void {
    if(!SchemaValidators.kvks(source)) {
      throw new Error(JSON.stringify((<any>SchemaValidators.kvks).errors));
    }
  }

  public transform(source: KVKSourceFile, invalidVkeys?: string[]): VisualKeyboard {
    // NOTE: at this point, the xml should have been validated
    // and matched the schema result so we can assume properties exist

    const result: VisualKeyboard = {
      header: {
        version: BUILDER_KVK_HEADER_VERSION,
        flags: 0,
        ansiFont: {...DEFAULT_KVK_FONT},
        unicodeFont: {...DEFAULT_KVK_FONT},
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

    for(const encoding of source.visualkeyboard.encoding) {
      const isUnicode = (encoding.$?.name == 'unicode'),
        font = isUnicode ? result.header.unicodeFont : result.header.ansiFont;
      font.name = encoding.$?.fontname ?? DEFAULT_KVK_FONT.name;
      font.size = parseInt(encoding.$?.fontsize ?? DEFAULT_KVK_FONT.size.toString(), 10);
      for(const layer of encoding.layer) {
        const shift = this.kvksShiftToKvkShift(layer.$?.shift);
        for(const sourceKey of layer.key) {
          const vkey = (USVirtualKeyCodes as any)[sourceKey.$?.vkey];
          if(!vkey) {
            if(typeof invalidVkeys !== 'undefined') {
              invalidVkeys.push(sourceKey.$?.vkey);
            }
            continue;
          }
          const key: VisualKeyboardKey = {
            flags:
              (isUnicode ? VisualKeyboardKeyFlags.kvkkUnicode : 0) |
              (sourceKey.bitmap ? VisualKeyboardKeyFlags.kvkkBitmap : 0),
            shift: shift,
            text: sourceKey.bitmap ? '' : (sourceKey._ ?? ''),
            vkey: vkey
          };
          if(sourceKey.bitmap) {
            key.bitmap = this.base64ToArray(sourceKey.bitmap);
          }
          result.keys.push(key);
        }
      }
    }

    return result;
  }

  private base64ToArray(source: string): Uint8Array {
    const binary = atob(source);
    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) {
      bytes[i] = binary.charCodeAt(i);
    }
    return bytes;
  }

  /**
   * xml2js will not place single-entry objects into arrays.
   * Easiest way to fix this is to box them ourselves as needed
   * @param source KVKSourceFile
   */
  private boxArrays(source: KVKSourceFile) {
    boxXmlArray(source.visualkeyboard, 'encoding');
    for(const encoding of source.visualkeyboard.encoding) {
      boxXmlArray(encoding, 'layer');
      for(const layer of encoding.layer) {
        boxXmlArray(layer, 'key');
      }
    }
    return source;
  }


  public kvksShiftToKvkShift(shift: string): VisualKeyboardShiftState {
    shift = shift.toUpperCase();

    // TODO-LDML(lowpri): make a map of this?
    for(const state of VisualKeyboardLegalShiftStates) {
      if(state.name == shift) {
        return state.shift;
      }
    }
    return 0;
  }
}
