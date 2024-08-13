import * as xml2js from '../deps/xml2js/xml2js.js';
import KVKSourceFile, { KVKSEncoding, KVKSFlags, KVKSKey, KVKSLayer } from './kvks-file.js';
import { VisualKeyboard, VisualKeyboardHeaderFlags, VisualKeyboardKeyFlags, VisualKeyboardLegalShiftStates, VisualKeyboardShiftState } from './visual-keyboard.js';
import { USVirtualKeyCodes } from '../consts/virtual-key-constants.js';

export default class KVKSFileWriter {
  public write(vk: VisualKeyboard): string {

    const builder = new xml2js.Builder({
      allowSurrogateChars: true,
      attrkey: '$',
      charkey: '_',
      xmldec: {
        version: '1.0',
        encoding: 'UTF-8',
        standalone: true
      }
    })

    let flags: KVKSFlags = {};
    if(vk.header.flags & VisualKeyboardHeaderFlags.kvkhDisplayUnderlying) {
      flags.displayunderlying = '';
    }
    if(vk.header.flags & VisualKeyboardHeaderFlags.kvkh102) {
      flags.key102 = '';
    }
    if(vk.header.flags & VisualKeyboardHeaderFlags.kvkhAltGr) {
      flags.usealtgr = '';
    }
    if(vk.header.flags & VisualKeyboardHeaderFlags.kvkhUseUnderlying) {
      flags.useunderlying = '';
    }



    let kvks: KVKSourceFile = {
      visualkeyboard: {
        header: {
          version: '10.0',
          kbdname: vk.header.associatedKeyboard,
          flags: flags,
        },
        encoding: []
      }
    };

    if(vk.header.underlyingLayout) kvks.visualkeyboard.header.layout = vk.header.underlyingLayout;

    let encodings: {ansi: {o: KVKSEncoding, l: {[name:string]:KVKSLayer}}, unicode: {o: KVKSEncoding, l: {[name:string]:KVKSLayer}}} = {ansi:null,unicode:null};

    for(let key of vk.keys) {
      const encoding = key.flags & VisualKeyboardKeyFlags.kvkkUnicode ? 'unicode' : 'ansi';
      const shift = this.kvkShiftToKvksShift(key.shift);

      if(!encodings[encoding]) {
        encodings[encoding] = {
          o: {
            layer: [],
            $: {name: encoding,
            fontname: encoding == 'ansi' ? vk.header.ansiFont.name : vk.header.unicodeFont.name,
            fontsize: (encoding == 'ansi' ? vk.header.ansiFont.size : vk.header.unicodeFont.size).toString(),
            },
          },
          l: {}
        };
        kvks.visualkeyboard.encoding.push(encodings[encoding].o);
      }
      let e = encodings[encoding];
      if(!e.l[shift]) {
        e.l[shift] = {
          key: [],
          $: {shift: shift},
        };
        e.o.layer.push(e.l[shift]);
      }
      let l = e.l[shift];

      // TODO-LDML: map
      let vkeyName = '';
      for(let vkey of Object.keys(USVirtualKeyCodes)) {
        if((USVirtualKeyCodes as any)[vkey] == key.vkey) {
          vkeyName = vkey;
          break;
        }
      }

      if(vkeyName == '') {
        //TODO-LDML: warn
        continue;
      }
      let k: KVKSKey = {
        $: {vkey: vkeyName},
        _: key.text,
      }
      if(key.bitmap) {
        k.bitmap = this.arrayToBase64(key.bitmap);
      }

      l.key.push(k);
    }

    let result = builder.buildObject(kvks);
    return result; //Uint8Array.from(result);
  }

  private arrayToBase64(source: Uint8Array): string {
    const ascii = String.fromCharCode(...source);
    return btoa(ascii);
  }


  public kvkShiftToKvksShift(shift: VisualKeyboardShiftState): string {
    // TODO-LDML(lowpri): make a map of this?
    for(let state of VisualKeyboardLegalShiftStates) {
      if(state.shift == shift) {
        return state.name;
      }
    }
    return '';
  }
}