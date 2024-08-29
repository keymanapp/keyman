import { VisualKeyboard as VK, Constants } from '@keymanapp/common-types';
import KVKSourceFile, { KVKSEncoding, KVKSFlags, KVKSKey, KVKSLayer } from './kvks-file.js';
import { XMLBuilder } from 'fast-xml-parser';

import USVirtualKeyCodes = Constants.USVirtualKeyCodes;
import VisualKeyboard = VK.VisualKeyboard;
import VisualKeyboardHeaderFlags = VK.VisualKeyboardHeaderFlags;
import VisualKeyboardKeyFlags = VK.VisualKeyboardKeyFlags;
import VisualKeyboardLegalShiftStates = VK.VisualKeyboardLegalShiftStates;
import VisualKeyboardShiftState = VK.VisualKeyboardShiftState;

export default class KVKSFileWriter {
  public write(vk: VisualKeyboard): string {

    const builder = new XMLBuilder({
      // allowSurrogateChars: true,
      // attrkey: '$',
      // charkey: '_',
      // xmldec: {
      //   version: '1.0',
      //   encoding: 'UTF-8',
      //   standalone: true
      // }
    });


    const flags: KVKSFlags = {};
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



    const kvks: KVKSourceFile = {
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

    const encodings: {ansi: {o: KVKSEncoding, l: {[name:string]:KVKSLayer}}, unicode: {o: KVKSEncoding, l: {[name:string]:KVKSLayer}}} = {ansi:null,unicode:null};

    for(const key of vk.keys) {
      const encoding = key.flags & VisualKeyboardKeyFlags.kvkkUnicode ? 'unicode' : 'ansi';
      const shift = this.kvkShiftToKvksShift(key.shift);

      if(!encodings[encoding]) {
        encodings[encoding] = {
          o: {
            layer: [],
            name: encoding,
            fontname: encoding == 'ansi' ? vk.header.ansiFont.name : vk.header.unicodeFont.name,
            fontsize: (encoding == 'ansi' ? vk.header.ansiFont.size : vk.header.unicodeFont.size).toString(),
          },
          l: {}
        };
        kvks.visualkeyboard.encoding.push(encodings[encoding].o);
      }
      const e = encodings[encoding];
      if(!e.l[shift]) {
        e.l[shift] = {
          key: [],
          shift: shift,
        };
        e.o.layer.push(e.l[shift]);
      }
      const l = e.l[shift];

      // TODO-LDML: map
      let vkeyName = '';
      for(const vkey of Object.keys(USVirtualKeyCodes)) {
        if((USVirtualKeyCodes as any)[vkey] == key.vkey) {
          vkeyName = vkey;
          break;
        }
      }

      if(vkeyName == '') {
        //TODO-LDML: warn
        continue;
      }
      const k: KVKSKey = {
        vkey: vkeyName,
        '#text': key.text,
      }
      if(key.bitmap) {
        k.bitmap = this.arrayToBase64(key.bitmap);
      }

      l.key.push(k);
    }

    const result = builder.build(kvks);
    return result; //Uint8Array.from(result);
  }

  private arrayToBase64(source: Uint8Array): string {
    const ascii = String.fromCharCode(...source);
    return btoa(ascii);
  }


  public kvkShiftToKvksShift(shift: VisualKeyboardShiftState): string {
    // TODO-LDML(lowpri): make a map of this?
    for(const state of VisualKeyboardLegalShiftStates) {
      if(state.shift == shift) {
        return state.name;
      }
    }
    return '';
  }
}
