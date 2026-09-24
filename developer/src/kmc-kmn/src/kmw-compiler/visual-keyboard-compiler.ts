import { KvkFile, VisualKeyboard, visualKeyboardShiftToLayerName } from "@keymanapp/common-types";
import { FTabStop, nl } from "./compiler-globals.js";
import { CKeymanWebKeyCodes } from "./keymanweb-key-codes.js";
import { RequotedString } from "./kmw-compiler.js";

export function VisualKeyboardFromFile(visualKeyboard: VisualKeyboard.VisualKeyboard, debug: boolean): string {
  const f102 = visualKeyboard.header.flags & KvkFile.BUILDER_KVK_HEADER_FLAGS.kvkh102 ? '1' : '0';
  return `{F:' 1em "${RequotedString(visualKeyboard.header.unicodeFont.name, true)}"',K102:${f102}}` +
    `;` + VisualKeyboardToKLS(visualKeyboard) +
    ';' + BuildBKFromKLS(debug);
}

function WideQuote(s: string): string {
  let result = '';
  for(let i = 0; i < s.length; i++) {
    if(s[i] == '"' || s[i] == '\\') {
      result += '\\' + s[i];
    } else {
      result += s[i];
    }
  }
  return result;
}

function VisualKeyboardToKLS(FVK: VisualKeyboard.VisualKeyboard): string {

  interface TLayer {
    shift: number;
    name: string;
    keys: string[];
  };

  const layers: TLayer[] = [];

  // Discover the layers used in the visual keyboard
  for(const key of FVK.keys) {
    if(key.flags & KvkFile.BUILDER_KVK_KEY_FLAGS.kvkkUnicode) {
      // Find the index of the key in KMW VK arrays
      const n = CKeymanWebKeyCodes[key.vkey];
      if(n == 0xFF) {
        continue;
      }

      let layer = layers.find(layer => layer.shift == key.shift);
      if(!layer) {
        // 0-64 covers all possible VirtualKeyCodes in CKemyanWebKeyCodes
        layer = { shift: key.shift, name: '', keys: new Array(65)};
        layers.push(layer);
      }
      layer.keys[n] = key.text;
    }
  }

  // Build the layer array

  let result = nl+FTabStop+'this.KV.KLS={'+nl;

  for(let i = 0; i < layers.length; i++) {
    const layer = layers[i];
    result += `${FTabStop}${FTabStop}"${visualKeyboardShiftToLayerName(layer.shift)}": [`;
    for(let j = 0; j < layer.keys.length - 1; j++) {
      result += '"'+WideQuote(layer.keys[j] ?? '')+'",';
    }
    result += '"'+WideQuote(layer.keys[layer.keys.length-1] ?? '')+'"]';
    if(i < layers.length - 1) {
      result += ',' + nl;
    }
  }
  result += nl+FTabStop+'}';
  return result;
}

function BuildBKFromKLS(debug: boolean): string {
  const func =
    'function(x){var e=Array.apply(null,Array(65)).map(String.prototype.valueOf,"")'+
    ',r=[],v,i,m=[\'default\',\'shift\',\'ctrl\',\'shift-ctrl\',\'alt\',\'shift-alt\','+
    '\'ctrl-alt\',\'shift-ctrl-alt\'];for(i=m.length-1;i>=0;i--)if((v=x[m[i]])||r.length)'+
    'r=(v?v:e).slice().concat(r);return r}';
  const func_debug =
  'function(x){'+nl+
  '    var'+nl+
  '      empty=Array.apply(null, Array(65)).map(String.prototype.valueOf,""),'+nl+
  '      result=[], v, i,'+nl+
  '      modifiers=[\'default\',\'shift\',\'ctrl\',\'shift-ctrl\',\'alt\',\'shift-alt\',\'ctrl-alt\',\'shift-ctrl-alt\'];'+nl+
  '    for(i=modifiers.length-1;i>=0;i--) {'+nl+
  '      v = x[modifiers[i]];'+nl+
  '      if(v || result.length > 0) {'+nl+
  '        result=(v ? v : empty).slice().concat(result);'+nl+
  '      }'+nl+
  '    }'+nl+
  '    return result;'+nl+
  '  }';

  return nl+FTabStop+'this.KV.BK=('+(debug ? func_debug : func)+')(this.KV.KLS)';
}
