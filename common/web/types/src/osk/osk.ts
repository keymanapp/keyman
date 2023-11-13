import { TouchLayoutFile, TouchLayoutFlick, TouchLayoutKey, TouchLayoutPlatform, TouchLayoutSubKey } from "src/keyman-touch-layout/keyman-touch-layout-file.js";
import { VisualKeyboard } from "../kvk/visual-keyboard.js";
import SchemaValidators from "../schema-validators.js";

export interface StringRefUsage {
  filename: string;
  count: number;
};

export interface StringRef {
  str: string;
  usages: StringRefUsage[];
};

export interface StringResult {
  str: string;                         // the key cap string
  unicode: string;                     // unicode code points in <str> for reference
  pua: string;                         // hexadecimal single character in PUA range
  usages: StringRefUsage[] | string[]; // files in which the string is referenced
};

export type PuaMap = {[index:string]: string};

export function parseMapping(mapping: any) {
  if(!SchemaValidators.displayMap(<any>mapping))
  /* c8 ignore next 3 */
  {
    throw new Error(JSON.stringify((<any>SchemaValidators.displayMap).errors));
  }

  let map: PuaMap = {};
  for (let item of mapping.map) {
    map[item.str] = String.fromCharCode(parseInt(item.pua, 16));
  }
  return map;
}

function cleanString(s: string): string {
  s = s.replace(/\u25cc/g, '');
  return s.trim();
}

function remap(text: string, map: PuaMap) {
  if(map[text.trim()]) {
    return map[text.trim()];
  }
  text = cleanString(text);
  if(map[text]) {
    return map[text];
  }
  return text;
}

export function remapVisualKeyboard(vk: VisualKeyboard, map: PuaMap): boolean {
  let dirty = false;
  for(let key of vk.keys) {
    if(!key.text) {
      continue;
    }
    const text = remap(key.text, map);
    dirty = dirty || text != key.text;
    key.text = text;
  }
  return dirty;
}

export function remapTouchLayout(source: TouchLayoutFile, map: PuaMap) {
  let dirty = false;
  const scanKey = (key: TouchLayoutKey | TouchLayoutSubKey) => {
    if(!key.text) {
      return;
    }
    if(key.text.length > 2 && key.text[0] == '*' && key.text[key.text.length-1] == '*') {
      // Don't touch '*special*' key captions
      return;
    }
    const text = remap(key.text, map);
    dirty = dirty || text != key.text;
    key.text = text;
  }

  const scanPlatform = (platform: TouchLayoutPlatform) => {
    if(!platform) {
      return;
    }
    for(let layer of platform.layer) {
      for(let row of layer.row) {
        for(let key of row.key) {
          scanKey(key);
          let f: keyof TouchLayoutFlick;
          for(f in key.flick ?? {}) {
            scanKey(key.flick[f]);
          }
          for(let sk of key.sk ?? []) {
            scanKey(sk);
          }
          for(let mt of key.multitap ?? []) {
            scanKey(mt);
          }
        }
      }
    }
  }
  scanPlatform(source.desktop);
  scanPlatform(source.phone);
  scanPlatform(source.tablet);

  return dirty;
}