import { TouchLayout } from "@keymanapp/common-types";
import { VisualKeyboard } from "@keymanapp/common-types";
import { SchemaValidators } from "@keymanapp/common-types";

export interface StringRefUsage {
  filename: string;
  count: number;
};

export interface StringRef {
  str: string;
  usages: StringRefUsage[];
};

/**
 * Represents a single key cap found by `AnalyzeOskCharacterUse`
 */
export interface StringResult {
  /** the key cap string */
  str: string;
  /** unicode code points in <str> for reference, without 'U+' prefix, e.g. '0061 0301' */
  unicode: string;
  /** hexadecimal single character in PUA range, without 'U+' prefix, e.g. 'F100' */
  pua: string;
  /** files in which the string is referenced; will be an array of
   * {@link StringRefUsage} if includeCounts is true, otherwise will be an array
   * of strings listing files in which the key cap may be found */
  usages: StringRefUsage[] | string[];
};

export type PuaMap = {[index:string]: string};

export function parseMapping(mapping: any) {
  if(!SchemaValidators.default.displayMap(<any>mapping))
  /* c8 ignore next 3 */
  {
    throw new Error(JSON.stringify((<any>SchemaValidators.default.displayMap).errors));
  }

  const map: PuaMap = {};
  for (const item of mapping.map) {
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

export function remapVisualKeyboard(vk: VisualKeyboard.VisualKeyboard, map: PuaMap): boolean {
  let dirty = false;
  for(const key of vk.keys) {
    if(!key.text) {
      continue;
    }
    const text = remap(key.text, map);
    dirty = dirty || text != key.text;
    key.text = text;
  }
  return dirty;
}

export function remapTouchLayout(source: TouchLayout.TouchLayoutFile, map: PuaMap) {
  let dirty = false;
  const scanKey = (key: TouchLayout.TouchLayoutKey | TouchLayout.TouchLayoutSubKey) => {
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

  const scanPlatform = (platform: TouchLayout.TouchLayoutPlatform) => {
    if(!platform) {
      return;
    }
    for(const layer of platform.layer) {
      for(const row of layer.row) {
        for(const key of row.key) {
          scanKey(key);
          let f: keyof TouchLayout.TouchLayoutFlick;
          for(f in key.flick ?? {}) {
            scanKey(key.flick[f]);
          }
          for(const sk of key.sk ?? []) {
            scanKey(sk);
          }
          for(const mt of key.multitap ?? []) {
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
