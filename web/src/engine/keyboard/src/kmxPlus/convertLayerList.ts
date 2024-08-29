import { Layer, LayerList } from './layr.js';
import { TouchLayout } from '@keymanapp/common-types';
import TouchLayoutLayer = TouchLayout.TouchLayoutLayer;
import TouchLayoutRow = TouchLayout.TouchLayoutRow;
import TouchLayoutKey = TouchLayout.TouchLayoutKey;
import TouchLayoutSubKey = TouchLayout.TouchLayoutSubKey;
import TouchLayoutFlick = TouchLayout.TouchLayoutFlick;

import Codes from '../codes.js';
import { KeySpec, Keys } from './keys.js';
import { ButtonClasses } from '../keyboards/defaultLayouts.js';

const keyNames = Object.keys(Codes.keyCodes);

export function convertLayerList(layerList: LayerList, keys: Keys) {
  return layerList.layers.map((layer) => convertLayer(layer, keys));
}

function nameFromModifier(code: number) {
  const modNames = Object.keys(Codes.modifierCodes);

  for(let mod of modNames) {
    if(Codes.modifierCodes[mod] == code) {
      return mod.toLowerCase();
    }
  }

  return '';
}

export function convertLayer(layer: Layer, keys: Keys): TouchLayoutLayer {
  const modName = nameFromModifier(layer.modifiers);
  const name = layer.id == '' ? modName : layer.id;

  const rows: TouchLayoutRow[] = layer.keys.map((row, index) => {
    return {
      id: index,
      key: row.map((keySpec) => convertKey(keySpec, keys, modName))
      //
    } as TouchLayoutRow
  });

  // TODO:  add frame keys as needed
  // like shift, etc.

  return {
    id: name,
    row: rows
  };
}

export function convertKey(keySpec: KeySpec, keys: Keys, defaultModifier: string): TouchLayoutKey | TouchLayoutSubKey {
  if(keySpec.isGap) {
    return {
      sp: ButtonClasses.spacer,
      width: keySpec.width
    }
  }

  let id = keySpec.id;
  for(let keyId of keyNames) {
    if(Codes.keyCodes[keyId] == keySpec.keyCode) {
      id = keyId;
    }
  }

  // TODO:  if unmatched, we need a synthetic key ID (or similar) for
  // custom keys.
  //
  // MD recommendation:  build a `T_` id for such keys, keeping them unique.
  // May need a table with matching codes (per JS-keyboard VKDictionary) to
  // use within Core as a numeric key ID for rules... or maybe the string itself
  // will work within Core?  I leave that to y'all.

  let obj: TouchLayoutKey = {
    id: id as unknown as any, // may not actually match Keyman keyboard id restrictions!
    text: keySpec.to, // TODO:  Look up correct value from `disp` table.
    width: keySpec.width
    //
  };

  if(keySpec.switch) {
    obj.nextlayer = keySpec.switch;
  }

  if(keySpec.longpress && keySpec.longpress.length > 0) {
    obj.sk = keySpec.longpress.map((id) => convertKey(keys.keys.get(id), keys, defaultModifier) as TouchLayoutSubKey);
  }

  if(keySpec.multitap && keySpec.multitap.length > 0) {
    obj.multitap = keySpec.multitap.map((id) => convertKey(keys.keys.get(id), keys, defaultModifier) as TouchLayoutSubKey);
  }

  if(keySpec.flicks) {
    const flickObj: TouchLayoutFlick = {};

    for(let flick of keySpec.flicks) {
      if(flick.dirSequence.length == 1) {
        const dir = flick.dirSequence[0] as keyof TouchLayoutFlick;
        const key = convertKey(keys.keys.get(flick.keyId), keys, defaultModifier) as TouchLayoutSubKey;

        flickObj[dir] = key;
      }
      // else skip - we don't support multisegment flicks.
    }

    obj.flick = flickObj;
  }

  // Should match the modifier
  if(keySpec.modifiers) {
    // [JH] Wait... but the key always has a modifier value on it... right?
    // I don't see where the KMX+ spec allows that to be undefined.
    obj.layer = keySpec.modifiers !== undefined ? nameFromModifier(keySpec.modifiers) : defaultModifier;
  }

  return obj;
}