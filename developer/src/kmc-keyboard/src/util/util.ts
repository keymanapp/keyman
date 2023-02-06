import { LDMLKeyboard } from "@keymanapp/common-types";
import { constants } from "@keymanapp/ldml-keyboard-constants";
/**
 * Verifies that value is an item in the enumeration.
 */
export function isValidEnumValue<T extends {[key: number]: string | number}>(enu: T, value: string) {
  return (Object.values(enu) as string[]).includes(value);
}

/**
 * Returns unique LKKeys only, preserving later ones
 * in case of conflict. (i.e. later overrides)
 * @param keys list of keys to consider. (mutated)
 * @returns Array of unique keys. Order is not specified.
 */
export function calculateUniqueKeys(keys?: LDMLKeyboard.LKKey[]): LDMLKeyboard.LKKey[] {
  if (!keys) {
    return [];
  }
  // Need 'newer' (later) keys to override older ones.
  const reverseKeys = keys.reverse(); // newest to oldest
  const alreadySeen = new Set<string>();
  // filter out only the keys that haven't already been seen
  const uniqueKeys = reverseKeys.filter(({ id }) => {
    if (!alreadySeen.has(id)) {
      alreadySeen.add(id);
      return true;
    }
    return false;
  });

  return uniqueKeys;
}

/**
 *
 * @param layersList list of layers elements, from `keyboard?.layers`
 * @returns set of key IDs
 */
export function allUsedKeyIdsInLayers(layersList : LDMLKeyboard.LKLayers[] | null): Set<string> {
  const s = new Set<string>();
  if (layersList) {
    for (const layers of layersList || []) {
      for (const layer of layers.layer || []) {
        for (const row of layer.row || []) {
          if (row.keys) {
            for (const k of row.keys.split(" ")) {
              s.add(k);
            }
          }
        }
      }
    }
  }
  return s;
}

/**
 * Determine modifier from layer info
 * @param layer layer obj
 * @returns modifier
 */
export function translateLayerAttrToModifier(layer: LDMLKeyboard.LKLayer) : number {
  const { modifier } = layer;
  if (modifier) {
    let mod = constants.keys_mod_none;
    for (let str of modifier.split(' ')) {
      const submod = constants.keys_mod_map.get(str);
      mod |= submod;
    }
    return mod;
  }
  // TODO-LDML: other modifiers, other ids?
  return constants.keys_mod_none;
}
