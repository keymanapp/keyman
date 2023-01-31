import { LKKey, LKLayers } from "@keymanapp/common-types/src/ldml-keyboard/ldml-keyboard-xml";

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
export function calculateUniqueKeys(keys?: LKKey[]): LKKey[] {
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
export function allUsedKeyIdsInLayers(layersList : LKLayers[] | null): Set<string> {
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
