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

/** Convert an array of keys to a hash */
export function hashKeys(keys?: LDMLKeyboard.LKKey[]) : Map<string, LDMLKeyboard.LKKey> {
  const m = new Map<string, LDMLKeyboard.LKKey>();
  for (const k of keys ?? []) {
    m.set(k.id, k);
  }
  return m;
}

export function hashFlicks(flicks?: LDMLKeyboard.LKFlick[]) : Map<string, LDMLKeyboard.LKFlick> {
  const m = new Map<string, LDMLKeyboard.LKFlick>();
  for (const k of flicks ?? []) {
    m.set(k.id, k);
  }
  return m;
}


/**
 * @param layersList list of layers elements, from `keyboard?.layers`
 * @returns set of key IDs
 */
export function allUsedKeyIdsInLayers(layersList?: LDMLKeyboard.LKLayers[]): Set<string> {
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
 * Extract all of the key ids that this key refers to, not counting flicks
 * @returns map from the key id to a set of attribute names (such as `multiTapDefaultKeyId` etc.) used by that key.
 */
export function allUsedKeyIdsInKey(key : LDMLKeyboard.LKKey) : Map<string, string[]> {
  const m = new Map<string, string[]>();
  /** add one key */
  function addKey(keyId : string | undefined, attr: string) {
    if (!keyId) return;
    if (!m.has(keyId)) m.set(keyId,[]);
    m.get(keyId).push(attr);
  }
  /** add a set of keys */
  function addKeys(keyIds : string | undefined, attr: string) {
    if (!keyIds) return;
    for (const keyId of keyIds?.split(' ')) {
      addKey(keyId, attr);
    }
  }

  const {longPressKeyIds, longPressDefaultKeyId, multiTapKeyIds} = key;

  addKey(longPressDefaultKeyId, 'longPressDefaultKeyId');
  addKeys(longPressKeyIds, 'longPressKeyIds');
  addKeys(multiTapKeyIds, 'multiTapKeyIds');

  return m;
}

export function allUsedKeyIdsInFlick(flick? : LDMLKeyboard.LKFlick) : Set<string> {
  const s = new Set<string>();
  if(flick) {
    for (const {keyId} of flick.flickSegment) {
      s.add(keyId);
    }
  }
  return s;
}

/**
 * Helper function for validating child elements. Written for the convenience of message passing functions.
 *
 * @param values array of values to check
 * @param onDuplicate callback with array of duplicate values, deduped
 * @param allowed optional set of valid values
 * @param onInvalid callback with array of invalid values, deduped
 * @returns true if all OK
 */
export function verifyValidAndUnique(
  values: string[],
  onDuplicate: (duplicates: string[]) => void,
  allowed?: Set<string>,
  onInvalid?: (invalids: string[]) => void)
  : boolean {
  const dups: string[] = [];
  const invalids: string[] = [];
  const seen = new Set<string>();
  for (const value of values) {
    if (allowed && !allowed.has(value)) {
      invalids.push(value);
    }
    if (seen.has(value)) {
      dups.push(value);
    } else {
      seen.add(value);
    }
  }

  function dedupedSortedArray(values: string[]) : string[] {
    return Array.from(new Set(values)).sort();
  }

  if (dups.length > 0 && onDuplicate) {
    onDuplicate(dedupedSortedArray(dups));
  }
  if (invalids.length > 0 && onInvalid) {
    onInvalid(dedupedSortedArray(invalids));
  }
  return (!dups.length && !invalids.length);
}

/**
 * Determine modifier from layer info
 * @param layer layer obj
 * @returns modifier array
 */
export function translateLayerAttrToModifier(layer: LDMLKeyboard.LKLayer) : number[] {
  const { modifiers } = layer;
  if (!modifiers) return [constants.keys_mod_none];
  return modifiers.split(',').map(m => translateModifierSubsetToLayer(m)).sort();
}

function translateModifierSubsetToLayer(modifiers: string) : number {
  // TODO-LDML: Default #11072
  if (modifiers) {
    if (modifiers.indexOf(',') !== -1) {
      throw Error(`translateModifierSubsetToLayer only takes a single subset of the modifiers`);
    }
    let mod = constants.keys_mod_none;
    for (let str of modifiers.split(' ')) {
      const submod = constants.keys_mod_map.get(str);
      mod |= submod;
    }
    return mod;
  }
  // TODO-LDML: other modifiers, other ids?
  return constants.keys_mod_none;
}

/**
 * @param modifier modifier sequence such as undefined, "none", "shift altR" etc
 * @returns true if valid
 */
export function validModifier(modifier?: string) : boolean {
  if (!modifier) return true;  // valid to have no modifier, == none
  // TODO-LDML: enforce illegal combinations per spec.
  for (let sub of modifier.trim().split(',')) {
    for (let str of sub.trim().split(' ')) {
      if (!constants.keys_mod_map.has(str)) {
        return false;
      }
    }
  }
  return true;
}


