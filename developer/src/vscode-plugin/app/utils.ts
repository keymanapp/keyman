/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Utility functions
 */

import { KMXPlus } from '@keymanapp/common-types';
import { constants } from '@keymanapp/ldml-keyboard-constants';

/**
 * @returns true if this is a gap key
 */
export function isGapKey(key: KMXPlus.KeysKeys) {
    const {flags} = key;
    return flags & constants.keys_key_flags_gap;
}

/** convert a modifier number to a string */
export function modToStr(mod: number) {
  if (mod === constants.keys_mod_none) {
    return 'none';
  }
  let ret: string[] = [];
  constants.keys_mod_map.forEach((mask, name) => {
    if (mod & mask) {
      ret.push(name);
    }
  });
  ret.sort(); // make it deterministic
  return ret.join(',');
}

/** treat hardware as width -1 for sorting */
export const WIDTH_HARDWARE = -1;

/** get an index for a LayrList */
export function touchWidth(list: KMXPlus.LayrList): number {
  if (list.hardware.value === constants.layr_list_hardware_touch) {
    return list.minDeviceWidth;
  } else {
    return -1;
  }
}

/** convert a layer entry to a title */
export function layerTitle(layer: KMXPlus.LayrEntry) {
  if (layer.id.value) return layer.id.value + modToStr(layer.mod);
  return modToStr(layer.mod);
}
/** convert a LayrList to a title */
export function listTitle(list: KMXPlus.LayrList) {
  const myWidth = touchWidth(list);
  const isTouch = (list.hardware.value === constants.layr_list_hardware_touch);
  const title = isTouch ? `Touch>${list.minDeviceWidth}px` : `${list.hardware.value}`;
  return title;
}

