
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KeysFlick, KMXPlusData, StrsItem } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STRS } from "./build-strs.js";
import { build_list_index, BUILDER_LIST } from "./build-list.js";
import { BUILDER_SECTION } from "./builder-section.js";

/* ------------------------------------------------------------------
 * keys section
   ------------------------------------------------------------------ */

/**
 * This struct is a single <key> in the keys keybag
 */
interface BUILDER_KEYS_KEY {
  to: number; // str or single codepoint
  flags: number;
  id: number; // str with original key id
  _id: string; // original key id, for sorting
  switch: number; // str with layer of new l
  width: number; // ceil((width||1)*10), so 12 for width 1.2
  longPress: number; // list of longPress sequences
  longPressDefault: number; // str with the default longPress target
  multiTap: number; // list of multiTap sequences
  flicks: number; // index into the flicks[] subtable for this flick list
};

/**
 * This is a <flicks>, a list of <flick> elements.
 */
interface BUILDER_KEYS_FLICKS {
  count: number; // number of BUILDER_KEYS_FLICK entries in this flick list
  flick: number; // index into the flick[] subtable of the first flick in the list
  id: number; // str with the original id of this flicks
  _id: string; // copy of the flicks id, used for sorting during build
  _flicks: KeysFlick[]; // temporary copy of KeysFlick object
};

/**
 * This is a single <flick> element.
 */
interface BUILDER_KEYS_FLICK {
  directions: number; // list of cardinal/intercardinal directions
  flags: number; //
  to: number; // str or single codepoint
};


interface BUILDER_KEYS_KMAP {
  vkey: number;
  mod: number;
  key: number; //index to keys.key
};

/**
 * Builder for the 'keys' section
 */
export interface BUILDER_KEYS extends BUILDER_SECTION {
  ident: number;
  size: number;
  keyCount: number;
  flicksCount: number;
  flickCount: number;
  kmapCount: number;
  keys: BUILDER_KEYS_KEY[];
  flicks: BUILDER_KEYS_FLICKS[];
  flick: BUILDER_KEYS_FLICK[];
  kmap: BUILDER_KEYS_KMAP[];
};

export function build_keys(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS, sect_list: BUILDER_LIST): BUILDER_KEYS {
  if(kmxplus.keys.keys.length == 0 &&
      (kmxplus.keys.flicks.length <= 1)) { // if no keys and only the 'null' flick.
    return null;
  }

  let keys: BUILDER_KEYS = {
    ident: constants.hex_section_id(constants.section.keys),
    size: 0,
    keyCount: kmxplus.keys.keys.length,
    flicksCount: kmxplus.keys.flicks.length,
    flickCount: 0,
    kmapCount: kmxplus.keys.kmap.length,
    keys: [],
    flicks: [],
    flick: [],
    kmap: [],
    _offset: 0,
  };

  // flicks first: the keys will need to index into the flicks table.

  // Note that per the Keys class and spec, there is always a flicks=0 meaning 'no flicks'
  keys.flicks = kmxplus.keys.flicks.map((flicks) => {
    let result : BUILDER_KEYS_FLICKS = {
      count: flicks.flicks.length,
      flick: keys.flick.length, // index of first flick
      id: build_strs_index(sect_strs, flicks.id),
      _id: flicks.id.value,
      _flicks: flicks.flicks,
    };
    return result;
  });
  // Sort the flicks array by id
  keys.flicks.sort((a, b) => StrsItem.binaryStringCompare(a._id, b._id));
  // now, allocate 'flick' entries for each 'flicks'
  keys.flicks.forEach((flicks) => {
    flicks._flicks.forEach((flick) => {
      keys.flick.push({
        directions: build_list_index(sect_list, flick.directions),
        flags: flick.flags,
        to: build_strs_index(sect_strs, flick.to),
      });
      keys.flickCount++;
    });
  });

  // now, keys
  keys.keys = kmxplus.keys.keys.map((key) => {
    let result : BUILDER_KEYS_KEY = {
      to: build_strs_index(sect_strs, key.to),
      flags: key.flags,
      id: build_strs_index(sect_strs, key.id),
      _id: key.id.value,
      switch: build_strs_index(sect_strs, key.switch),
      width: key.width,
      longPress: build_list_index(sect_list, key.longPress),
      longPressDefault: build_strs_index(sect_strs, key.longPressDefault),
      multiTap: build_list_index(sect_list, key.multiTap),
      flicks: keys.flicks.findIndex(v => v._id === (key.flicks || '')), // flicks id='' is the 'null' flicks
    };
    // Make sure the flicks were found
    if (result.flicks === -1) {
      throw new Error(`Keys: Could not find flicks id=${key.flicks} for key=${key.id.value}`);
    }
    return result;
  });
  // sort the keys by id
  keys.keys.sort((a, b) => StrsItem.binaryStringCompare(a._id, b._id));

  // finally, kmap
  keys.kmap = kmxplus.keys.kmap.map(({vkey, mod, key}) => {
    let result : BUILDER_KEYS_KMAP = {
      vkey,
      mod,
      key: keys.keys.findIndex(k => k._id === key),
    };
    // Make sure the key was found
    if (result.key === -1) {
      throw new Error(`Keys: Could not find keys.key id=${result.key} for keys.kmap.key=${key}`);
    }
    return result;
  });

  // Sort kmap by vkey, mod order, per C7043
  keys.kmap.sort((a,b) => {
    let rc = 0;
    if (rc === 0) {
      rc = (a.vkey - b.vkey);
    }
    if (rc === 0) {
      rc = (a.mod - b.mod);
    }
    return rc;
  });

  let offset = constants.length_keys +
    (constants.length_keys_key * keys.keyCount) +
    (constants.length_keys_flick_element * keys.flickCount) +
    (constants.length_keys_flick_list * keys.flicksCount) +
    (constants.length_keys_kmap * keys.kmapCount);
  keys.size = offset;

  return keys;
}
