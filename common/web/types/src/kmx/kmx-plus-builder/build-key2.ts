
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Key2Flick, KMXPlusData, StrsItem } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STRS } from "./build-strs.js";
import { build_list_index, BUILDER_LIST } from "./build-list.js";
import { BUILDER_SECTION } from "./builder-section.js";

/* ------------------------------------------------------------------
 * key2 section
   ------------------------------------------------------------------ */

/**
 * This struct is a single <key> in the key2 keybag
 */
interface BUILDER_KEY2_KEY {
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
interface BUILDER_KEY2_FLICKS {
  count: number; // number of BUILDER_KEY2_FLICK entries in this flick list
  flick: number; // index into the flick[] subtable of the first flick in the list
  id: number; // str with the original id of this flicks
  _id: string; // copy of the flicks id, used for sorting during build
  _flicks: Key2Flick[]; // temporary copy of Key2Flick object
};

/**
 * This is a single <flick> element.
 */
interface BUILDER_KEY2_FLICK {
  directions: number; // list of cardinal/intercardinal directions
  flags: number; //
  to: number; // str or single codepoint
};


interface BUILDER_KEY2_KMAP {
  vkey: number;
  mod: number;
  key: number; //index to key2.key
};

/**
 * Builder for the 'keys' section
 */
export interface BUILDER_KEY2 extends BUILDER_SECTION {
  ident: number;
  size: number;
  keyCount: number;
  flicksCount: number;
  flickCount: number;
  kmapCount: number;
  keys: BUILDER_KEY2_KEY[];
  flicks: BUILDER_KEY2_FLICKS[];
  flick: BUILDER_KEY2_FLICK[];
  kmap: BUILDER_KEY2_KMAP[];
};

export function build_key2(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS, sect_list: BUILDER_LIST): BUILDER_KEY2 {
  if(kmxplus.key2.keys.length == 0 &&
      (kmxplus.key2.flicks.length <= 1)) { // if no keys and only the 'null' flick.
    return null;
  }

  let key2: BUILDER_KEY2 = {
    ident: constants.hex_section_id(constants.section.key2),
    size: 0,
    keyCount: kmxplus.key2.keys.length,
    flicksCount: kmxplus.key2.flicks.length,
    flickCount: 0,
    kmapCount: kmxplus.key2.kmap.length,
    keys: [],
    flicks: [],
    flick: [],
    kmap: [],
    _offset: 0,
  };

  // flicks first: the keys will need to index into the flicks table.

  // Note that per the Key2 class and spec, there is always a flicks=0 meaning 'no flicks'
  key2.flicks = kmxplus.key2.flicks.map((flicks) => {
    let result : BUILDER_KEY2_FLICKS = {
      count: flicks.flicks.length,
      flick: key2.flick.length, // index of first flick
      id: build_strs_index(sect_strs, flicks.id),
      _id: flicks.id.value,
      _flicks: flicks.flicks,
    };
    return result;
  });
  // Sort the flicks array by id
  key2.flicks.sort((a, b) => StrsItem.binaryStringCompare(a._id, b._id));
  // now, allocate 'flick' entries for each 'flicks'
  key2.flicks.forEach((flicks) => {
    flicks._flicks.forEach((flick) => {
      key2.flick.push({
        directions: build_list_index(sect_list, flick.directions),
        flags: flick.flags,
        to: build_strs_index(sect_strs, flick.to),
      });
      key2.flickCount++;
    });
  });

  // now, keys
  key2.keys = kmxplus.key2.keys.map((key) => {
    let result : BUILDER_KEY2_KEY = {
      to: build_strs_index(sect_strs, key.to),
      flags: key.flags,
      id: build_strs_index(sect_strs, key.id),
      _id: key.id.value,
      switch: build_strs_index(sect_strs, key.switch),
      width: key.width,
      longPress: build_list_index(sect_list, key.longPress),
      longPressDefault: build_strs_index(sect_strs, key.longPressDefault),
      multiTap: build_list_index(sect_list, key.multiTap),
      flicks: key2.flicks.findIndex(v => v._id === (key.flicks || '')), // flicks id='' is the 'null' flicks
    };
    // Make sure the flicks were found
    if (result.flicks === -1) {
      throw new Error(`Key2: Could not find flicks id=${key.flicks} for key=${key.id.value}`);
    }
    return result;
  });
  // sort the keys by id
  key2.keys.sort((a, b) => StrsItem.binaryStringCompare(a._id, b._id));

  // finally, kmap
  key2.kmap = kmxplus.key2.kmap.map(({vkey, mod, key}) => {
    let result : BUILDER_KEY2_KMAP = {
      vkey,
      mod,
      key: key2.keys.findIndex(k => k._id === key),
    };
    // Make sure the key was found
    if (result.key === -1) {
      throw new Error(`Key2: Could not find key2.key id=${result.key} for key2.kmap.key=${key}`);
    }
    return result;
  });

  // Sort kmap by vkey, mod order, per C7043
  key2.kmap.sort((a,b) => {
    let rc = 0;
    if (rc === 0) {
      rc = (a.vkey - b.vkey);
    }
    if (rc === 0) {
      rc = (a.mod - b.mod);
    }
    return rc;
  });

  let offset = constants.length_key2 +
    (constants.length_key2_key * key2.keyCount) +
    (constants.length_key2_flick_element * key2.flickCount) +
    (constants.length_key2_flick_list * key2.flicksCount) +
    (constants.length_key2_kmap * key2.kmapCount);
  key2.size = offset;

  return key2;
}
