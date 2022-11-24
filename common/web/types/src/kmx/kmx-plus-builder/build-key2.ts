
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlusData } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STRS } from "./build-strs.js";
import { build_list_index, BUILDER_LIST } from "./build-list.js";
import { BUILDER_SECTION } from "./builder-section.js";

/* ------------------------------------------------------------------
 * key2 section
   ------------------------------------------------------------------ */

interface BUILDER_KEY2_KEY {
  vkey: number;
  to: number; // str
  flags: number;
  id: number; // str
  switch: number; // str
  width: number; // width*10
  longPress: number; // list
  longPressDefault: number; // str
  multiTap: number; // list
  flicks: number; // index into flicks[]
};

interface BUILDER_KEY2_FLICK {
  directions: number; // list
  flags: number;
  to: number; // str
};

interface BUILDER_KEY2_FLICKS {
  count: number;
  flick: number; // index into flick[]
  id: number; //str
  _id: string;
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
  reserved0: number;
  reserved1: number;
  reserved2: number;
  keys: BUILDER_KEY2_KEY[];
  flicks: BUILDER_KEY2_FLICKS[];
  flick: BUILDER_KEY2_FLICK[];
};

export function build_key2(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS, sect_list: BUILDER_LIST): BUILDER_KEY2 {
  if(!kmxplus.key2.keys.length &&
      (kmxplus.key2.flicks.length <= 1)) { // if no keys and only the 'null' flick.
    return null;
  }

  let key2: BUILDER_KEY2 = {
    ident: constants.hex_section_id(constants.section.key2),
    size: 0,
    keyCount: kmxplus.key2.keys.length,
    flicksCount: kmxplus.key2.flicks.length,
    flickCount: 0,
    reserved0: 0,
    reserved1: 0,
    reserved2: 0,
    keys: [],
    flicks: [],
    flick: [],
    _offset: 0,
  };

  // flicks first
  // sort the input, to simplify bookkeeping later
  kmxplus.key2.flicks.sort((a, b) => a.compareTo(b));
  // we always need a flicks=0 to mena 'no flicks'
  key2.flicks = kmxplus.key2.flicks.map((flicks) => {
    let result : BUILDER_KEY2_FLICKS = {
      count: flicks.flicks.length,
      flick: key2.flick.length, // index of first flick
      id: build_strs_index(sect_strs, flicks.id),
      _id: flicks.id.value,
    };
    flicks.flicks.forEach((flick) => {
      key2.flick.push({
        directions: build_list_index(sect_list, flick.directions),
        flags: flick.flags,
        to: build_strs_index(sect_strs, flick.to),
      });
      key2.flickCount++;
    });
    return result;
  });

  // now keys
  kmxplus.key2.keys.sort((a, b) => a.id.compareTo(b.id));
  key2.keys = kmxplus.key2.keys.map((key) => {
    let result : BUILDER_KEY2_KEY = {
      vkey: key.vkey,
      to: build_strs_index(sect_strs, key.to),
      flags: key.flags,
      id: build_strs_index(sect_strs, key.id),
      switch: build_strs_index(sect_strs, key.switch),
      width: key.width,
      longPress: build_list_index(sect_list, key.longPress),
      longPressDefault: build_strs_index(sect_strs, key.longPressDefault),
      multiTap: build_list_index(sect_list, key.multiTap),
      flicks: key2.flicks.findIndex(v => v._id === key.flicks),
    };
    // Make sure the flicks were was found
    if (key.flicks && !result.flicks) {
      throw new Error(`Key2: Could not find flicks id=${key.flicks} for key=${key.id.value}`);
    }
    return result;
  });

  let offset = constants.length_key2 +
    (constants.length_key2_key * key2.keyCount) +
    (constants.length_key2_flick_element * key2.flickCount) +
    (constants.length_key2_flick_list * key2.flicksCount);
  key2.size = offset;

  return key2;
}
