
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KeyFlags, KMXPlusData } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";

/* ------------------------------------------------------------------
 * keys section
   ------------------------------------------------------------------ */


// interface BUILDER_KEY2_ITEM {
//   vkey: number;
//   mod: number;
//   to: number; //str or UTF-32 char depending on value of 'extend'
//   flags: number; //bitfield
// };

/**
 * Builder for the 'keys' section
 */
export interface BUILDER_LAYR extends BUILDER_SECTION {
//   count: number;
//   reserved: number;
//   items: BUILDER_KEYS_ITEM[];
};


export function build_layr(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS, sect_list: BUILDER_LIST): BUILDER_LAYR {
  if(!kmxplus.keys.keys.length) {
    return null;
  }

  let layr: BUILDER_LAYR = {
      ident: 0,
      size: 0,
      _offset: 0
  };

  return layr;
}
