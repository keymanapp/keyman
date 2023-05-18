
/* ------------------------------------------------------------------
 * loca section
   ------------------------------------------------------------------ */

import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlusData } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";

/**
 * Builder for the 'loca' section
 */
export interface BUILDER_LOCA extends BUILDER_SECTION {
  count: number;
  items: number[]; //str[]
};

export function build_loca(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS): BUILDER_LOCA {
  let loca: BUILDER_LOCA = {
    ident: constants.hex_section_id(constants.section.loca),
    size: constants.length_loca + constants.length_loca_item * kmxplus.loca.locales.length,
    _offset: 0,
    count: kmxplus.loca.locales.length,
    items: []
  };

  for(let item of kmxplus.loca.locales) {
    loca.items.push(build_strs_index(sect_strs, item));
  }

  return loca;
}

