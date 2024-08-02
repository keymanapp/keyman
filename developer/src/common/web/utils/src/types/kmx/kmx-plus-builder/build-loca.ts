
/* ------------------------------------------------------------------
 * loca section
   ------------------------------------------------------------------ */

import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus } from "@keymanapp/common-types";
import { build_strs_index, BUILDER_STR_REF, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";

import KMXPlusData = KMXPlus.KMXPlusData;

/**
 * Builder for the 'loca' section
 */
export interface BUILDER_LOCA extends BUILDER_SECTION {
  count: number;
  items: BUILDER_STR_REF[]; //str[]
};

export function build_loca(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS): BUILDER_LOCA {
  const loca: BUILDER_LOCA = {
    ident: constants.hex_section_id(constants.section.loca),
    size: constants.length_loca + constants.length_loca_item * kmxplus.loca.locales.length,
    _offset: 0,
    count: kmxplus.loca.locales.length,
    items: []
  };

  for(const item of kmxplus.loca.locales) {
    loca.items.push(build_strs_index(sect_strs, item));
  }

  return loca;
}

