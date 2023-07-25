
/* ------------------------------------------------------------------
 * name section
   ------------------------------------------------------------------ */

import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlusData } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STR_REF, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";

/**
 * Builder for the 'name' section
 */
export interface BUILDER_NAME extends BUILDER_SECTION {
  count: number;
  items: BUILDER_STR_REF[];
};

export function build_name(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS): BUILDER_NAME {
  if(!kmxplus.name.names.length) {
    return null;
  }

  let name: BUILDER_NAME = {
    ident: constants.hex_section_id(constants.section.name),
    size: constants.length_name + constants.length_name_item * kmxplus.name.names.length,
    _offset: 0,
    count: kmxplus.name.names.length,
    items: []
  };

  for(let item of kmxplus.name.names) {
    name.items.push(build_strs_index(sect_strs, item));
  }

  return name;
}
