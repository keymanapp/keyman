
/* ------------------------------------------------------------------
 * meta section
   ------------------------------------------------------------------ */

import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlusData } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STR_REF, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";

/**
 * Builder for the 'meta' section
 */
export interface BUILDER_META extends BUILDER_SECTION {
  author: BUILDER_STR_REF; //str
  conform: BUILDER_STR_REF; //str
  layout: BUILDER_STR_REF; //str
  name: BUILDER_STR_REF; //str
  indicator: BUILDER_STR_REF; //str
  version: BUILDER_STR_REF; //str
  settings: number; //bitfield
};

export function build_meta(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS): BUILDER_META {
  return {
    ident: constants.hex_section_id(constants.section.meta),
    size: constants.length_meta,
    _offset: 0,
    author: build_strs_index(sect_strs, kmxplus.meta.author),
    conform: build_strs_index(sect_strs, kmxplus.meta.conform),
    layout: build_strs_index(sect_strs, kmxplus.meta.layout),
    name: build_strs_index(sect_strs, kmxplus.meta.name),
    indicator: build_strs_index(sect_strs, kmxplus.meta.indicator),
    version: build_strs_index(sect_strs, kmxplus.meta.version),
    settings: kmxplus.meta.settings ?? 0,
  };
}
