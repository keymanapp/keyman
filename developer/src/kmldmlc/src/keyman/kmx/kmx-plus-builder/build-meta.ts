
/* ------------------------------------------------------------------
 * meta section
   ------------------------------------------------------------------ */

import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlusData } from "../kmx-plus";
import { alloc_string, BUILDER_STRS } from "./build-strs";
import { BUILDER_SECTION } from "./builder-section";

/**
 * Builder for the 'meta' section
 */
export interface BUILDER_META extends BUILDER_SECTION {
  name: number; //str
  author: number; //str
  conform: number; //str
  layout: number; //str
  normalization: number; //str
  indicator: number; //str
  version: number; //str
  settings: number; //bitfield
};

export function build_meta(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS): BUILDER_META {
  return {
    ident: constants.hex_section_id(constants.section.meta),
    size: constants.length_meta,
    _offset: 0,
    name: alloc_string(sect_strs, kmxplus.meta.name),
    author: alloc_string(sect_strs, kmxplus.meta.author),
    conform: alloc_string(sect_strs, kmxplus.meta.conform),
    layout: alloc_string(sect_strs, kmxplus.meta.layout),
    normalization: alloc_string(sect_strs, kmxplus.meta.normalization),
    indicator: alloc_string(sect_strs, kmxplus.meta.indicator),
    version: alloc_string(sect_strs, kmxplus.meta.version),
    settings: kmxplus.meta.settings ?? 0,
  };
}
