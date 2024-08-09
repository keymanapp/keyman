import { constants } from '@keymanapp/ldml-keyboard-constants';
import { KMXPlus } from "@keymanapp/common-types";
import { build_strs_index, BUILDER_STR_REF, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from './builder-section.js';

import KMXPlusData = KMXPlus.KMXPlusData;

/* ------------------------------------------------------------------
 * disp section
   ------------------------------------------------------------------ */

/**
 * Builder for the 'disp' section
 */
interface BUILDER_DISP_ITEM {
  to: BUILDER_STR_REF;
  id: BUILDER_STR_REF;
  display: BUILDER_STR_REF;
};

export interface BUILDER_DISP extends BUILDER_SECTION {
  count: number;
  baseCharacter: BUILDER_STR_REF;
  items: BUILDER_DISP_ITEM[];
};

export function build_disp(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS): BUILDER_DISP {
  if(!kmxplus.disp.disps.length && !kmxplus.disp.baseCharacter.value) {
    return null;
  }

  const disp: BUILDER_DISP = {
    ident: constants.hex_section_id(constants.section.disp),
    size: constants.length_disp + constants.length_disp_item * kmxplus.disp.disps.length,
    _offset: 0,
    count: kmxplus.disp.disps.length,
    baseCharacter: build_strs_index(sect_strs, kmxplus.disp.baseCharacter),
    items: []
  };

  for(const item of kmxplus.disp.disps) {
    disp.items.push({
      to: build_strs_index(sect_strs, item.to),
      id: build_strs_index(sect_strs, item.id),
      display: build_strs_index(sect_strs, item.display),
    });
  }

  return disp;
}
