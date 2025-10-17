import { constants, KMXPlusVersion } from '@keymanapp/ldml-keyboard-constants';
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
  // v17 DISP
  to: BUILDER_STR_REF;  // not used in v19
  id: BUILDER_STR_REF;  // not used in v19
  display: BUILDER_STR_REF;

  // v19 DIS2
  toId: BUILDER_STR_REF;
  flags: number;
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
    ident: constants.hex_section_id(
      kmxplus.version == KMXPlusVersion.Version17
        ? constants.section.disp
        : constants.section.dis2
    ),
    size:
      kmxplus.version == KMXPlusVersion.Version17
        ? constants.length_disp + constants.length_disp_item * kmxplus.disp.disps.length
        : constants.length_dis2 + constants.length_dis2_item * kmxplus.disp.disps.length,
    _offset: 0,
    count: kmxplus.disp.disps.length,
    baseCharacter: build_strs_index(sect_strs, kmxplus.disp.baseCharacter),
    items: []
  };

  for(const item of kmxplus.disp.disps) {
    const to = build_strs_index(sect_strs, item.to);
    const id = build_strs_index(sect_strs, item.id);
    disp.items.push({
      to,
      id,
      display: build_strs_index(sect_strs, item.display),
      // v19
      toId: to ? to : id,
      flags: 0 // TODO-EMBED-OSK-IN-KMX
    });
  }

  return disp;
}
