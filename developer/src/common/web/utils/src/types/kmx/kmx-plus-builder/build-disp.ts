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
  // v17
  to: BUILDER_STR_REF;  // not used in v19
  id: BUILDER_STR_REF;  // not used in v19
  display: BUILDER_STR_REF;

  // v19+
  toId: BUILDER_STR_REF;
  flags: KMXPlus.DispItemFlags;
};

export interface BUILDER_DISP extends BUILDER_SECTION {
  count: number;
  baseCharacter: BUILDER_STR_REF;
  items: BUILDER_DISP_ITEM[];
};

export function build_disp(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS, version: KMXPlusVersion): BUILDER_DISP {
  if(!kmxplus.disp.disps.length && !kmxplus.disp.baseCharacter.value) {
    return null;
  }

  const disp: BUILDER_DISP = {
    header: {
      ident: constants.hex_section_id(constants.section.disp),
      size: constants.length_disp + constants.length_disp_item * kmxplus.disp.disps.length, // note size is same in v17 and v19 but content differs
      version: version == KMXPlusVersion.Version17 ? KMXPlusVersion.Version17 : KMXPlusVersion.Version19,
    },
    _offset: 0,
    count: kmxplus.disp.disps.length,
    baseCharacter: build_strs_index(sect_strs, kmxplus.disp.baseCharacter),
    items: []
  };

  for(const item of kmxplus.disp.disps) {
    // Only allocate strings that we are actually going to use
    const to = version == KMXPlusVersion.Version17 ? build_strs_index(sect_strs, item.to) : 0;
    const id = version == KMXPlusVersion.Version17 ? build_strs_index(sect_strs, item.id) : 0;
    const toId = version == KMXPlusVersion.Version19 ? build_strs_index(sect_strs, item.toId) : 0;
    disp.items.push({
      to,
      id,
      display: build_strs_index(sect_strs, item.display),
      // v19
      toId,
      flags: item.flags,
    });
  }

  return disp;
}
