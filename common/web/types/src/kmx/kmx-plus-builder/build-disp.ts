import { constants } from '@keymanapp/ldml-keyboard-constants';
import { KMXPlusData } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from './builder-section.js';

/* ------------------------------------------------------------------
 * disp section
   ------------------------------------------------------------------ */

/**
 * Builder for the 'disp' section
 */
interface BUILDER_DISP_ITEM {
  to: number;
  display: number;
};

export interface BUILDER_DISP extends BUILDER_SECTION {
  count: number;
  baseCharacter: number;
  reserved0: number;
  reserved1: number;
  reserved2: number;
  reserved3: number;
  items: BUILDER_DISP_ITEM[];
};

export function build_disp(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS): BUILDER_DISP {
  if(!kmxplus.disp.disps.length && !kmxplus.disp.baseCharacter.value) {
    return null;
  }

  let disp: BUILDER_DISP = {
    ident: constants.hex_section_id(constants.section.disp),
    size: constants.length_disp + constants.length_disp_item * kmxplus.disp.disps.length,
    _offset: 0,
    count: kmxplus.vkey.vkeys.length,
    baseCharacter: build_strs_index(sect_strs, kmxplus.disp.baseCharacter),
    reserved0: 0,
    reserved1: 0,
    reserved2: 0,
    reserved3: 0,
    items: []
  };

  for(let item of kmxplus.disp.disps) {
    disp.items.push({
      to: build_strs_index(sect_strs, item.to),
      display: build_strs_index(sect_strs, item.display),
    });
  }

  return disp;
}
