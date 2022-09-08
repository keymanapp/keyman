import { constants } from '@keymanapp/ldml-keyboard-constants';
import { KMXPlusData } from "../kmx-plus";
import { BUILDER_SECTION } from './builder-section';

/* ------------------------------------------------------------------
 * vkey section
   ------------------------------------------------------------------ */

/**
 * Builder for the 'vkey' section
 */
interface BUILDER_VKEY_ITEM {
  vkey: number;
  target: number;
};

export interface BUILDER_VKEY extends BUILDER_SECTION {
  count: number;
  reserved: number;
  items: BUILDER_VKEY_ITEM[];
};

export function build_vkey(kmxplus: KMXPlusData): BUILDER_VKEY {
  if(!kmxplus.vkey.vkeys.length) {
    return null;
  }

  let vkey: BUILDER_VKEY = {
    ident: constants.hex_section_id(constants.section.vkey),
    size: constants.length_vkey + constants.length_vkey_item * kmxplus.vkey.vkeys.length,
    _offset: 0,
    reserved: 0,
    count: kmxplus.vkey.vkeys.length,
    items: []
  };

  for(let item of kmxplus.vkey.vkeys) {
    vkey.items.push({
      vkey: item.vkey,
      target: item.target
    });
  }

  return vkey;
}
