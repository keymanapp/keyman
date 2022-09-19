
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Bksp, Finl, Tran } from "../kmx-plus.js";
import { build_elem_index, BUILDER_ELEM } from "./build-elem.js";
import { build_strs_index, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";

/* ------------------------------------------------------------------
* tran section
  ------------------------------------------------------------------ */

interface BUILDER_TRAN_ITEM {
  from: number; //elem.string
  to: number; //str
  before: number; //elem.string
  flags: number;
};

export interface BUILDER_TRAN extends BUILDER_SECTION {
 count: number;
 reserved: number;
 items: BUILDER_TRAN_ITEM[];
};

/**
* Builder for the 'tran', 'finl', and 'bksp' sections, all of which use the BUILDER_TRAN layout
*/
export function build_tran(source_tran: Tran|Finl|Bksp, sect_strs: BUILDER_STRS, sect_elem: BUILDER_ELEM): BUILDER_TRAN {
 if(!source_tran?.items.length) {
   return null;
 }

 let tran: BUILDER_TRAN = {
   ident: constants.hex_section_id(source_tran.id),
   size: constants.length_tran + constants.length_tran_item * source_tran.items.length,
   _offset: 0,
   count: source_tran.items.length,
   reserved: 0,
   items: []
 };

 for(let item of source_tran.items) {
   tran.items.push({
    from: build_elem_index(sect_elem, item.from),
    to: build_strs_index(sect_strs, item.to),
    before: build_elem_index(sect_elem, item.before),
    flags: item.flags
   });
 }

 return tran;
}
