import { constants } from "@keymanapp/ldml-keyboard-constants";
import { BUILDER_SECTION } from "./builder-section";

/* ------------------------------------------------------------------
 * strs section
   ------------------------------------------------------------------ */

interface BUILDER_STRS_ITEM {
  // While we use length which is number of utf-16 code units excluding null terminator,
  // we always write a null terminator, so we can get restructure to do that for us here
  offset: number; //? new r.Pointer(r.uint32le, new r.String(null, 'utf16le')),
  length: number; // in UTF-16 code units
  _value: string
};

/**
 * Builder for the 'strs' section
 */
export interface BUILDER_STRS extends BUILDER_SECTION {
  count: number;
  reserved: number;
  items: BUILDER_STRS_ITEM[];
};

export function build_strs(): BUILDER_STRS {
  return {
    ident: constants.hex_section_id(constants.section.strs),
    size: 0,  // finalized later
    _offset: 0,
    count: 0,  // finalized later
    reserved: 0,
    items: [], // finalized later
  };
}

export function finalize_strs(sect_strs: BUILDER_STRS): void {
  sect_strs.count = sect_strs.items.length;
  let offset = constants.length_strs + constants.length_strs_item * sect_strs.count;
  // TODO: consider padding
  for(let item of sect_strs.items) {
    item.offset = offset;
    offset += item.length * 2 + 2; /* UTF-16 code units + sizeof null terminator */
  }
  sect_strs.size = offset;
}

export function alloc_string(sect_strs: BUILDER_STRS, value: string) {
  if(typeof value != 'string') {
    if(value === undefined || value === null) {
      value = '';
    } else {
      throw 'unexpected value '+value;
    }
  }

  let idx = sect_strs.items.findIndex(v => v._value === value);
  if(idx >= 0) {
    return idx;
  }

  let item: BUILDER_STRS_ITEM = {
    _value: value,
    length: value.length,
    offset: 0 // will be filled in later
  };

  return sect_strs.items.push(item) - 1;
}
