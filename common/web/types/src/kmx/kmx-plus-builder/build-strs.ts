import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Strs, StrsItem } from "../kmx-plus.js";
import { BUILDER_SECTION } from "./builder-section.js";

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
  items: BUILDER_STRS_ITEM[];
};

export function build_strs(source_strs: Strs): BUILDER_STRS {
  let result: BUILDER_STRS = {
    ident: constants.hex_section_id(constants.section.strs),
    size: 0,  // finalized later
    _offset: 0,
    count: source_strs.strings.length,
    items: [], // filled below
  };

  result.items = source_strs.strings.map(item => { return {_value: item.value, length: item.value.length, offset: 0}; });
  result.items.sort((a,b) => StrsItem.binaryStringCompare(a._value, b._value));

  let offset = constants.length_strs + constants.length_strs_item * result.count;
  // TODO: consider padding
  for(let item of result.items) {
    item.offset = offset;
    offset += item.length * 2 + 2; /* UTF-16 code units + sizeof null terminator */
  }
  result.size = offset;

  return result;
}

export function build_strs_index(sect_strs: BUILDER_STRS, value: StrsItem) {
  if(!(value instanceof StrsItem)) {
    throw new Error('unexpected value '+ value);
  }

  let result = sect_strs.items.findIndex(v => v._value === value.value);
  if(result < 0) {
    throw new Error('unexpectedly missing StrsItem '+value.value);
  }
  return result;
}
