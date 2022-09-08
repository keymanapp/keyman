import { constants } from "@keymanapp/ldml-keyboard-constants";
import { ElementString } from "../element-string";
import { alloc_string, BUILDER_STRS } from "./build-strs";
import { BUILDER_SECTION } from "./builder-section";

/* ------------------------------------------------------------------
 * elem section
   ------------------------------------------------------------------ */

interface BUILDER_ELEM_ELEMENT {
  element: number;  // str | UTF-32 char
  flags: number;
};

interface BUILDER_ELEM_STRING {
  offset: number;
  length: number;
  items: BUILDER_ELEM_ELEMENT[];
  _value: ElementString;
};

/**
 * Builder for the 'elem' section
 */
export interface BUILDER_ELEM extends BUILDER_SECTION {
  count: number;
  reserved: number;
  strings: BUILDER_ELEM_STRING[];
};

export function build_elem(): BUILDER_ELEM {
  return {
    ident: constants.hex_section_id(constants.section.elem),
    size: 0,  // finalized later
    _offset: 0,
    count: 0,  // finalized later
    reserved: 0,
    strings: [], // finalized later
  };
}

/**
 *
 * @param sect_elem
 * @returns false if the section should be omitted from the file
 */
export function finalize_elem(sect_elem: BUILDER_ELEM): boolean {
  if(sect_elem.strings.length == 1) {
    // If we have only the 'null' element string, then we don't include the
    // section at all.
    return false;
  }
  sect_elem.count = sect_elem.strings.length;
  let offset = constants.length_elem + constants.length_elem_item * sect_elem.count;
  // TODO: consider padding
  for(let string of sect_elem.strings) {
    string.offset = offset;
    offset += string.length * constants.length_elem_item_element;
  }
  sect_elem.size = offset;
  return true;
}

export function alloc_element_string(sect_strs: BUILDER_STRS, sect_elem: BUILDER_ELEM, value: ElementString) {
  if(value === undefined || value === null) {
    // the "null" element string
    value = new ElementString('');
  }

  const idx = sect_elem.strings.findIndex(v => value.isEqual(v._value));
  if(idx >= 0) {
    return idx;
  }

  let str: BUILDER_ELEM_STRING = {
    _value: value,
    items: [],
    length: value.length,
    offset: 0 // will be filled in later
  };

  for(let v of value) {
    str.items.push({
      // TODO: support UTF-32 char (!UnicodeSet)
      // TODO: support UnicodeSet properly
      element: alloc_string(sect_strs, v.value),
      flags: constants.elem_flags_unicode_set |
             v.flags |                                                             //
             ((v.order ?? 0) << constants.elem_flags_order_bitshift) |             // -128 to +127; used only by reorder element values
             ((v.tertiary ?? 0) << constants.elem_flags_tertiary_bitshift)            // -128 to +127; used only by reorder element values
    });
  }

  return sect_elem.strings.push(str) - 1;
}
