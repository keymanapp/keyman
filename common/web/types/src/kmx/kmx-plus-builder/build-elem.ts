import { constants } from "@keymanapp/ldml-keyboard-constants";
import { ElementString } from "../element-string.js";
import { Elem } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STR_REF, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION, BUILDER_U32CHAR } from "./builder-section.js";
import { build_uset_index, BUILDER_USET, BUILDER_USET_REF } from "./build-uset.js";

/* ------------------------------------------------------------------
 * elem section
   ------------------------------------------------------------------ */

/** return from build_elem_index */
export type BUILDER_ELEM_REF = number;

interface BUILDER_ELEM_ELEMENT {
  element: BUILDER_STR_REF | BUILDER_USET_REF | BUILDER_U32CHAR;  // str | UTF-32 char
  flags: number;
  _value: string;
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
  strings: BUILDER_ELEM_STRING[];
};

function binaryElemCompare(a: BUILDER_ELEM_STRING, b: BUILDER_ELEM_STRING): number {
  for(let i = 0; i < a.items.length && i < b.items.length; i++) {
    if(a.items[i]._value < b.items[i]._value) return -1;
    if(a.items[i]._value > b.items[i]._value) return 1;
    if(a.items[i].flags < b.items[i].flags) return -1;
    if(a.items[i].flags > b.items[i].flags) return 1;
  }
  if(a.items.length < b.items.length) return -1;
  if(a.items.length > b.items.length) return 1;
  return 0;
}

export function build_elem(source_elem: Elem, sect_strs: BUILDER_STRS, sect_uset: BUILDER_USET): BUILDER_ELEM {
  let result: BUILDER_ELEM = {
    ident: constants.hex_section_id(constants.section.elem),
    size: 0,  // finalized below
    _offset: 0,
    count: source_elem.strings.length,
    strings: [], // finalized below
  };

  result.strings = source_elem.strings.map(item => {
    let res: BUILDER_ELEM_STRING = {
      offset: 0, // finalized below
      length: item.length,
      items: [],
      _value: item
    };

    res.items = item.map(v => {
      let element;
      const type = (v.flags & constants.elem_flags_type);
      if (type === constants.elem_flags_type_char || type == constants.elem_flags_type_str) {
        element = build_strs_index(sect_strs, v.value);
      } else if (type == constants.elem_flags_type_uset) {
        element = build_uset_index(sect_uset, v.uset);
      } else {
        throw Error(`Internal Error: Unknown element type 0x${type.toString(16)}`);
      }
      return {
        element,
        flags: v.flags |                                                             //
              ((v.order ?? 0) << constants.elem_flags_order_bitshift) |             // -128 to +127; used only by reorder element values
              ((v.tertiary ?? 0) << constants.elem_flags_tertiary_bitshift),        // -128 to +127; used only by reorder element values
        _value: v.value.value
      };
    });
    return res;
  });
  result.strings.sort((a,b) => binaryElemCompare(a, b));

  /* Calculate offsets and total size */

  let offset = constants.length_elem + constants.length_elem_item * result.count;
  for(let item of result.strings) {
    if (item.length === 0) {
      // no length gets a zero offset
      item.offset = 0;
    } else {
      item.offset = offset;
      offset += item.length * constants.length_elem_item_element;
    }
  }

  result.size = offset;
  return result;
}

export function build_elem_index(sect_elem: BUILDER_ELEM, value: ElementString) : BUILDER_ELEM_REF{
  if(!(value instanceof ElementString)) {
    throw new Error('unexpected value '+value);
  }

  const result = sect_elem.strings.findIndex(v => value.isEqual(v._value));
  if(result < 0) {
    throw new Error('unexpectedly missing StrsItem '+value);
  }
  return <BUILDER_ELEM_REF>result;
}
