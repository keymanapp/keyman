import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlusData, StrsItem, UsetItem } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STR_REF, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION, BUILDER_U32CHAR } from "./builder-section.js";

/** reference from build_uset_index */
export type BUILDER_USET_REF = number;

interface BUILDER_USET_USET {
  range: number;
  count: number;
  pattern: BUILDER_STR_REF;
  _pattern: StrsItem; // for sorting
};

interface BUILDER_USET_RANGE {
  start: BUILDER_U32CHAR; // uchar32
  end: BUILDER_U32CHAR; // uchar32
}

export interface BUILDER_USET extends BUILDER_SECTION {
  usetCount: number;
  rangeCount: number;
  usets: BUILDER_USET_USET[];
  ranges: BUILDER_USET_RANGE[];
};

/**
* Builder for the 'uset' section
*/
export function build_uset(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS ) : BUILDER_USET {
  if(!kmxplus?.uset?.usets) {
    return null;
  }

  // uset items in pattern order
  const items = [...kmxplus.uset.usets]
    .sort((a,b) => a.compareTo(b));

  const ranges : BUILDER_USET_RANGE[] = [];

  const usets : BUILDER_USET_USET[] = [];

  items.forEach(set => {
    usets.push({
      range: ranges.length, // index
      count: set.uset.length,
      pattern: build_strs_index(sect_strs, set.str),
      _pattern: set.str,
    });
    // append the ranges
    set.uset.ranges.forEach(([start, end]) => ranges.push({ start, end }));
  });

  const uset: BUILDER_USET = {
    ident: constants.hex_section_id(constants.section.uset),
    size: constants.length_uset +
      (constants.length_uset_uset * usets.length) +
      (constants.length_uset_range * ranges.length),
    usetCount: usets.length,
    rangeCount: ranges.length,
    usets,
    ranges,
    _offset: 0,
  };

  return uset;
}


/**
 * @returns uset index
 */
export function build_uset_index(sect_uset: BUILDER_USET, value: UsetItem) {
  if(!(value instanceof UsetItem)) {
    if (value === null) {
      throw new Error('unexpected null UsetItem');
    } else {
      throw new Error('Expected UsetItem but got '+ value);
    }
  }

  let result = sect_uset.usets.findIndex(v => v._pattern.value === value.str.value);
  if(result < 0) {
    throw new Error('unexpectedly missing UsetItem ' + value.uset.toString());
  }
  return result;
}
