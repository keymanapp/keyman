import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlusData, StrsItem } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";


interface BUILDER_USET_USET {
  range: number;
  count: number;
  pattern: number; // str
  _pattern: StrsItem; // for sorting
};

interface BUILDER_USET_RANGE {
  start: number;
  end: number;
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

  // collect all patterns, sorted
  const patterns = kmxplus.uset.usets.map(s => s.uset.pattern).sort();

  const ranges : BUILDER_USET_RANGE[] = [];

  const usets : BUILDER_USET_USET[] = [];

  // collect all sets
  // in pattern order
  patterns.forEach(pattern => {
    // find the set
    const set = kmxplus?.uset?.usets.find(s => s.uset.pattern == pattern);
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
