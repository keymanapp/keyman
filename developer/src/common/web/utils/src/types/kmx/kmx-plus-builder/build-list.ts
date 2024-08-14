import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus } from "@keymanapp/common-types";
import { build_strs_index, BUILDER_STR_REF, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";

import List = KMXPlus.List;
import ListItem = KMXPlus.ListItem;

/* ------------------------------------------------------------------
 * list section
   ------------------------------------------------------------------ */

/** reference from build_list_index */
export type BUILDER_LIST_REF = number;

   /**
 * A list entry.
 */
interface BUILDER_LIST_LIST {
  index: number; // index into indices[] subtable
  count: number; // number of strings in this list
  _value: ListItem; // for locating the list during finalization
};

interface BUILDER_LIST_INDEX {
  str: BUILDER_STR_REF; // str for this string
  _value: string; // for locating this string during finalization
};

/**
 * Builder for the 'list' section
 */
export interface BUILDER_LIST extends BUILDER_SECTION {
  listCount: number; // Number of lists total in the subtable
  indexCount: number; // Total number of indices in the subtable
  lists: BUILDER_LIST_LIST[];
  indices: BUILDER_LIST_INDEX[];
};

export function build_list(source_list: List, sect_strs: BUILDER_STRS): BUILDER_LIST {
  if(!source_list?.lists?.length) {
    // there's always the null list
    return null;
  }

  const result: BUILDER_LIST = {
    ident: constants.hex_section_id(constants.section.list),
    size: 0,
    _offset: 0,
    listCount: source_list.lists.length,
    indexCount: 0,
    lists: [],
    indices: [],
  };

  result.lists = source_list.lists.map(array => {
    const list : BUILDER_LIST_LIST = {
      index: result.indices.length, // the next indexcount
      count: array.length,
      _value: array
    };
    array.forEach((i) => {
      const index : BUILDER_LIST_INDEX = {
        // Get the final string index
        str: build_strs_index(sect_strs, i.value),
        _value: i.value.value, // unwrap the actual string value
      };
      result.indices.push(index); // increment the indexCount
      result.indexCount++;
    });
    return list;
  });

  // Sort the lists.
  result.lists.sort((a,b) => a._value.compareTo(b._value));

  const offset = constants.length_list +
    (constants.length_list_item * result.listCount) +
    (constants.length_list_index * result.indexCount);
  result.size = offset;

  return result;
}

/**
 * Returns the index into the list, analagous to build_strs_index
 * @param sect_strs
 * @param value
 * @returns
 */
export function build_list_index(sect_list: BUILDER_LIST, value: ListItem) : BUILDER_LIST_REF {
  if (!value) {
    return 0; // empty list
  }
  if(!(value instanceof ListItem)) {
    throw new Error('unexpected value '+ value);
  }

  const result = sect_list.lists.findIndex(v => v._value === value);
  if(result < 0) {
    throw new Error('unexpectedly missing ListItem ' + value); // TODO-LDML: it's an array of strs
  }
  return <BUILDER_LIST_REF>result;
}
