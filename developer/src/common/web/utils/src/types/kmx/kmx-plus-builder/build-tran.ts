
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus } from "@keymanapp/common-types";
import { BUILDER_ELEM, BUILDER_ELEM_REF, build_elem_index } from "./build-elem.js";
import { BUILDER_STRS, BUILDER_STR_REF, build_strs_index } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";

import Bksp = KMXPlus.Bksp;
import Tran = KMXPlus.Tran;

/* ------------------------------------------------------------------
* tran section
  ------------------------------------------------------------------ */

interface BUILDER_TRAN_GROUP {
  type: number; //type of group
  count: number; //count
  index: number; //index into subtable
};

interface BUILDER_TRAN_TRANSFORM {
  from: BUILDER_STR_REF; //str
  to: BUILDER_STR_REF; //str
  mapFrom: BUILDER_ELEM_REF; // elem
  mapTo: BUILDER_ELEM_REF; // elem
};

interface BUILDER_TRAN_REORDER {
  elements: BUILDER_ELEM_REF; //elem
  before: BUILDER_ELEM_REF; //elem
};

export interface BUILDER_TRAN extends BUILDER_SECTION {
  groupCount: number;
  transformCount: number;
  reorderCount: number;
  groups: BUILDER_TRAN_GROUP[];
  transforms: BUILDER_TRAN_TRANSFORM[];
  reorders: BUILDER_TRAN_REORDER[];
};

/**
* Builder for the 'tran', 'finl', and 'bksp' sections, all of which use the BUILDER_TRAN layout
*/
export function build_tran(source_tran: Tran | Bksp, sect_strs: BUILDER_STRS, sect_elem: BUILDER_ELEM): BUILDER_TRAN {
  if (!source_tran?.groups?.length) {
    return null;
  }

  const tran: BUILDER_TRAN = {
    ident: constants.hex_section_id(source_tran.id),
    size: 0, // need to compute total transforms + reorders
    _offset: 0,
    groupCount: source_tran.groups.length,
    transformCount: 0,
    reorderCount: 0,
    groups: [],
    transforms: [],
    reorders: [],
  };

  for (const group of source_tran.groups) {
    if (group.type === constants.tran_group_type_transform) {
      tran.groups.push({
        type: group.type,
        count: group.transforms.length,
        index: tran.transforms.length, // index of first item
      });
      for (const transform of group.transforms) {
        tran.transforms.push({
          from: build_strs_index(sect_strs, transform.from),
          to: build_strs_index(sect_strs, transform.to),
          mapFrom: build_strs_index(sect_strs, transform.mapFrom),
          mapTo: build_strs_index(sect_strs, transform.mapTo),
        });
      }
    } else if (group.type === constants.tran_group_type_reorder) {
      tran.groups.push({
        type: group.type,
        count: group.reorders.length,
        index: tran.reorders.length, // index of first item
      });
      for (const reorder of group.reorders) {
        tran.reorders.push({
          elements: build_elem_index(sect_elem, reorder.elements),
          before: build_elem_index(sect_elem, reorder.before),
        });
      }
    } else {
      throw new Error(`Internal error: at transformGroup #${tran.groups.length + 1}: Unexpected transformGroup type ${group.type}`);
    }
  }
  // now set the sizes
  tran.transformCount = tran.transforms.length;
  tran.reorderCount = tran.reorders.length;

  tran.size = constants.length_tran +
    (constants.length_tran_group * source_tran.groups.length) +
    (constants.length_tran_transform * tran.transforms.length) +
    (constants.length_tran_reorder * tran.reorders.length);

  return tran;
}
