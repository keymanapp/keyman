
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Bksp, Tran } from "../kmx-plus.js";
import { BUILDER_ELEM, build_elem_index } from "./build-elem.js";
import { BUILDER_STRS, build_strs_index } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";

/* ------------------------------------------------------------------
* tran section
  ------------------------------------------------------------------ */

interface BUILDER_TRAN_GROUP {
  type: number; //type of group
  count: number; //count
  index: number; //index into subtable
};

interface BUILDER_TRAN_TRANSFORM {
  from: number; //str
  to: number; //str
  mapFrom: number; // elem
  mapTo: number; // elem
};

interface BUILDER_TRAN_REORDER {
  elements: number; //elem
  before: number; //elem
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

  let tran: BUILDER_TRAN = {
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

  for (let group of source_tran.groups) {
    tran.groups.push({
      type: group.type,
      count: group.reorders.length + group.transforms.length,
      index: tran.groups.length,
    });
    for (let transform of group.transforms) {
      tran.transforms.push({
        from: build_strs_index(sect_strs, transform.from),
        to: build_strs_index(sect_strs, transform.to),
        mapFrom: build_strs_index(sect_strs, transform.mapFrom),
        mapTo: build_strs_index(sect_strs, transform.mapTo),
      });
    }
    for (let reorder of group.reorders) {
      tran.reorders.push({
        elements: build_elem_index(sect_elem, reorder.elements),
        before: build_elem_index(sect_elem, reorder.before),
      });
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
