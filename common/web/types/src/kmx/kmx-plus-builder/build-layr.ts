
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { /*KeyFlags,*/ KMXPlusData } from "../kmx-plus.js";
import { /*build_strs_index,*/ BUILDER_STRS } from "./build-strs.js";
import { /*build_list_index,*/ BUILDER_LIST } from "./build-list.js";
import { BUILDER_SECTION } from "./builder-section.js";

/* ------------------------------------------------------------------
 * layr section
   ------------------------------------------------------------------ */


interface BUILDER_LAYR_LIST {
    flags: number;
    hardware: number; // str
    layer: number; // index
    count: number;
};

interface BUILDER_LAYR_LAYER {
    id: number; // str
    modifier: number; // str
    row: number; // row index
    count: number;
};

interface BUILDER_LAYR_ROW {
    key: number;
    count: number;
};

interface BUILDER_LAYR_KEY {
    key: number;
};

/**
 * Builder for the 'keys' section
 */
export interface BUILDER_LAYR extends BUILDER_SECTION {
  listCount: number,
  layerCount: number,
  rowCount: number,
  keyCount: number,
  reserved0: number,
  reserved1: number,
  lists: BUILDER_LAYR_LIST[],
  layers: BUILDER_LAYR_LAYER[],
  rows: BUILDER_LAYR_ROW[],
  keys: BUILDER_LAYR_KEY[],
};


export function build_layr(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS, sect_list: BUILDER_LIST): BUILDER_LAYR {
  if(!kmxplus.layr?.layers && !kmxplus.layr?.lists && !kmxplus.layr?.rows && !kmxplus.layr?.keys) {
    return null;
  }

  let layr: BUILDER_LAYR = {
      ident: constants.hex_section_id(constants.section.key2),
      size: constants.length_layr,
      _offset: 0,
      listCount: 0,
      layerCount: 0,
      rowCount: 0,
      keyCount: 0,
      reserved0: 0,
      reserved1: 0,
      lists: [],
      layers: [],
      rows: [],
      keys: []
  };

  // TODO-LDML

  return layr;
}
