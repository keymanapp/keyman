
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { /*KeyFlags,*/ KMXPlusData } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STRS } from "./build-strs.js";
import { /*build_list_index*/ BUILDER_LIST } from "./build-list.js";
import { BUILDER_SECTION } from "./builder-section.js";

/* ------------------------------------------------------------------
 * layr section
   ------------------------------------------------------------------ */


interface BUILDER_LAYR_LIST {
    flags: number;
    hardware: number; // str
    layer: number; // index
    count: number;
    minDeviceWidth: number;
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
  if (!kmxplus.layr?.lists) {
    return null;
  }

  let layr: BUILDER_LAYR = {
      ident: constants.hex_section_id(constants.section.key2),
      size: constants.length_layr,
      _offset: 0,
      listCount: kmxplus.layr.lists.length,
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

  // pre-sort layers
  kmxplus.layr.lists.sort((a, b) => {
    const aform = a.flags & constants.layr_list_flags_mask_form;
    const bform = b.flags & constants.layr_list_flags_mask_form;
    if (aform < bform) {
        return -1;
    } else if(aform > bform) {
        return 1;
    }
    if (a.minDeviceWidth < b.minDeviceWidth) {
        return -1;
    } else if(a.minDeviceWidth > b.minDeviceWidth) {
        return 1;
    } else {
        return 0; // same
    }
  });

  layr.lists = kmxplus.layr.lists.map((list) => {
    const blist : BUILDER_LAYR_LIST = {
        flags: list.flags,
        hardware: build_strs_index(sect_strs, list.hardware),
        layer: layr.layers.length,
        count: list.layers.length,
        minDeviceWidth: list.minDeviceWidth,
    };
    list.layers.forEach((layer) => {
        const blayer : BUILDER_LAYR_LAYER = {
            id: build_strs_index(sect_strs, layer.id),
            modifier: build_strs_index(sect_strs, layer.modifier),
            row: layr.rows.length,
            count: layer.rows.length,
        };
        layer.rows.forEach((row) => {
            const brow : BUILDER_LAYR_ROW = {
                key: layr.keys.length,
                count: row.keys.length,
            };
            row.keys.forEach((key) => {
                const bkey : BUILDER_LAYR_KEY = {
                    key: build_strs_index(sect_strs, key),
                };
                layr.keys.push(bkey);
                layr.keyCount++;
            });
            layr.rows.push(brow);
            layr.rowCount++;
        });
        layr.layers.push(blayer);
        layr.layerCount++;
    });

    return blist;
  });

  let offset = constants.length_layr +
    (constants.length_layr_list * layr.listCount) +
    (constants.length_layr_entry * layr.layerCount) +
    (constants.length_layr_row * layr.rowCount) +
    (constants.length_layr_key * layr.keyCount);
  layr.size = offset;
  return layr;
}
