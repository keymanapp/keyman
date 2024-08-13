
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlusData, LayrEntry, LayrRow, StrsItem } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STR_REF, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_LIST } from "./build-list.js";
import { BUILDER_SECTION } from "./builder-section.js";

/* ------------------------------------------------------------------
 * layr section -
 ------------------------------------------------------------------ */

/**
 * List of layers, the <layers> element
 */
interface BUILDER_LAYR_LIST {
  hardware: BUILDER_STR_REF; // hardware or 'touch'
  layer: number; // index of first layer in the list, in the
  count: number; // number of layer entries in the list
  minDeviceWidth: number; // width in millimeters
  _layers: LayrEntry[]; // original layer entry, for in-memory only
};

/**
 * <layer> element
 */
interface BUILDER_LAYR_LAYER {
  id: BUILDER_STR_REF; // str of layer id
  _id: string; // original layer id, for sorting
  mod: number; // bitfield with modifier info
  row: number; // row index into row subtable
  _rows: LayrRow[]; // original rows, for in-memory only
  count: number; // number of row entries
};

/**
 * <row> element
 */
interface BUILDER_LAYR_ROW {
  key: number; // index into key subtable
  count: number; // number of keys
};

/**
 * portion of keys attribute of <row>
 */
interface BUILDER_LAYR_KEY {
  key: number;
};

/**
 * Builder for the 'keys' section
 */
export interface BUILDER_LAYR extends BUILDER_SECTION {
  listCount: number, // number of entries in lists subtable
  layerCount: number, // number of entries in layers subtable
  rowCount: number, // number of entries in rows subtable
  keyCount: number, // number of entries in keys subtable
  lists: BUILDER_LAYR_LIST[], // subtable of <layers> elements
  layers: BUILDER_LAYR_LAYER[], // subtable of <layer> elements
  rows: BUILDER_LAYR_ROW[], // subtable of <row> elements
  keys: BUILDER_LAYR_KEY[], // subtable of key entries
};

export function build_layr(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS, sect_list: BUILDER_LIST): BUILDER_LAYR {
  if (!kmxplus.layr?.lists) {
    return null;  // if there aren't any layers at all (which should be an invalid keyboard)
  }

  let layr: BUILDER_LAYR = {
    ident: constants.hex_section_id(constants.section.layr),
    size: constants.length_layr,
    _offset: 0,
    listCount: kmxplus.layr.lists.length,
    layerCount: 0, // calculated below
    rowCount: 0, // calculated below
    keyCount: 0, // calculated below
    lists: [],
    layers: [],
    rows: [],
    keys: []
  };

  layr.lists = kmxplus.layr.lists.map((list) => {
    const blist: BUILDER_LAYR_LIST = {
      hardware: build_strs_index(sect_strs, list.hardware),
      layer: null, // to be set below
      _layers: list.layers,
      count: list.layers.length,
      minDeviceWidth: list.minDeviceWidth,
    };
    return blist;
  });
  // now sort the lists
  layr.lists.sort((a, b) => {
    // sort by string #
    if (a.hardware < b.hardware) {
      return -1;
    } else if (a.hardware > b.hardware) {
      return 1;
    }
    if (a.minDeviceWidth < b.minDeviceWidth) {
      return -1;
    } else if (a.minDeviceWidth > b.minDeviceWidth) {
      return 1;
    } else {
      return 0; // same
    }
  });
  // Now allocate the layers, rows, and keys
  layr.lists.forEach((list) => {
    list.layer = layr.layers.length; // index to first layer in list
    const blayers = list._layers.map((layer) => {
      const blayer: BUILDER_LAYR_LAYER = {
        _id: layer.id.value, // original id
        id: build_strs_index(sect_strs, layer.id),
        mod: layer.mod,
        row: null, // row ID, to be filled in
        _rows: layer.rows, // temporary
        count: layer.rows.length, // number of rows
      };
      return blayer;
    });
    // sort the new layers
    blayers.sort((a, b) => StrsItem.binaryStringCompare(a._id, b._id));
    blayers.forEach((layer) => {
      layer.row = layr.rows.length; // index to first row in list
      layer._rows.forEach((row) => {
        const brow: BUILDER_LAYR_ROW = {
          key: layr.keys.length,
          count: row.keys.length,
        };
        row.keys.forEach((key) => {
          const bkey: BUILDER_LAYR_KEY = {
            key: build_strs_index(sect_strs, key),
          };
          layr.keys.push(bkey);
        });
        layr.rows.push(brow);
      });
      layr.layers.push(layer);
    });
  });

  layr.layerCount = layr.layers.length;
  layr.rowCount = layr.rows.length;
  layr.keyCount = layr.keys.length;

  let offset = constants.length_layr +
    (constants.length_layr_list * layr.listCount) +
    (constants.length_layr_entry * layr.layerCount) +
    (constants.length_layr_row * layr.rowCount) +
    (constants.length_layr_key * layr.keyCount);
  layr.size = offset;
  return layr;
}
