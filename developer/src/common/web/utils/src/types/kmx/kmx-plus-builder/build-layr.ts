
import { constants, KMXPlusVersion } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus } from "@keymanapp/common-types";
import { build_strs_index, BUILDER_STR_REF, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_LIST } from "./build-list.js";
import { BUILDER_SECTION } from "./builder-section.js";

import KMXPlusData = KMXPlus.KMXPlusData;
import LayrEntry = KMXPlus.LayrEntry;
import LayrRow = KMXPlus.LayrRow;
import StrsItem = KMXPlus.StrsItem;

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

interface BUILDER_LAYR_FORM extends BUILDER_LAYR_LIST {
  // v19 - renaming from LIST to FORM
  // baseLayout: BUILDER_STR_REF;   // TODO-EMBED-OSK-IN-KMX: do we need to add this now? or to lay3?
  fontFaceName: BUILDER_STR_REF; // face name of font for key caps
  fontSizePct: number;           // font size in % of default size for implementation, typically 100
  flags: number;
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
  // v19
  formCount: number,            // number of entries in forms subtable
  forms: BUILDER_LAYR_FORM[],   // subtable of <layers> elements, roughly same as lists[]
};

export function build_layr(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS, sect_list: BUILDER_LIST): BUILDER_LAYR {
  if (!kmxplus.layr?.lists) {
    return null;  // if there aren't any layers at all (which should be an invalid keyboard)
  }

  const layr: BUILDER_LAYR = {
    ident: constants.hex_section_id(
      kmxplus.version == KMXPlusVersion.Version17
        ? constants.section.layr
        : constants.section.lay2
    ),
    size: 0, // calculated below
    _offset: 0,
    listCount: kmxplus.layr.lists.length,
    layerCount: 0, // calculated below
    rowCount: 0, // calculated below
    keyCount: 0, // calculated below
    lists: [],
    layers: [],
    rows: [],
    keys: [],

    // v19
    formCount: kmxplus.layr.lists.length, // TODO-EMBED-OSK-IN-KMX, rename throughout? == listCount
    forms: [], // superset of lists[]
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

  layr.forms = layr.lists.map(e => ({
    ...e,
    flags: 0,         // TODO-EMBED-OSK-IN-KMX
    fontFaceName: 0,  // TODO-EMBED-OSK-IN-KMX
    fontSizePct: 100, // TODO-EMBED-OSK-IN-KMX
  }));

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

  layr.size = kmxplus.version == KMXPlusVersion.Version17
    ? constants.length_layr +
      (constants.length_layr_list * layr.listCount) +
      (constants.length_layr_entry * layr.layerCount) +
      (constants.length_layr_row * layr.rowCount) +
      (constants.length_layr_key * layr.keyCount)
    : constants.length_lay2 +
      (constants.length_lay2_form * layr.formCount) +
      (constants.length_lay2_entry * layr.layerCount) +
      (constants.length_lay2_row * layr.rowCount) +
      (constants.length_lay2_key * layr.keyCount);
  return layr;
}

