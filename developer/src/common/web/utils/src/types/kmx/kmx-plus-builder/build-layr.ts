
import { constants, KMXPlusVersion } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus } from "@keymanapp/common-types";
import { build_strs_index, BUILDER_STR_REF, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";

import KMXPlusData = KMXPlus.KMXPlusData;
import LayrEntry = KMXPlus.LayrEntry;
import LayrRow = KMXPlus.LayrRow;
import StrsItem = KMXPlus.StrsItem;

/* ------------------------------------------------------------------
 * layr section -
 ------------------------------------------------------------------ */

/**
 * A form that contains a set of layers, the <layers> element
 */
interface BUILDER_LAYR_FORM {
  hardware: BUILDER_STR_REF; // hardware or 'touch'
  layer: number; // index of first layer in the form, in the layers subtable
  count: number; // number of layer entries in the form
  minDeviceWidth: number; // width in millimeters
  _layers: LayrEntry[]; // original layer entry, for in-memory only
  baseLayout: BUILDER_STR_REF;   // v19: base layout (e.g. 'us')
  fontFaceName: BUILDER_STR_REF; // v19: face name of font for key caps
  fontSizePct: number;           // v19: font size in % of default size for implementation, typically 100
  flags: KMXPlus.LayrFormFlags;  // v19: flags
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
  formCount: number, // number of entries in forms subtable (renamed in v19, formerly, listCount)
  layerCount: number, // number of entries in layers subtable
  rowCount: number, // number of entries in rows subtable
  keyCount: number, // number of entries in keys subtable
  forms: BUILDER_LAYR_FORM[], // subtable of <layers> elements (renamed in v19, formerly, lists)
  layers: BUILDER_LAYR_LAYER[], // subtable of <layer> elements
  rows: BUILDER_LAYR_ROW[], // subtable of <row> elements
  keys: BUILDER_LAYR_KEY[], // subtable of key entries
};

export function build_layr(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS, version: KMXPlusVersion): BUILDER_LAYR {
  if (!kmxplus.layr?.forms) {
    return null;  // if there aren't any layers at all (which should be an invalid keyboard)
  }

  const layr: BUILDER_LAYR = {
    header: {
      ident: constants.hex_section_id(constants.section.layr),
      size: 0, // calculated below
      version: version == KMXPlusVersion.Version17 ? KMXPlusVersion.Version17 : KMXPlusVersion.Version19,
    },
    _offset: 0,
    formCount: kmxplus.layr.forms.length,
    layerCount: 0, // calculated below
    rowCount: 0, // calculated below
    keyCount: 0, // calculated below
    forms: [],
    layers: [],
    rows: [],
    keys: [],
  };

  layr.forms = kmxplus.layr.forms.map<BUILDER_LAYR_FORM>((form) => {
    return {
      hardware: build_strs_index(sect_strs, form.hardware),
      layer: null, // to be set below
      _layers: form.layers,
      count: form.layers.length,
      minDeviceWidth: form.minDeviceWidth,
      baseLayout: build_strs_index(sect_strs, form.baseLayout),
      fontFaceName: build_strs_index(sect_strs, form.fontFaceName),
      fontSizePct: form.fontSizePct,
      flags: form.flags,
    };
  });

  // now sort the forms
  layr.forms.sort((a, b) => {
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
  layr.forms.forEach((form) => {
    form.layer = layr.layers.length; // index to first layer in form
    const blayers = form._layers.map((layer) => {
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
      layer.row = layr.rows.length; // index to first row in form
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

  layr.header.size = version == KMXPlusVersion.Version17
    ? constants.length_layr +
      (constants.length_layr_form_v17 * layr.formCount) +
      (constants.length_layr_entry * layr.layerCount) +
      (constants.length_layr_row * layr.rowCount) +
      (constants.length_layr_key * layr.keyCount)
    : constants.length_layr +
      (constants.length_layr_form_v19 * layr.formCount) +
      (constants.length_layr_entry * layr.layerCount) +
      (constants.length_layr_row * layr.rowCount) +
      (constants.length_layr_key * layr.keyCount);
  return layr;
}
