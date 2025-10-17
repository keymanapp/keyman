/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Binary file format Restructure structs for KMX+
 */
import * as KMX from '../kmx.js';
import * as r from 'restructure';
import KMXFile = KMX.KMXFile;

export class KMXPlusFileFormat extends KMXFile {

  /* KMXPlus file structures */

  public readonly COMP_PLUS_SECT_ITEM: any;
  public readonly COMP_PLUS_SECT: any;

  // COMP_PLUS_BKSP == COMP_PLUS_TRAN
  public readonly COMP_PLUS_BKSP_ITEM: any;
  public readonly COMP_PLUS_BKSP: any;

  // DISP: v17

  public readonly COMP_PLUS_DISP_ITEM: any;
  public readonly COMP_PLUS_DISP: any;

  // DISP2: v19, replaces DISP

  public readonly COMP_PLUS_DIS2_ITEM: any;
  public readonly COMP_PLUS_DIS2: any;

  public readonly COMP_PLUS_ELEM_ELEMENT: any;
  public readonly COMP_PLUS_ELEM_STRING: any;
  public readonly COMP_PLUS_ELEM: any;

  // COMP_PLUS_KEYS is now COMP_PLUS_KEYS_KMAP

  public readonly COMP_PLUS_LAYR_ENTRY: any;
  public readonly COMP_PLUS_LAYR_KEY: any;
  public readonly COMP_PLUS_LAYR_LIST: any;
  public readonly COMP_PLUS_LAYR_ROW: any;
  public readonly COMP_PLUS_LAYR: any;

  // LAY2: v19, replaces LAYR

  public readonly COMP_PLUS_LAY2_ENTRY: any;
  public readonly COMP_PLUS_LAY2_KEY: any;
  public readonly COMP_PLUS_LAY2_FORM: any;
  public readonly COMP_PLUS_LAY2_ROW: any;
  public readonly COMP_PLUS_LAY2: any;

  // KEYS

  public readonly COMP_PLUS_KEYS_FLICK: any;
  public readonly COMP_PLUS_KEYS_FLICKS: any;
  public readonly COMP_PLUS_KEYS_KEY: any;
  public readonly COMP_PLUS_KEYS_KMAP: any;
  public readonly COMP_PLUS_KEYS: any;

  public readonly COMP_PLUS_LIST_LIST: any;
  public readonly COMP_PLUS_LIST_INDEX: any;
  public readonly COMP_PLUS_LIST: any;

  public readonly COMP_PLUS_LOCA_ITEM: any;
  public readonly COMP_PLUS_LOCA: any;

  public readonly COMP_PLUS_META: any;

  public readonly COMP_PLUS_STRS_ITEM: any;
  public readonly COMP_PLUS_STRS: any;

  public readonly COMP_PLUS_TRAN_GROUP: any;
  public readonly COMP_PLUS_TRAN_TRANSFORM: any;
  public readonly COMP_PLUS_TRAN_REORDER: any;
  public readonly COMP_PLUS_TRAN: any;

  public readonly COMP_PLUS_USET_USET: any;
  public readonly COMP_PLUS_USET_RANGE: any;
  public readonly COMP_PLUS_USET: any;

  public readonly COMP_PLUS_VKEY_ITEM: any;
  public readonly COMP_PLUS_VKEY: any;

  public readonly COMP_PLUS_VARS: any;
  public readonly COMP_PLUS_VARS_ITEM: any;

  constructor() {
    super();
    // Binary-correct structures matching kmx_plus.h

    // helpers
    const STR_REF       = r.uint32le;
    const ELEM_REF      = r.uint32le;
    const LIST_REF      = r.uint32le;
    const STR_OR_CHAR32 = r.uint32le;
    const CHAR32        = r.uint32le;
    const STR_OR_CHAR32_OR_USET = r.uint32le;
    const IDENT         = r.uint32le;
    // 'sect'

    this.COMP_PLUS_SECT_ITEM = new r.Struct({
      sect: r.uint32le,
      offset: r.uint32le //? new r.VoidPointer(r.uint32le, {type: 'global'})
    });

    this.COMP_PLUS_SECT = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      total: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_SECT_ITEM, 'count')
    });

    // 'bksp' - see 'tran'

    // 'disp' (v17)

    this.COMP_PLUS_DISP_ITEM = new r.Struct({
      to: STR_REF,
      id: STR_REF,
      display: STR_REF,
    });

    this.COMP_PLUS_DISP = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      count: r.uint32le,
      baseCharacter: CHAR32,
      items: new r.Array(this.COMP_PLUS_DISP_ITEM, 'count'),
    });

    // 'dis2' (v19, replaces 'disp')

    this.COMP_PLUS_DIS2_ITEM = new r.Struct({
      id: STR_REF,
      display: STR_REF,
      flags: r.uint32le,
    });

    this.COMP_PLUS_DIS2 = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      count: r.uint32le,
      baseCharacter: CHAR32,
      items: new r.Array(this.COMP_PLUS_DIS2_ITEM, 'count'),
    });

    // 'elem'

    this.COMP_PLUS_ELEM_ELEMENT = new r.Struct({
      element: STR_OR_CHAR32_OR_USET,
      flags: r.uint32le
    });

    this.COMP_PLUS_ELEM_STRING = new r.Struct({
      offset: r.uint32le,
      length: r.uint32le
    });

    this.COMP_PLUS_ELEM = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      count: r.uint32le,
      strings: new r.Array(this.COMP_PLUS_ELEM_STRING, 'count')
      // + variable subtable: Element data (see KMXPlusBuilder.emitElements())
    });

    // 'finl' - see 'tran'

    // 'keys' - see 'keys.kmap'

    // 'layr' (v17)

    this.COMP_PLUS_LAYR_ENTRY = new r.Struct({
      id: r.uint32le, // str
      mod: r.uint32le, // bitfield
      row: r.uint32le, // index into rows
      count: r.uint32le,
    });

    this.COMP_PLUS_LAYR_KEY = new r.Struct({
      key: r.uint32le, // str: key id
    });

    this.COMP_PLUS_LAYR_LIST = new r.Struct({
      hardware: STR_REF, // str: hardware name
      layer: r.uint32le, // index into layers
      count: r.uint32le,
      minDeviceWidth: r.uint32le, // integer: millimeters
    });

    this.COMP_PLUS_LAYR_ROW = new r.Struct({
      key: r.uint32le,
      count: r.uint32le,
    });

    this.COMP_PLUS_LAYR = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      listCount: r.uint32le,
      layerCount: r.uint32le,
      rowCount: r.uint32le,
      keyCount: r.uint32le,
      lists: new r.Array(this.COMP_PLUS_LAYR_LIST, 'listCount'),
      layers: new r.Array(this.COMP_PLUS_LAYR_ENTRY, 'layerCount'),
      rows: new r.Array(this.COMP_PLUS_LAYR_ROW, 'rowCount'),
      keys: new r.Array(this.COMP_PLUS_LAYR_KEY, 'keyCount'),
    });

    // 'lay2' (v19, replaces 'layr')

    this.COMP_PLUS_LAY2_ENTRY = new r.Struct({
      id: r.uint32le, // str
      mod: r.uint32le, // bitfield
      row: r.uint32le, // index into rows
      count: r.uint32le,
    });

    this.COMP_PLUS_LAY2_KEY = new r.Struct({
      key: r.uint32le, // str: key id
    });

    this.COMP_PLUS_LAY2_FORM = new r.Struct({
      hardware: STR_REF,          // str: hardware name
      layer: r.uint32le,          // index into layers
      count: r.uint32le,
      minDeviceWidth: r.uint32le, // integer: millimeters
      baseLayout: STR_REF,        // str: identifier for base layout (reserved)
      fontFaceName: STR_REF,      // str: font face name
      fontSizePct: r.uint32le,    // font size in % of default size
      flags: r.uint32le,
    });

    this.COMP_PLUS_LAY2_ROW = new r.Struct({
      key: r.uint32le,
      count: r.uint32le,
    });

    this.COMP_PLUS_LAY2 = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      formCount: r.uint32le,
      layerCount: r.uint32le,
      rowCount: r.uint32le,
      keyCount: r.uint32le,
      forms: new r.Array(this.COMP_PLUS_LAY2_FORM, 'formCount'),
      layers: new r.Array(this.COMP_PLUS_LAY2_ENTRY, 'layerCount'),
      rows: new r.Array(this.COMP_PLUS_LAY2_ROW, 'rowCount'),
      keys: new r.Array(this.COMP_PLUS_LAY2_KEY, 'keyCount'),
    });

    // 'keys'

    this.COMP_PLUS_KEYS_FLICK = new r.Struct({
      directions: LIST_REF, // list
      to: STR_OR_CHAR32, // str | codepoint
    });

    this.COMP_PLUS_KEYS_FLICKS = new r.Struct({
      count: r.uint32le,
      flick: r.uint32le,
      id: STR_REF, // str
    });

    this.COMP_PLUS_KEYS_KEY = new r.Struct({
      to: STR_OR_CHAR32, // str | codepoint
      flags: r.uint32le,
      id: STR_REF, // str
      switch: STR_REF, // str
      width: r.uint32le, // width*10  ( 1 = 0.1 keys)
      longPress: LIST_REF, // list index
      longPressDefault: STR_REF, // str
      multiTap: LIST_REF, // list index
      flicks: r.uint32le, // index into flicks table
    });

    this.COMP_PLUS_KEYS_KMAP = new r.Struct({
      vkey: r.uint32le,
      mod: r.uint32le,
      key: r.uint32le, // index into 'keys' subtable
    });

    this.COMP_PLUS_KEYS = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      keyCount: r.uint32le,
      flicksCount: r.uint32le,
      flickCount: r.uint32le,
      kmapCount: r.uint32le,
      keys: new r.Array(this.COMP_PLUS_KEYS_KEY, 'keyCount'),
      flicks: new r.Array(this.COMP_PLUS_KEYS_FLICKS, 'flicksCount'),
      flick: new r.Array(this.COMP_PLUS_KEYS_FLICK, 'flickCount'),
      kmap: new r.Array(this.COMP_PLUS_KEYS_KMAP, 'kmapCount'),
    });

    // 'list'

    this.COMP_PLUS_LIST_LIST = new r.Struct({
      index: r.uint32le,
      count: r.uint32le,
    });

    this.COMP_PLUS_LIST_INDEX = new r.Struct({
      str: STR_REF, // str
    });

    this.COMP_PLUS_LIST = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      listCount: r.uint32le,
      indexCount: r.uint32le,
      lists: new r.Array(this.COMP_PLUS_LIST_LIST, 'listCount'),
      indices: new r.Array(this.COMP_PLUS_LIST_INDEX, 'indexCount'),
    });

    // 'loca'

    this.COMP_PLUS_LOCA_ITEM = r.uint32le; //str

    this.COMP_PLUS_LOCA = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_LOCA_ITEM, 'count')
    });

    // 'meta'

    this.COMP_PLUS_META = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      author: STR_REF, //str
      conform: STR_REF, //str
      layout: STR_REF, //str
      name: STR_REF, //str
      indicator: STR_REF, //str
      version: STR_REF, //str
      settings: r.uint32le, //new r.Bitfield(r.uint32le, ['normalizationDisabled'])
    });

    // 'name' is gone

    // 'ordr' now part of 'tran'

    // 'strs'

    this.COMP_PLUS_STRS_ITEM = new r.Struct({
      // While we use length which is number of utf-16 code units excluding null terminator,
      // we always write a null terminator, so we can get restructure to do that for us here
      offset: r.uint32le, //? new r.Pointer(r.uint32le, new r.String(null, 'utf16le')),
      length: r.uint32le
    });

    this.COMP_PLUS_STRS = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_STRS_ITEM, 'count')
      // + variable subtable: String data (see KMXPlusBuilder.emitStrings())
    });

    // 'tran'

    this.COMP_PLUS_TRAN_GROUP = new r.Struct({
      type: r.uint32le, //type of group
      count: r.uint32le, //number of items
      index: r.uint32le, //index into subtable
    });

    this.COMP_PLUS_TRAN_TRANSFORM = new r.Struct({
      from: STR_REF, //str
      to: STR_REF, //str
      mapFrom: ELEM_REF, //elem
      mapTo: ELEM_REF //elem
    });

    this.COMP_PLUS_TRAN_REORDER = new r.Struct({
      elements: ELEM_REF, //elem
      before: ELEM_REF, //elem
    });

    this.COMP_PLUS_TRAN = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      groupCount: r.uint32le,
      transformCount: r.uint32le,
      reorderCount: r.uint32le,
      groups: new r.Array(this.COMP_PLUS_TRAN_GROUP, 'groupCount'),
      transforms: new r.Array(this.COMP_PLUS_TRAN_TRANSFORM, 'transformCount'),
      reorders: new r.Array(this.COMP_PLUS_TRAN_REORDER, 'reorderCount'),
    });

    // 'uset'

    this.COMP_PLUS_USET_USET = new r.Struct({
      range: r.uint32le,
      count: r.uint32le,
      pattern: STR_REF, // str
    });

    this.COMP_PLUS_USET_RANGE = new r.Struct({
      start: CHAR32,
      end: CHAR32,
    });

    this.COMP_PLUS_USET = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      usetCount: r.uint32le,
      rangeCount: r.uint32le,
      usets: new r.Array(this.COMP_PLUS_USET_USET, 'usetCount'),
      ranges: new r.Array(this.COMP_PLUS_USET_RANGE, 'rangeCount'),
    });

    // 'vars'

    this.COMP_PLUS_VARS_ITEM = new r.Struct({
      type: r.uint32le,
      id: STR_REF, // str
      value: STR_REF, // str
      elem: ELEM_REF,
    });

    this.COMP_PLUS_VARS = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      markers: LIST_REF,
      varCount: r.uint32le,
      varEntries: new r.Array(this.COMP_PLUS_VARS_ITEM, 'varCount'),
    });

    // 'vkey' is removed

    // Aliases

    this.COMP_PLUS_BKSP = this.COMP_PLUS_TRAN;
  }
}
