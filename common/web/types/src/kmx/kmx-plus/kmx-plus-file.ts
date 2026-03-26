/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-10-08
 *
 * Binary file format Restructure structs for KMX+
 */
import * as KMX from '../kmx.js';
import * as r from 'restructure';
import KMXFile = KMX.KMXFile;
import { KMXPlusVersion } from '@keymanapp/ldml-keyboard-constants';

/* interfaces that match the COMP_PLUS_* structs -- for type safety */

export type IIDENT = number;
export type ISTR_REF = number;
export type ISTR_OR_CHAR32_OR_USET = number;
export type ITABLE_REF = number;

export interface ICOMP_PLUS_SectionHeader {
  ident: IIDENT;
  size: number;
  version?: number;
};

// 'sect'

export interface ICOMP_PLUS_SECT_ITEM {
  sect: IIDENT;
  offset: number;
};

export interface ICOMP_PLUS_SECT {
  header: ICOMP_PLUS_SectionHeader;
  total: number;
  count: number;
  items: ICOMP_PLUS_SECT_ITEM[];
};

// 'bksp' - see 'tran'

// 'disp'

export interface ICOMP_PLUS_DISP_ITEM_v17 {
  to: ISTR_REF;
  id: ISTR_REF;
  display: ISTR_REF;
};

export interface ICOMP_PLUS_DISP_v17 {
  header: ICOMP_PLUS_SectionHeader;
  count: number;
  baseCharacter: ISTR_REF;
  items: ICOMP_PLUS_DISP_ITEM_v17[];
};

export interface ICOMP_PLUS_DISP_ITEM_v19 {
  toId: ISTR_REF;
  display: ISTR_REF;
  flags: number;
};

export interface ICOMP_PLUS_DISP_v19 {
  header: ICOMP_PLUS_SectionHeader;
  count: number;
  baseCharacter: ISTR_REF;
  items: ICOMP_PLUS_DISP_ITEM_v19[];
};

// 'elem'

export interface ICOMP_PLUS_ELEM_ELEMENT {
  element: ISTR_OR_CHAR32_OR_USET;
  flags: number;
};

export interface ICOMP_PLUS_ELEM_STRING {
  offset: number;
  length: number;
};

export interface ICOMP_PLUS_ELEM {
  header: ICOMP_PLUS_SectionHeader;
  count: number;
  strings: ICOMP_PLUS_ELEM_STRING[];
  // + variable subtable: Element data (see KMXPlusBuilder.emitElements())
};

// 'keys'

export type ISTR_OR_CHAR32 = number;
export type ILIST_REF = number;

export interface ICOMP_PLUS_KEYS_FLICK {
  directions: ILIST_REF; // list
  to: ISTR_OR_CHAR32; // str | codepoint
};

export interface ICOMP_PLUS_KEYS_FLICKS {
  count: number;
  flick: ITABLE_REF; // keys.flick index
  id: ISTR_REF; // str
};

export interface ICOMP_PLUS_KEYS_KEY {
  to: ISTR_OR_CHAR32; // str | codepoint
  flags: number;
  id: ISTR_REF; // str
  switch: ISTR_REF; // str
  width: number; // width*10  ( 1 = 0.1 keys)
  longPress: ILIST_REF; // list index
  longPressDefault: ISTR_REF; // str
  multiTap: ILIST_REF; // list index
  flicks: ITABLE_REF; // keys.flicks index
};

export interface ICOMP_PLUS_KEYS_KMAP {
  vkey: number;
  mod: number;
  key: number; // index into 'keys' subtable
};

export interface ICOMP_PLUS_KEYS {
  header: ICOMP_PLUS_SectionHeader;
  keyCount: number;
  flicksCount: number;
  flickCount: number;
  kmapCount: number;
  keys: ICOMP_PLUS_KEYS_KEY[];
  flicks: ICOMP_PLUS_KEYS_FLICKS[];
  flick: ICOMP_PLUS_KEYS_FLICK[];
  kmap: ICOMP_PLUS_KEYS_KMAP[];
};

export interface ICOMP_PLUS_LAYR_ENTRY {
  id: ISTR_REF;    // str
  mod: number;     // bitfield
  row: ITABLE_REF; // layr.rows index
  count: number;
};

export interface ICOMP_PLUS_LAYR_KEY {
  key: ISTR_REF;   // str: key id
};

export interface ICOMP_PLUS_LAYR_FORM_v17 {
  hardware: ISTR_REF; // str: hardware name
  layer: ITABLE_REF;  // layr.layers index
  count: number;
  minDeviceWidth: number; // integer: millimeters
};

export interface ICOMP_PLUS_LAYR_FORM_v19 {
  hardware: ISTR_REF;         // str: hardware name
  layer: ITABLE_REF;          // layr.layrs index
  count: number;
  minDeviceWidth: number;     // integer: millimeters
  baseLayout: ISTR_REF;       // v19: str: identifier for base layout (reserved)
  flags: number;              // v19: flags
};

export interface ICOMP_PLUS_LAYR_ROW {
  key: ITABLE_REF;      // layr.keys index
  count: number;
};

export interface ICOMP_PLUS_LAYR_v17 {
  header: ICOMP_PLUS_SectionHeader;
  formCount: number;
  layerCount: number;
  rowCount: number;
  keyCount: number;
  forms: ICOMP_PLUS_LAYR_FORM_v17[];
  layers: ICOMP_PLUS_LAYR_ENTRY[];
  rows: ICOMP_PLUS_LAYR_ROW[];
  keys: ICOMP_PLUS_LAYR_KEY[];
};

export interface ICOMP_PLUS_LAYR_v19 {
  header: ICOMP_PLUS_SectionHeader;
  formCount: number;
  layerCount: number;
  rowCount: number;
  keyCount: number;
  forms: ICOMP_PLUS_LAYR_FORM_v19[];
  layers: ICOMP_PLUS_LAYR_ENTRY[];
  rows: ICOMP_PLUS_LAYR_ROW[];
  keys: ICOMP_PLUS_LAYR_KEY[];
};

// 'list'

export interface ICOMP_PLUS_LIST_LIST {
  index: ITABLE_REF;  // list.indices index
  count: number;
};

export interface ICOMP_PLUS_LIST_INDEX {
  str: ISTR_REF; // str
};

export interface ICOMP_PLUS_LIST {
  header: ICOMP_PLUS_SectionHeader;
  listCount: number;
  indexCount: number;
  lists: ICOMP_PLUS_LIST_LIST[];
  indices: ICOMP_PLUS_LIST_INDEX[];
};

// 'loca'

export type ICOMP_PLUS_LOCA_ITEM = ISTR_REF; //str

export interface ICOMP_PLUS_LOCA {
  header: ICOMP_PLUS_SectionHeader;
  count: number;
  items: ICOMP_PLUS_LOCA_ITEM[];
};

// 'meta'

export interface ICOMP_PLUS_META {
  header: ICOMP_PLUS_SectionHeader;
  author: ISTR_REF; //str
  conform: ISTR_REF; //str
  layout: ISTR_REF; //str
  name: ISTR_REF; //str
  indicator: ISTR_REF; //str
  version: ISTR_REF; //str
  settings: number; //new r.Bitfield(number, ['normalizationDisabled'])
};

// 'strs'

export interface ICOMP_PLUS_STRS_ITEM {
  // While we use length which is number of utf-16 code units excluding null terminator,
  // we always write a null terminator, so we can get restructure to do that for us here
  offset: number; // offset to utf16le string
  length: number;
};

export interface ICOMP_PLUS_STRS {
  header: ICOMP_PLUS_SectionHeader;
  count: number;
  items: ICOMP_PLUS_STRS_ITEM[];
  // + variable subtable: String data (see KMXPlusBuilder.emitStrings())
};

// 'tran'

export interface ICOMP_PLUS_TRAN_GROUP {
  type: number;      // type of group
  count: number;     // number of items
  index: ITABLE_REF; // tran.transforms or tran.reorders index (per type)
};

export type IELEM_REF = number;

export interface ICOMP_PLUS_TRAN_TRANSFORM {
  from: ISTR_REF; //str
  to: ISTR_REF; //str
  mapFrom: IELEM_REF; //elem
  mapTo: IELEM_REF; //elem
};

export interface ICOMP_PLUS_TRAN_REORDER {
  elements: IELEM_REF; //elem
  before: IELEM_REF; //elem
};

export interface ICOMP_PLUS_TRAN {
  header: ICOMP_PLUS_SectionHeader;
  groupCount: number;
  transformCount: number;
  reorderCount: number;
  groups: ICOMP_PLUS_TRAN_GROUP[];
  transforms: ICOMP_PLUS_TRAN_TRANSFORM[];
  reorders: ICOMP_PLUS_TRAN_REORDER[];
};

// 'uset'

export interface ICOMP_PLUS_USET_USET {
  range: ITABLE_REF;  // uset.ranges
  count: number;
  pattern: ISTR_REF;  // str
};

export type ICHAR32 = number;

export interface ICOMP_PLUS_USET_RANGE {
  start: ICHAR32;
  end: ICHAR32;
};

export interface ICOMP_PLUS_USET {
  header: ICOMP_PLUS_SectionHeader;
  usetCount: number;
  rangeCount: number;
  usets: ICOMP_PLUS_USET_USET[];
  ranges: ICOMP_PLUS_USET_RANGE[];
};

// 'vars'

export interface ICOMP_PLUS_VARS_ITEM {
  type: number;
  id: ISTR_REF; // str
  value: ISTR_REF; // str
  elem: IELEM_REF;
};

export interface ICOMP_PLUS_VARS {
  header: ICOMP_PLUS_SectionHeader;
  markers: ILIST_REF;
  varCount: number;
  varEntries: ICOMP_PLUS_VARS_ITEM[];
};

// Aliases

export type ICOMP_PLUS_BKSP = ICOMP_PLUS_TRAN;

/**
 * Binary representation of KMX+ data, using Restructure. These structures
 * should be directly used only by KMX+ file readers and writers; in general,
 * most things should use the in-memory `KMXPlusData` structures in kmx-plus.ts.
 */
export class KMXPlusFileFormat extends KMXFile {

  /* KMXPlus file structures */

  public readonly COMP_PLUS_SECT_ITEM: any;
  public readonly COMP_PLUS_SECT: any;

  // COMP_PLUS_BKSP == COMP_PLUS_TRAN
  public readonly COMP_PLUS_BKSP_ITEM: any;
  public readonly COMP_PLUS_BKSP: any;

  public readonly COMP_PLUS_DISP_ITEM_v17: any;
  public readonly COMP_PLUS_DISP_v17: any;
  public readonly COMP_PLUS_DISP_ITEM_v19: any;
  public readonly COMP_PLUS_DISP_v19: any;

  public readonly COMP_PLUS_ELEM_ELEMENT: any;
  public readonly COMP_PLUS_ELEM_STRING: any;
  public readonly COMP_PLUS_ELEM: any;

  // COMP_PLUS_KEYS is now COMP_PLUS_KEYS_KMAP

  public readonly COMP_PLUS_LAYR_ENTRY: any;
  public readonly COMP_PLUS_LAYR_KEY: any;
  public readonly COMP_PLUS_LAYR_FORM_v17: any;
  public readonly COMP_PLUS_LAYR_FORM_v19: any;
  public readonly COMP_PLUS_LAYR_ROW: any;
  public readonly COMP_PLUS_LAYR_v17: any;
  public readonly COMP_PLUS_LAYR_v19: any;

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

  public readonly COMP_PLUS_SectionHeader: any;

  constructor(public readonly version: KMXPlusVersion) {
    super();
    // Binary-correct structures matching kmx_plus.h

    if(![KMXPlusVersion.Version17, KMXPlusVersion.Version19].includes(version)) {
      throw new Error(`Support for version ${version} not implemented`);
    }

    // helpers
    const STR_REF       = r.uint32le;
    const ELEM_REF      = r.uint32le;
    const LIST_REF      = r.uint32le;
    const STR_OR_CHAR32 = r.uint32le;
    const CHAR32        = r.uint32le;
    const STR_OR_CHAR32_OR_USET = r.uint32le;
    const IDENT         = r.uint32le;

    // Section header - version dependent

    if(version == KMXPlusVersion.Version17) {
      this.COMP_PLUS_SectionHeader = new r.Struct({
        ident: IDENT,
        size: r.uint32le,
      });
    } else {
      this.COMP_PLUS_SectionHeader = new r.Struct({
        ident: IDENT,
        size: r.uint32le,
        version: r.uint32le,
      });
    }

    // 'sect'

    this.COMP_PLUS_SECT_ITEM = new r.Struct({
      sect: r.uint32le,
      offset: r.uint32le //? new r.VoidPointer(r.uint32le, {type: 'global'})
    });

    this.COMP_PLUS_SECT = new r.Struct({
      header: this.COMP_PLUS_SectionHeader,
      total: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_SECT_ITEM, 'count')
    });

    // 'bksp' - see 'tran'

    // 'disp'
    this.COMP_PLUS_DISP_ITEM_v17 = new r.Struct({
      to: STR_REF,
      id: STR_REF,
      display: STR_REF,
    });

    this.COMP_PLUS_DISP_v17 = new r.Struct({
      header: this.COMP_PLUS_SectionHeader,
      count: r.uint32le,
      baseCharacter: STR_REF,
      items: new r.Array(this.COMP_PLUS_DISP_ITEM_v17, 'count'),
    });

    this.COMP_PLUS_DISP_ITEM_v19 = new r.Struct({
      toId: STR_REF,
      display: STR_REF,
      flags: r.uint32le,
    });

    this.COMP_PLUS_DISP_v19 = new r.Struct({
      header: this.COMP_PLUS_SectionHeader,
      count: r.uint32le,
      baseCharacter: STR_REF,
      items: new r.Array(this.COMP_PLUS_DISP_ITEM_v19, 'count'),
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
      header: this.COMP_PLUS_SectionHeader,
      count: r.uint32le,
      strings: new r.Array(this.COMP_PLUS_ELEM_STRING, 'count')
      // + variable subtable: Element data (see KMXPlusBuilder.emitElements())
    });

    // 'finl' - see 'tran'

    // 'keys' - see 'keys.kmap'

    // 'layr'

    this.COMP_PLUS_LAYR_ENTRY = new r.Struct({
      id: STR_REF, // str
      mod: r.uint32le, // bitfield
      row: r.uint32le, // index into rows
      count: r.uint32le,
    });

    this.COMP_PLUS_LAYR_KEY = new r.Struct({
      key: STR_REF, // str: key id
    });

    this.COMP_PLUS_LAYR_FORM_v17 = new r.Struct({
      hardware: STR_REF, // str: hardware name
      layer: r.uint32le, // index into layers
      count: r.uint32le,
      minDeviceWidth: r.uint32le, // integer: millimeters
    });

    this.COMP_PLUS_LAYR_FORM_v19 = new r.Struct({
      hardware: STR_REF,          // str: hardware name
      layer: r.uint32le,          // index into layers
      count: r.uint32le,
      minDeviceWidth: r.uint32le, // integer: millimeters
      baseLayout: STR_REF,        // v19: str: identifier for base layout (reserved)
      flags: r.uint32le,          // v19: flags
    });

    this.COMP_PLUS_LAYR_ROW = new r.Struct({
      key: r.uint32le,
      count: r.uint32le,
    });

    this.COMP_PLUS_LAYR_v17 = new r.Struct({
      header: this.COMP_PLUS_SectionHeader,
      formCount: r.uint32le,
      layerCount: r.uint32le,
      rowCount: r.uint32le,
      keyCount: r.uint32le,
      forms: new r.Array(this.COMP_PLUS_LAYR_FORM_v17, 'formCount'),
      layers: new r.Array(this.COMP_PLUS_LAYR_ENTRY, 'layerCount'),
      rows: new r.Array(this.COMP_PLUS_LAYR_ROW, 'rowCount'),
      keys: new r.Array(this.COMP_PLUS_LAYR_KEY, 'keyCount'),
    });

    this.COMP_PLUS_LAYR_v19 = new r.Struct({
      header: this.COMP_PLUS_SectionHeader,
      formCount: r.uint32le,
      layerCount: r.uint32le,
      rowCount: r.uint32le,
      keyCount: r.uint32le,
      forms: new r.Array(this.COMP_PLUS_LAYR_FORM_v19, 'formCount'),
      layers: new r.Array(this.COMP_PLUS_LAYR_ENTRY, 'layerCount'),
      rows: new r.Array(this.COMP_PLUS_LAYR_ROW, 'rowCount'),
      keys: new r.Array(this.COMP_PLUS_LAYR_KEY, 'keyCount'),
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
      header: this.COMP_PLUS_SectionHeader,
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
      header: this.COMP_PLUS_SectionHeader,
      listCount: r.uint32le,
      indexCount: r.uint32le,
      lists: new r.Array(this.COMP_PLUS_LIST_LIST, 'listCount'),
      indices: new r.Array(this.COMP_PLUS_LIST_INDEX, 'indexCount'),
    });

    // 'loca'

    this.COMP_PLUS_LOCA_ITEM = r.uint32le; //str

    this.COMP_PLUS_LOCA = new r.Struct({
      header: this.COMP_PLUS_SectionHeader,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_LOCA_ITEM, 'count')
    });

    // 'meta'

    this.COMP_PLUS_META = new r.Struct({
      header: this.COMP_PLUS_SectionHeader,
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
      header: this.COMP_PLUS_SectionHeader,
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
      header: this.COMP_PLUS_SectionHeader,
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
      header: this.COMP_PLUS_SectionHeader,
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
      header: this.COMP_PLUS_SectionHeader,
      markers: LIST_REF,
      varCount: r.uint32le,
      varEntries: new r.Array(this.COMP_PLUS_VARS_ITEM, 'varCount'),
    });

    // 'vkey' is removed

    // Aliases

    this.COMP_PLUS_BKSP = this.COMP_PLUS_TRAN;
  }
}
