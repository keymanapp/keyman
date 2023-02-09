import { constants } from '@keymanapp/ldml-keyboard-constants';
import * as r from 'restructure';
import { ElementString } from './element-string.js';
import { ListItem } from './string-list.js';
import { unescapeString } from '../util/util.js';
import { KMXFile } from './kmx.js';

// Implementation of file structures from /core/src/ldml/C7043_ldml.md
// Writer in kmx-builder.ts
// Reader in kmx-loader.ts

export class Section {
}

export class GlobalSections {
  // These sections are used by other sections during compilation
  strs: Strs;
  elem: Elem;
  list: List;
}

// 'sect'

export class Sect extends Section {};

// 'bksp' -- see 'tran'

// 'elem'

export class Elem extends Section {
  strings: ElementString[] = [];
  constructor(strs: Strs) {
    super();
    this.strings.push(new ElementString(strs, '')); // C7043: null element string
  }
  allocElementString(strs: Strs, source: string, order?: string, tertiary?: string, tertiary_base?: string, prebase?: string): ElementString {
    let s = new ElementString(strs, source, order, tertiary, tertiary_base, prebase);
    let result = this.strings.find(item => item.isEqual(s));
    if(result === undefined) {
      result = s;
      this.strings.push(result);
    }
    return result;
  }
};

// 'finl' -- see 'tran'

// 'keys'

export enum KeyFlags {
  none = 0,
  extend = constants.keys_flags_extend,  // note, this should be used only when streaming, ignored in-memory
  // additional flags reserved for future use
};

export class KeysItem {
  vkey: number;
  mod: number;
  to: StrsItem;
  flags: KeyFlags;
};

export class Keys extends Section {
  keys: KeysItem[] = [];
};

// 'loca'

export class Loca extends Section {
  locales: StrsItem[] = [];
};

// 'meta'

export enum KeyboardSettings {
  none = 0,
  fallback = constants.meta_settings_fallback_omit,
  transformFailure = constants.meta_settings_transformFailure_omit,
  transformPartial = constants.meta_settings_transformPartial_hide,
};

export enum Meta_NormalizationForm { NFC='NFC', NFD='NFD', other='other' };

export class Meta extends Section {
  author: StrsItem;
  conform: StrsItem;
  layout: StrsItem;
  normalization: StrsItem;
  indicator: StrsItem;
  version: StrsItem; // semver version string, defaults to "0"
  settings: KeyboardSettings;
};

// 'name'

export class Name extends Section {
  names: StrsItem[] = [];
};

// 'ordr'

export class OrdrItem {
  elements: ElementString;
  before: ElementString;
};

export class Ordr extends Section {
  items: OrdrItem[] = [];
};

// 'strs'

/**
 * A string item in memory. This will be replaced with an index
 * into the string table at finalization.
 */
export class StrsItem {
  readonly value: string;
  constructor(value: string) {
    this.value = value;
  }
  compareTo(o: StrsItem): number {
    return StrsItem.binaryStringCompare(this.value, o.value);
  }
  static binaryStringCompare(a: string, b: string): number {
    // https://tc39.es/ecma262/multipage/abstract-operations.html#sec-islessthan
    if(typeof a != 'string' || typeof b != 'string') {
      throw new Error('binaryStringCompare: inputs must be strings');
    }
    if(a < b) return -1;
    if(a > b) return 1;
    return 0;
  }
};

export class Strs extends Section {
  strings: StrsItem[] = [ new StrsItem('') ]; // C7043: The null string is always requierd
  /**
   * Allocate a StrsItem given the string, unescaping if necessary.
   * @param s escaped string
   * @returns
   */
  allocAndUnescapeString(s?: string): StrsItem {
    return this.allocString(unescapeString(s));
  }
  /**
   * Allocate a StrsItem given the string.
   * @param s string
   * @returns
   */
  allocString(s?: string): StrsItem {
    if(s === undefined || s === null) {
      // undefined or null are always equivalent to empty string, see C7043
      s = '';
    }

    if(typeof s !== 'string') {
      throw new Error('alloc_string: s must be a string, undefined, or null.');
    }

    let result = this.strings.find(item => item.value === s);
    if(result === undefined) {
      result = new StrsItem(s);
      this.strings.push(result);
    }
    return result;
  }
};

// 'tran'

export enum TranItemFlags {
  none = 0,
  error = constants.tran_flags_error,
};

export class TranItem extends Section {
  from: ElementString;
  to: StrsItem;
  before: ElementString;
  flags: TranItemFlags;
};

export class Tran extends Section {
  items: TranItem[] = [];
  get id() {
    return constants.section.tran;
  }
};

// alias types for 'bksp', 'finl'

export class Bksp extends Tran {
  override get id() {
    return constants.section.bksp;
  }
};
export class BkspItem extends TranItem {};
export type BkspItemFlags = TranItemFlags;
export const BkspItemFlags = TranItemFlags;

export class Finl extends Tran {
  override get id() {
    return constants.section.finl;
  }
};
export class FinlItem extends TranItem {};
export type FinlItemFlags = TranItemFlags;
export const FinlItemFlags = TranItemFlags;

// 'vkey'

export class VkeyItem {
  vkey: number;
  target: number;
}

export class Vkey extends Section {
  vkeys: VkeyItem[] = [];
};

// 'disp'
export class DispItem {
  to: StrsItem;
  display: StrsItem;
};

export class Disp extends Section {
  baseCharacter: StrsItem;
  disps: DispItem[] = [];
};

// 'layr'

/**
 * In-memory `<layers>`
 */
export class LayrList {
  hardware: number;
  layers: LayrEntry[] = [];
  minDeviceWidth: number; // millimeters
};

/**
 * In-memory `<layer>`
 */
 export class LayrEntry {
  id: StrsItem;
  mod: number;
  rows: LayrRow[] = [];
};

/**
 * In-memory `<row>`
 */
 export class LayrRow {
  keys: StrsItem[] = [];
};

export class Layr extends Section {
  lists: LayrList[] = [];
};

export class Key2Keys {
  flags: number;
  flicks: string; // for in-memory only
  id: StrsItem;
  longPress: ListItem;
  longPressDefault: StrsItem;
  multiTap: ListItem;
  switch: StrsItem;
  to: StrsItem;
  vkey: number;
  width: number;
};

export class Key2Flicks {
  flicks: Key2Flick[] = [];
  id: StrsItem;
  compareTo(b: Key2Flicks): number {
    return this.id.compareTo(b.id);
  }
  constructor(id: StrsItem) {
    this.id = id;
  }
};

export class Key2Flick {
  directions: ListItem;
  flags: number;
  to: StrsItem;
};

export class Key2 extends Section {
  keys: Key2Keys[] = [];
  flicks: Key2Flicks[] = [];
  constructor(strs: Strs) {
    super();
    let nullFlicks = new Key2Flicks(strs.allocString(''));
    this.flicks.push(nullFlicks); // C7043: null element string
  }
};

export class List extends Section {
  /**
   * Allocate a list from a space-separated list of items.
   * Note that passing undefined or null or `''` will
   * end up being the same as the empty list `[]`
   * @param strs Strs section for allocation
   * @param s space-separated list of items
   * @returns a List object
   */
  allocListFromSpaces(strs: Strs, s?: string): ListItem {
    s = s ?? '';
    return this.allocList(strs, s.split(' '));
  }
  allocListFromEscapedSpaces(strs: Strs, s?: string): ListItem {
    if(s === undefined || s === null) {
      s = '';
    }
    return this.allocList(strs, s.split(' ').map(unescapeString));
  }
  /**
   * Return a List object referring to the string list.
   * Note that a falsy list, or a list containing only an empty string
   * `['']` will be stored as an empty list `[]`.
   * @param strs Strs section for allocation
   * @param s string list to allocate
   * @returns
   */
  allocList(strs: Strs, s?: string[]): ListItem {
    // Special case the 'null' list for [] or ['']
    if (!s || (s.length === 1 && s[0] === '')) {
      return this.lists[0];
    }
    let result = this.lists.find(item => item.isEqual(s));
    if(result === undefined) {
      // allocate a new ListItem
      result = new ListItem(strs, s);
      this.lists.push(result);
    }
    return result;
  }
  constructor(strs: Strs) {
    super();
    this.lists.push(new ListItem(strs, [])); // C7043: null element string
  }
  lists: ListItem[] = [];
};

export { ListItem as ListItem };

export interface KMXPlusData {
    sect?: Strs; // sect is ignored in-memory
    bksp?: Bksp;
    disp?: Disp;
    elem?: Elem; // elem is ignored in-memory
    finl?: Finl;
    key2?: Key2;
    keys?: Keys;
    layr?: Layr;
    list?: List; // list is ignored in-memory
    loca?: Loca;
    meta?: Meta;
    name?: Name;
    ordr?: Ordr;
    strs?: Strs; // strs is ignored in-memory
    tran?: Tran;
    vkey?: Vkey;
};

export class KMXPlusFile extends KMXFile {

  /* KMXPlus file structures */

  public readonly COMP_PLUS_SECT_ITEM: any;
  public readonly COMP_PLUS_SECT: any;

  // COMP_PLUS_BKSP == COMP_PLUS_TRAN
  public readonly COMP_PLUS_BKSP_ITEM: any;
  public readonly COMP_PLUS_BKSP: any;

  public readonly COMP_PLUS_DISP_ITEM: any;
  public readonly COMP_PLUS_DISP: any;

  public readonly COMP_PLUS_ELEM_ELEMENT: any;
  public readonly COMP_PLUS_ELEM_STRING: any;
  public readonly COMP_PLUS_ELEM: any;

  // COMP_PLUS_FINL == COMP_PLUS_TRAN
  public readonly COMP_PLUS_FINL_ITEM: any;
  public readonly COMP_PLUS_FINL: any;

  public readonly COMP_PLUS_KEYS_ITEM: any;
  public readonly COMP_PLUS_KEYS: any;

  public readonly COMP_PLUS_LAYR_ENTRY: any;
  public readonly COMP_PLUS_LAYR_KEY: any;
  public readonly COMP_PLUS_LAYR_LIST: any;
  public readonly COMP_PLUS_LAYR_ROW: any;
  public readonly COMP_PLUS_LAYR: any;

  public readonly COMP_PLUS_KEY2_FLICK: any;
  public readonly COMP_PLUS_KEY2_FLICKS: any;
  public readonly COMP_PLUS_KEY2_KEY: any;
  public readonly COMP_PLUS_KEY2: any;

  public readonly COMP_PLUS_LIST_LIST: any;
  public readonly COMP_PLUS_LIST_INDEX: any;
  public readonly COMP_PLUS_LIST: any;

  public readonly COMP_PLUS_LOCA_ITEM: any;
  public readonly COMP_PLUS_LOCA: any;

  public readonly COMP_PLUS_META: any;

  public readonly COMP_PLUS_NAME_ITEM: any;
  public readonly COMP_PLUS_NAME: any;

  public readonly COMP_PLUS_ORDR_ITEM: any;
  public readonly COMP_PLUS_ORDR: any;

  public readonly COMP_PLUS_STRS_ITEM: any;
  public readonly COMP_PLUS_STRS: any;

  public readonly COMP_PLUS_TRAN_ITEM: any;
  public readonly COMP_PLUS_TRAN: any;

  public readonly COMP_PLUS_VKEY_ITEM: any;
  public readonly COMP_PLUS_VKEY: any;

  /* File in-memory data */

  public kmxplus: KMXPlusData = { };

  constructor() {
    super();


    // Binary-correct structures matching kmx_plus.h

    // 'sect'

    this.COMP_PLUS_SECT_ITEM = new r.Struct({
      sect: r.uint32le,
      offset: r.uint32le //? new r.VoidPointer(r.uint32le, {type: 'global'})
    });

    this.COMP_PLUS_SECT = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      total: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_SECT_ITEM, 'count')
    });

    // 'bksp' - see 'tran'

    // 'disp'
    this.COMP_PLUS_DISP_ITEM = new r.Struct({
      to: r.uint32le,
      display: r.uint32le,
    });

    this.COMP_PLUS_DISP = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      baseCharacter: r.uint32le,
      items: new r.Array(this.COMP_PLUS_DISP_ITEM, 'count'),
    });

    // 'elem'

    this.COMP_PLUS_ELEM_ELEMENT = new r.Struct({
      element: r.uint32le,
      flags: r.uint32le
    });

    this.COMP_PLUS_ELEM_STRING = new r.Struct({
      offset: r.uint32le,
      length: r.uint32le
    });

    this.COMP_PLUS_ELEM = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      strings: new r.Array(this.COMP_PLUS_ELEM_STRING, 'count')
      // + variable subtable: Element data (see KMXPlusBuilder.emitElements())
    });

    // 'finl' - see 'tran'

    // 'keys'

    this.COMP_PLUS_KEYS_ITEM = new r.Struct({
      vkey: r.uint32le,
      mod: r.uint32le,
      to: r.uint32le, //str or UTF-32 char depending on value of 'extend'
      flags: r.uint32le, //new r.Bitfield(r.uint32le, ['extend'])
    });

    this.COMP_PLUS_KEYS = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_KEYS_ITEM, 'count')
    });

    // 'layr'

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
      hardware: r.uint32le, //enum
      layer: r.uint32le, // index into layers
      count: r.uint32le,
      minDeviceWidth: r.uint32le, // integer: millimeters
    });

    this.COMP_PLUS_LAYR_ROW = new r.Struct({
      key: r.uint32le,
      count: r.uint32le,
    });

    this.COMP_PLUS_LAYR = new r.Struct({
      ident: r.uint32le,
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

    this.COMP_PLUS_KEY2_FLICK = new r.Struct({
      directions: r.uint32le, // list
      flags: r.uint32le,
      to: r.uint32le, // str | codepoint
    });

    this.COMP_PLUS_KEY2_FLICKS = new r.Struct({
      count: r.uint32le,
      flick: r.uint32le,
      id: r.uint32le, // str
    });

    this.COMP_PLUS_KEY2_KEY = new r.Struct({
      vkey: r.uint32le,
      to: r.uint32le, // str | codepoint
      flags: r.uint32le,
      id: r.uint32le, // str
      switch: r.uint32le, // str
      width: r.uint32le, // width*10  ( 1 = 0.1 keys)
      longPress: r.uint32le, // list index
      longPressDefault: r.uint32le, // str
      multiTap: r.uint32le, // list index
      flicks: r.uint32le, // index into flicks table
    });

    this.COMP_PLUS_KEY2 = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      keyCount: r.uint32le,
      flicksCount: r.uint32le,
      flickCount: r.uint32le,
      keys: new r.Array(this.COMP_PLUS_KEY2_KEY, 'keyCount'),
      flicks: new r.Array(this.COMP_PLUS_KEY2_FLICKS, 'flicksCount'),
      flick: new r.Array(this.COMP_PLUS_KEY2_FLICK, 'flickCount'),
    });

    // 'list'

    this.COMP_PLUS_LIST_LIST = new r.Struct({
      index: r.uint32le,
      count: r.uint32le,
    });

    this.COMP_PLUS_LIST_INDEX = new r.Struct({
      str: r.uint32le, // str
    });

    this.COMP_PLUS_LIST = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      listCount: r.uint32le,
      indexCount: r.uint32le,
      lists: new r.Array(this.COMP_PLUS_LIST_LIST, 'listCount'),
      indices: new r.Array(this.COMP_PLUS_LIST_INDEX, 'indexCount'),
    });

    // 'loca'

    this.COMP_PLUS_LOCA_ITEM = r.uint32le; //str

    this.COMP_PLUS_LOCA = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_LOCA_ITEM, 'count')
    });

    // 'meta'

    this.COMP_PLUS_META = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      author: r.uint32le, //str
      conform: r.uint32le, //str
      layout: r.uint32le, //str
      normalization: r.uint32le, //str
      indicator: r.uint32le, //str
      version: r.uint32le, //str
      settings: r.uint32le, //new r.Bitfield(r.uint32le, ['fallback', 'transformFailure', 'transformPartial'])
    });

    // 'name'

    this.COMP_PLUS_NAME_ITEM = r.uint32le; //str

    this.COMP_PLUS_NAME = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_NAME_ITEM, 'count')
    });

    // 'ordr'

    this.COMP_PLUS_ORDR_ITEM = new r.Struct({
      elements: r.uint32le, //elem
      before: r.uint32le //elem
    });

    this.COMP_PLUS_ORDR = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_ORDR_ITEM, 'count')
    });

    // 'strs'

    this.COMP_PLUS_STRS_ITEM = new r.Struct({
      // While we use length which is number of utf-16 code units excluding null terminator,
      // we always write a null terminator, so we can get restructure to do that for us here
      offset: r.uint32le, //? new r.Pointer(r.uint32le, new r.String(null, 'utf16le')),
      length: r.uint32le
    });

    this.COMP_PLUS_STRS = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_STRS_ITEM, 'count')
      // + variable subtable: String data (see KMXPlusBuilder.emitStrings())
    });

    // 'tran'

    this.COMP_PLUS_TRAN_ITEM = new r.Struct({
      from: r.uint32le, //elem
      to: r.uint32le, //str
      before: r.uint32le, //elem
      flags: r.uint32le //bitfield
    });

    this.COMP_PLUS_TRAN = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_TRAN_ITEM, 'count')
    });

    // 'vkey'

    this.COMP_PLUS_VKEY_ITEM = new r.Struct({
      vkey: r.uint32le,
      target: r.uint32le
    });

    this.COMP_PLUS_VKEY = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_VKEY_ITEM, 'count')
    });

    // Aliases

    this.COMP_PLUS_BKSP_ITEM = this.COMP_PLUS_TRAN_ITEM;
    this.COMP_PLUS_FINL_ITEM = this.COMP_PLUS_TRAN_ITEM;
    this.COMP_PLUS_BKSP = this.COMP_PLUS_TRAN;
    this.COMP_PLUS_FINL = this.COMP_PLUS_TRAN;
  }
}
