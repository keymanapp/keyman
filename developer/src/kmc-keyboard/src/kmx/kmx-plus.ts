import { constants } from '@keymanapp/ldml-keyboard-constants';
import * as r from 'restructure';
import { ElementString } from './element-string.js';

import KMXFile from './kmx.js';

// Implementation of file structures from /core/src/ldml/C7043_ldml.md
// Writer in kmx-builder.ts
// Reader in kmx-loader.ts

export class Section {
}

export class GlobalSections {
  // These sections are used by other sections during compilation
  strs: Strs;
  elem: Elem;
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

export class StrsItem {
  readonly value: string;
  constructor(value: string) {
    this.value = value;
  }
}

export class Strs extends Section {
  strings: StrsItem[] = [ new StrsItem('') ]; // C7043: The null string is always requierd

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

export interface KMXPlusData {
    sect?: Strs; // sect is ignored in-memory
    bksp?: Bksp;
    elem?: Elem; // elem is ignored in-memory
    finl?: Finl;
    keys?: Keys;
    loca?: Loca;
    meta?: Meta;
    name?: Name;
    ordr?: Ordr;
    strs?: Strs; // strs is ignored in-memory
    tran?: Tran;
    vkey?: Vkey;
};

export default class KMXPlusFile extends KMXFile {

  /* KMXPlus file structures */

  public readonly COMP_PLUS_SECT_ITEM: any;
  public readonly COMP_PLUS_SECT: any;

  // COMP_PLUS_BKSP == COMP_PLUS_TRAN
  public readonly COMP_PLUS_BKSP_ITEM: any;
  public readonly COMP_PLUS_BKSP: any;

  public readonly COMP_PLUS_ELEM_ELEMENT: any;
  public readonly COMP_PLUS_ELEM_STRING: any;
  public readonly COMP_PLUS_ELEM: any;

  // COMP_PLUS_FINL == COMP_PLUS_TRAN
  public readonly COMP_PLUS_FINL_ITEM: any;
  public readonly COMP_PLUS_FINL: any;

  public readonly COMP_PLUS_KEYS_ITEM: any;
  public readonly COMP_PLUS_KEYS: any;

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
      reserved: new r.Reserved(r.uint32le), // padding
      strings: new r.Array(this.COMP_PLUS_ELEM_STRING, 'count')
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
      reserved: new r.Reserved(r.uint32le), // padding
      items: new r.Array(this.COMP_PLUS_KEYS_ITEM, 'count')
    });

    // 'loca'

    this.COMP_PLUS_LOCA_ITEM = r.uint32le; //str

    this.COMP_PLUS_LOCA = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      reserved: new r.Reserved(r.uint32le), // padding
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
      reserved: new r.Reserved(r.uint32le), // padding
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
      reserved: new r.Reserved(r.uint32le), // padding
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
      reserved: new r.Reserved(r.uint32le), // padding
      items: new r.Array(this.COMP_PLUS_STRS_ITEM, 'count')
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
      reserved: new r.Reserved(r.uint32le), // padding
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
      reserved: new r.Reserved(r.uint32le), // padding
      items: new r.Array(this.COMP_PLUS_VKEY_ITEM, 'count')
    });

    // Aliases

    this.COMP_PLUS_BKSP_ITEM = this.COMP_PLUS_TRAN_ITEM;
    this.COMP_PLUS_FINL_ITEM = this.COMP_PLUS_TRAN_ITEM;
    this.COMP_PLUS_BKSP = this.COMP_PLUS_TRAN;
    this.COMP_PLUS_FINL = this.COMP_PLUS_TRAN;
  }
}