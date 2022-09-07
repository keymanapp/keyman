import * as r from 'restructure';

import KMXFile from './kmx';

// Implementation of file structures from /core/src/ldml/C7043_ldml.md
// Writer in kmx-builder.ts
// Reader in kmx-loader.ts

export class Section {
}

export enum KeyboardSettings {
  none = 0,
  fallback = 1<<0,
  transformFailure = 1<<1,
  transformPartial = 1<<2,
};

export enum Meta_NormalizationForm { NFC='NFC', NFD='NFD', other='other' };

export class Meta extends Section {
  name: string;
  author: string;
  conform: string;
  layout: string;
  normalization: Meta_NormalizationForm;
  indicator: string;
  version: string; // semver version string, defaults to "0"
  settings: KeyboardSettings;
};

export class Loca extends Section {
  locales: string[] = [];
};

export enum KeyFlags {
  none = 0,
  extend = 1<<0,  // note, this should be used only when streaming, ignored in-memory
  // additional flags reserved for future use
};

export class KeysItem {
  vkey: number;
  mod: number;
  to: string;
  flags: KeyFlags;
};

export class Keys extends Section {
  keys: KeysItem[] = [];
};

export class VkeyItem {
  vkey: number;
  target: number;
}

export class Vkey extends Section {
  vkeys: VkeyItem[] = [];
};

export default class KMXPlusFile extends KMXFile {

  /* KMXPlus file structures */

  public readonly COMP_PLUS_SECT_ITEM: any;
  public readonly COMP_PLUS_SECT: any;

  public readonly COMP_PLUS_STRS_ITEM: any;
  public readonly COMP_PLUS_STRS: any;

  public readonly COMP_PLUS_META: any;

  public readonly COMP_PLUS_LOCA_ITEM: any;
  public readonly COMP_PLUS_LOCA: any;

  public readonly COMP_PLUS_KEYS_ITEM: any;
  public readonly COMP_PLUS_KEYS: any;

  public readonly COMP_PLUS_VKEY_ITEM: any;
  public readonly COMP_PLUS_VKEY: any;

  /* File in-memory data */

  public kmxplus: {
    sect?: Section; // sect is ignored here for writing
    strs?: Section; // strs is ignored here for writing
    loca?: Loca;
    meta?: Meta;
    keys?: Keys;
    vkey?: Vkey;
  } = { };

  constructor() {
    super();


    // Binary-correct structures matching kmx_plus.h

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

    this.COMP_PLUS_META = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      name: r.uint32le, //str
      author: r.uint32le, //str
      conform: r.uint32le, //str
      layout: r.uint32le, //str
      normalization: r.uint32le, //str
      indicator: r.uint32le, //str
      version: r.uint32le, //str
      settings: r.uint32le, //new r.Bitfield(r.uint32le, ['fallback', 'transformFailure', 'transformPartial'])
    });

    this.COMP_PLUS_LOCA_ITEM = r.uint32le; //str

    this.COMP_PLUS_LOCA = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      reserved: new r.Reserved(r.uint32le), // padding
      items: new r.Array(this.COMP_PLUS_LOCA_ITEM, 'count')
    });

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
  }
}