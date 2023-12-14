import * as r from 'restructure';
//
// Binary backing structures for .kvk format
// matching VisualKeyboardSaverBinary.pas
//

export const BUILDER_KVK_HEADER_IDENTIFIER = 0x464B564B; // 'KVKF', little-endian
export const KVK_HEADER_IDENTIFIER_BYTES = [0x4B, 0x56, 0x4B, 0x46];
export const BUILDER_KVK_HEADER_VERSION    = 0x0600;     // Keyman 6.0

export interface BUILDER_KVK_STRING {
  len: number;
  str: string;
};

export interface BUILDER_KVK_FONT {
  name: BUILDER_KVK_STRING;       // 0000, variable length
  size: number;                   // 4 bytes
  color: number;                  // 4 bytes
};

export const BUILDER_KVK_FONT_Size = 8; // size of fixed elements of BUILDER_KVK_FONT

export const enum BUILDER_KVK_KEY_FLAGS {
  kvkkBitmap = 0x01,
  kvkkUnicode = 0x02
};

export const enum BUILDER_KVK_SHIFT_STATE {
  KVKS_NORMAL =  0,
  KVKS_SHIFT  =  1,
  KVKS_CTRL   =  2,
  KVKS_ALT    =  4,
  KVKS_LCTRL  =  8,
  KVKS_RCTRL  = 16,
  KVKS_LALT   = 32,
  KVKS_RALT   = 64
};

export interface BUILDER_KVK_KEY {
  flags: BUILDER_KVK_KEY_FLAGS;   // 0000, 1 byte
  shift: number;                  // 0001, 2 bytes
  vkey: number;                   // 0003, 2 bytes
  text: BUILDER_KVK_STRING;       // 0005, variable length
  bitmapSize: number;             // 4 bytes
  bitmapData: number[];
};

export const BUILDER_KVK_KEY_Size = 9; // size of fixed elements of BUILDER_KVK_KEY

export const enum BUILDER_KVK_HEADER_FLAGS {
  kvkhNone = 0x00, // no flags
  kvkh102 = 0x01,
  kvkhDisplayUnderlying = 0x02,
  kvkhUseUnderlying = 0x04,
  kvkhAltGr = 0x08
};

export interface BUILDER_KVK_HEADER {
  identifier: number;             // 0000, 4 bytes
  version: number;                // 0004, 4 bytes
  flags: BUILDER_KVK_HEADER_FLAGS;            // 0008, 1 byte
  associatedKeyboard: BUILDER_KVK_STRING;     // 0009, variable length
  ansiFont: BUILDER_KVK_FONT;     // variable length
  unicodeFont: BUILDER_KVK_FONT;  // variable length
};

export const BUILDER_KVK_HEADER_Size = 9 + BUILDER_KVK_FONT_Size + BUILDER_KVK_FONT_Size; // size of fixed elements of BUILDER_KVK_HEADER

export interface BUILDER_KVK_FILE {
  header: BUILDER_KVK_HEADER;     // variable length
  keyCount: number;               // 4 bytes
  keys: BUILDER_KVK_KEY[];        // variable length
};

export default class KVKFile {
  public KVK_HEADER: any;
  public KVK_KEY: any;
  public KVK_KEYS: any;
  public KVK_FONT: any;
  public KVK_FILE: any;
  public KVK_STRING: any;

  constructor() {
    //
    // Binary restructure definitions matching VisualKeyboardSaverBinary.pas
    // TODO: move binaries to separate kvk-file.ts
    //

    this.KVK_STRING = new r.Struct({
      len: r.int16le,
      str: new r.String(null, 'utf16le')
    });

    this.KVK_FONT = new r.Struct({
      name: this.KVK_STRING,
      size: r.int32le,
      color: r.uint32le
    });

    this.KVK_KEY = new r.Struct({
      flags: r.uint8,
      shift: r.uint16le,
      vkey: r.uint16le,
      text: this.KVK_STRING,
      bitmapSize: r.uint32le,
      bitmapData: new r.Array(r.uint8, 'bitmapSize')
    });

    this.KVK_HEADER = new r.Struct({
      identifier: r.uint32le,   // KVKF
      version: r.uint32le,      // 0x0600
      flags: r.uint8,
      associatedKeyboard: this.KVK_STRING,
      ansiFont: this.KVK_FONT,
      unicodeFont: this.KVK_FONT
    });

    this.KVK_FILE = new r.Struct({
      header: this.KVK_HEADER,
      keyCount: r.uint32le,
      keys: new r.Array(this.KVK_KEY, 'keyCount')
    });

  }
};
