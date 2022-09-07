import * as r from 'restructure';
import KMXPlusFile, { KeyFlags } from "./kmx-plus";
import { constants } from '@keymanapp/ldml-keyboard-constants';

interface BUILDER_SECTION {
  ident: number;
  size: number;
  _offset: number; // used only for building the output
};

/* ------------------------------------------------------------------
 * sect section
   ------------------------------------------------------------------ */

interface BUILDER_SECT_ITEM {
  sect: number;
  offset: number; //? new r.VoidPointer(r.uint32le, {type: 'global'})
};

/**
 * Builder for the 'sect' (Section table of contents) section
 */
interface BUILDER_SECT extends BUILDER_SECTION {
  total: number;
  count: number;
  items: BUILDER_SECT_ITEM[];
};

/* ------------------------------------------------------------------
 * keys section
   ------------------------------------------------------------------ */

interface BUILDER_KEYS_ITEM {
  vkey: number;
  mod: number;
  to: number; //str or UTF-32 char depending on value of 'extend'
  flags: number; //bitfield
};

/**
 * Builder for the 'keys' section
 */
interface BUILDER_KEYS extends BUILDER_SECTION {
  count: number;
  reserved: number;
  items: BUILDER_KEYS_ITEM[];
};

/* ------------------------------------------------------------------
 * loca section
   ------------------------------------------------------------------ */

/**
 * Builder for the 'loca' section
 */
interface BUILDER_LOCA extends BUILDER_SECTION {
  count: number;
  reserved: number;
  items: number[]; //str[]
};

/* ------------------------------------------------------------------
 * meta section
   ------------------------------------------------------------------ */

/**
 * Builder for the 'meta' section
 */
interface BUILDER_META extends BUILDER_SECTION {
  name: number; //str
  author: number; //str
  conform: number; //str
  layout: number; //str
  normalization: number; //str
  indicator: number; //str
  version: number; //str
  settings: number; //bitfield
};

/* ------------------------------------------------------------------
 * name section
   ------------------------------------------------------------------ */

/**
 * Builder for the 'name' section
 */
interface BUILDER_NAME extends BUILDER_SECTION {
  count: number;
  reserved: number;
  items: number[]; //str[]
};

/* ------------------------------------------------------------------
 * strs section
   ------------------------------------------------------------------ */

interface BUILDER_STRS_ITEM {
  // While we use length which is number of utf-16 code units excluding null terminator,
  // we always write a null terminator, so we can get restructure to do that for us here
  offset: number; //? new r.Pointer(r.uint32le, new r.String(null, 'utf16le')),
  length: number; // in UTF-16 code units
  _value: string
};

/**
 * Builder for the 'strs' section
 */
interface BUILDER_STRS extends BUILDER_SECTION {
  count: number;
  reserved: number;
  items: BUILDER_STRS_ITEM[];
};

/* ------------------------------------------------------------------
 * vkey section
   ------------------------------------------------------------------ */

/**
 * Builder for the 'vkey' section
 */
interface BUILDER_VKEY_ITEM {
  vkey: number;
  target: number;
};

interface BUILDER_VKEY extends BUILDER_SECTION {
  count: number;
  reserved: number;
  items: BUILDER_VKEY_ITEM[];
};


export default class KMXPlusBuilder {
  private file: KMXPlusFile;
  //private writeDebug: boolean;

  private sect_sect: BUILDER_SECT;
  private sect_keys: BUILDER_KEYS;
  private sect_loca: BUILDER_LOCA;
  private sect_meta: BUILDER_META;
  private sect_name: BUILDER_NAME;
  private sect_strs: BUILDER_STRS;
  private sect_vkey: BUILDER_VKEY;

  constructor(file: KMXPlusFile, _writeDebug: boolean) {
    this.file = file;
    //this.writeDebug = _writeDebug;
  }

  public compile(): Uint8Array {
    const fileSize = this.build();
    let file: Uint8Array = new Uint8Array(fileSize);

    this.emitSection(file, this.file.COMP_PLUS_SECT, this.sect_sect);
    this.emitSection(file, this.file.COMP_PLUS_KEYS, this.sect_keys);
    this.emitSection(file, this.file.COMP_PLUS_LOCA, this.sect_loca);
    this.emitSection(file, this.file.COMP_PLUS_META, this.sect_meta);
    this.emitSection(file, this.file.COMP_PLUS_NAME, this.sect_name);
    this.emitSection(file, this.file.COMP_PLUS_STRS, this.sect_strs);
    this.emitStrings(file);
    this.emitSection(file, this.file.COMP_PLUS_VKEY, this.sect_vkey);

    return file;
  }

  private build() {
    // Required sections: sect, strs, loca, meta

    this.sect_sect = this.build_sect();

    // We must prepare the strs section early so that other sections can
    // reference it. However, it will be emitted in alpha order.
    this.sect_strs = this.build_strs();
    // per C7043, the first string in sect_strs MUST be the zero-length string.
    this.alloc_string('');

    this.sect_meta = this.build_meta();
    this.sect_name = this.build_name();
    this.sect_loca = this.build_loca();
    this.sect_keys = this.build_keys();
    this.sect_vkey = this.build_vkey();

    // Finalize all sections

    this.finalize_strs(); // must be done after all strings allocated
    this.finalize_sect(); // must be done last

    return this.sect_sect.total;
  }

  private alloc_string(value: string) {
    if(typeof value != 'string') {
      if(value === undefined || value === null) {
        value = '';
      } else {
        throw 'unexpected value '+value;
      }
    }

    let idx = this.sect_strs.items.findIndex(v => v._value === value);
    if(idx >= 0) {
      return idx;
    }

    let item: BUILDER_STRS_ITEM = {
      _value: value,
      length: value.length,
      offset: 0 // will be filled in later
    };

    return this.sect_strs.items.push(item) - 1;
  }

  private build_sect(): BUILDER_SECT {
    return {
      ident: constants.hex_section_id(constants.section.sect),
      size: 0, // finalized later
      _offset: 0,
      total: 0, // finalized later
      count: 0, // finalized later
      items: [], // finalized later
    };
  }

  private build_keys(): BUILDER_KEYS {
    if(!this.file.kmxplus.keys.keys.length) {
      return null;
    }

    let keys: BUILDER_KEYS = {
      ident: constants.hex_section_id(constants.section.keys),
      size: constants.length_keys + constants.length_keys_item * this.file.kmxplus.keys.keys.length,
      _offset: 0,
      count: this.file.kmxplus.keys.keys.length,
      reserved: 0,
      items: []
    };

    for(let item of this.file.kmxplus.keys.keys) {
      keys.items.push({
        vkey: item.vkey,
        mod: item.mod,
        // todo: support 'extend'
        to: this.alloc_string(item.to),
        flags: KeyFlags.extend // todo: support non-extended
      });
    }

    return keys;
  }

  private build_loca(): BUILDER_LOCA {
    let loca: BUILDER_LOCA = {
      ident: constants.hex_section_id(constants.section.loca),
      size: constants.length_loca + constants.length_loca_item * this.file.kmxplus.loca.locales.length,
      _offset: 0,
      count: this.file.kmxplus.loca.locales.length,
      reserved: 0,
      items: []
    };

    for(let item of this.file.kmxplus.loca.locales) {
      loca.items.push(this.alloc_string(item));
    }

    return loca;
  }

  private build_meta(): BUILDER_META {
    return {
      ident: constants.hex_section_id(constants.section.meta),
      size: constants.length_meta,
      _offset: 0,
      name: this.alloc_string(this.file.kmxplus.meta.name),
      author: this.alloc_string(this.file.kmxplus.meta.author),
      conform: this.alloc_string(this.file.kmxplus.meta.conform),
      layout: this.alloc_string(this.file.kmxplus.meta.layout),
      normalization: this.alloc_string(this.file.kmxplus.meta.normalization),
      indicator: this.alloc_string(this.file.kmxplus.meta.indicator),
      version: this.alloc_string(this.file.kmxplus.meta.version),
      settings: this.file.kmxplus.meta.settings ?? 0,
    };
  }

  private build_name(): BUILDER_NAME {
    if(!this.file.kmxplus.name.names.length) {
      return null;
    }

    let name: BUILDER_NAME = {
      ident: constants.hex_section_id(constants.section.name),
      size: constants.length_name + constants.length_name_item * this.file.kmxplus.name.names.length,
      _offset: 0,
      count: this.file.kmxplus.name.names.length,
      reserved: 0,
      items: []
    };

    for(let item of this.file.kmxplus.name.names) {
      name.items.push(this.alloc_string(item));
    }

    return name;
  }

  private build_strs(): BUILDER_STRS {
    return {
      ident: constants.hex_section_id(constants.section.strs),
      size: 0,  // finalized later
      _offset: 0,
      count: 0,  // finalized later
      reserved: 0,
      items: [], // finalized later
    };
  }

  private build_vkey(): BUILDER_VKEY {
    if(!this.file.kmxplus.vkey.vkeys.length) {
      return null;
    }

    let vkey: BUILDER_VKEY = {
      ident: constants.hex_section_id(constants.section.vkey),
      size: constants.length_vkey + constants.length_vkey_item * this.file.kmxplus.vkey.vkeys.length,
      _offset: 0,
      reserved: 0,
      count: this.file.kmxplus.vkey.vkeys.length,
      items: []
    };

    for(let item of this.file.kmxplus.vkey.vkeys) {
      vkey.items.push({
        vkey: item.vkey,
        target: item.target
      });
    }

    return vkey;
  }

  private finalize_strs() {
    this.sect_strs.count = this.sect_strs.items.length;
    let offset = constants.length_strs + constants.length_strs_item * this.sect_strs.count;
    // TODO: consider padding
    for(let item of this.sect_strs.items) {
      item.offset = offset;
      offset += item.length * 2 + 2; /* UTF-16 code units + sizeof null terminator */
    }
    this.sect_strs.size = offset;
  }

  private finalize_sect() {
    // 'sect' section

    // We always have 'loca', 'meta' and 'strs'
    this.sect_sect.count = 3;

    // Handle optional sections
    if(this.sect_keys) {
      this.sect_sect.count++;
    }
    if(this.sect_name) {
      this.sect_sect.count++;
    }
    if(this.sect_vkey) {
      this.sect_sect.count++;
    }

    this.sect_sect.size = constants.length_sect + constants.length_sect_item * this.sect_sect.count;

    let offset = this.sect_sect.size;
    offset = this.finalize_sect_item(this.sect_keys, offset);
    offset = this.finalize_sect_item(this.sect_loca, offset);
    offset = this.finalize_sect_item(this.sect_meta, offset);
    offset = this.finalize_sect_item(this.sect_name, offset);
    offset = this.finalize_sect_item(this.sect_strs, offset);
    offset = this.finalize_sect_item(this.sect_vkey, offset);

    this.sect_sect.total = offset;
  }

  private finalize_sect_item(sect: BUILDER_SECTION, offset: number): number {
    if(!sect) {
      // Don't include null sections
      return offset;
    }
    sect._offset = offset;
    this.sect_sect.items.push({sect: sect.ident, offset: offset});
    // TODO: padding
    return offset + sect.size;
  }

  private emitSection(file: Uint8Array, comp: any, sect: BUILDER_SECTION) {
    if(sect) {
      file.set(comp.toBuffer(sect), sect._offset);
    }
  }

  private emitStrings(file: Uint8Array) {
    for(let item of this.sect_strs.items) {
      if(item._value === '') {
        // We have a special case for the zero-length string
        let sbuf = r.uint16le;
        file.set(sbuf.toBuffer(0), item.offset + this.sect_strs._offset);
      } else {
        let sbuf = new r.String(null, 'utf16le');
        file.set(sbuf.toBuffer(item._value), item.offset + this.sect_strs._offset);
      }
    }
  }
}