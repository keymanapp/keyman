import * as r from 'restructure';
import KMXPlusFile from "../kmx-plus.js";
import { constants } from '@keymanapp/ldml-keyboard-constants';
import { BUILDER_SECTION } from './builder-section.js';
import { BUILDER_SECT, build_sect } from './build-sect.js';
import { BUILDER_KEYS, build_keys } from './build-keys.js';
import { BUILDER_LOCA, build_loca } from './build-loca.js';
import { BUILDER_META, build_meta } from './build-meta.js';
import { BUILDER_NAME, build_name } from './build-name.js';
import { BUILDER_STRS, build_strs } from './build-strs.js';
import { BUILDER_VKEY, build_vkey } from './build-vkey.js';
import { BUILDER_TRAN, build_tran } from './build-tran.js';
import { BUILDER_ELEM, build_elem } from './build-elem.js';
import { BUILDER_ORDR, build_ordr } from './build-ordr.js';

type BUILDER_BKSP = BUILDER_TRAN;
type BUILDER_FINL = BUILDER_TRAN;

export default class KMXPlusBuilder {
  private file: KMXPlusFile;
  //private writeDebug: boolean;

  private sect_sect: BUILDER_SECT;
  private sect_bksp: BUILDER_BKSP;
  private sect_elem: BUILDER_ELEM;
  private sect_finl: BUILDER_FINL;
  private sect_keys: BUILDER_KEYS;
  private sect_loca: BUILDER_LOCA;
  private sect_meta: BUILDER_META;
  private sect_name: BUILDER_NAME;
  private sect_ordr: BUILDER_ORDR;
  private sect_strs: BUILDER_STRS;
  private sect_tran: BUILDER_TRAN;
  private sect_vkey: BUILDER_VKEY;

  constructor(file: KMXPlusFile, _writeDebug: boolean) {
    this.file = file;
    //this.writeDebug = _writeDebug;
  }

  public compile(): Uint8Array {
    const fileSize = this.build();
    let file: Uint8Array = new Uint8Array(fileSize);

    this.emitSection(file, this.file.COMP_PLUS_SECT, this.sect_sect);
    this.emitSection(file, this.file.COMP_PLUS_BKSP, this.sect_bksp);
    this.emitSection(file, this.file.COMP_PLUS_ELEM, this.sect_elem);
    this.emitElements(file);
    this.emitSection(file, this.file.COMP_PLUS_FINL, this.sect_finl);
    this.emitSection(file, this.file.COMP_PLUS_KEYS, this.sect_keys);
    this.emitSection(file, this.file.COMP_PLUS_LOCA, this.sect_loca);
    this.emitSection(file, this.file.COMP_PLUS_META, this.sect_meta);
    this.emitSection(file, this.file.COMP_PLUS_NAME, this.sect_name);
    this.emitSection(file, this.file.COMP_PLUS_ORDR, this.sect_ordr);
    this.emitSection(file, this.file.COMP_PLUS_STRS, this.sect_strs);
    this.emitStrings(file);
    this.emitSection(file, this.file.COMP_PLUS_TRAN, this.sect_tran);
    this.emitSection(file, this.file.COMP_PLUS_VKEY, this.sect_vkey);

    return file;
  }

  private build() {
    // Required sections: sect, strs, loca, meta

    // We must prepare the strs and elem sections early so that other sections can
    // reference them. However, they will be emitted in alpha order.
    this.sect_strs = build_strs(this.file.kmxplus.strs);
    this.sect_elem = build_elem(this.file.kmxplus.elem, this.sect_strs);

    const build_bksp = build_tran;
    const build_finl = build_tran;

    this.sect_bksp = build_bksp(this.file.kmxplus.bksp, this.sect_strs, this.sect_elem);
    this.sect_finl = build_finl(this.file.kmxplus.finl, this.sect_strs, this.sect_elem);
    this.sect_keys = build_keys(this.file.kmxplus, this.sect_strs);
    this.sect_loca = build_loca(this.file.kmxplus, this.sect_strs);
    this.sect_meta = build_meta(this.file.kmxplus, this.sect_strs);
    this.sect_name = build_name(this.file.kmxplus, this.sect_strs);
    this.sect_ordr = build_ordr(this.file.kmxplus, this.sect_strs, this.sect_elem);
    this.sect_tran = build_tran(this.file.kmxplus.tran, this.sect_strs, this.sect_elem);
    this.sect_vkey = build_vkey(this.file.kmxplus);

    // Finalize the sect (index) section

    this.sect_sect = build_sect();
    this.finalize_sect(); // must be done last
    return this.sect_sect.total;
  }

  private finalize_sect() {
    // 'sect' section

    // We always have 'loca', 'meta' and 'strs'
    this.sect_sect.count = 3;

    // Handle optional sections
    // TODO: use a loop...
    if(this.sect_bksp) {
      this.sect_sect.count++;
    }
    if(this.sect_elem) {
      this.sect_sect.count++;
    }
    if(this.sect_finl) {
      this.sect_sect.count++;
    }
    if(this.sect_keys) {
      this.sect_sect.count++;
    }
    if(this.sect_name) {
      this.sect_sect.count++;
    }
    if(this.sect_ordr) {
      this.sect_sect.count++;
    }
    if(this.sect_tran) {
      this.sect_sect.count++;
    }
    if(this.sect_vkey) {
      this.sect_sect.count++;
    }

    this.sect_sect.size = constants.length_sect + constants.length_sect_item * this.sect_sect.count;

    let offset = this.sect_sect.size;
    offset = this.finalize_sect_item(this.sect_bksp, offset);
    offset = this.finalize_sect_item(this.sect_elem, offset);
    offset = this.finalize_sect_item(this.sect_finl, offset);
    offset = this.finalize_sect_item(this.sect_keys, offset);
    offset = this.finalize_sect_item(this.sect_loca, offset);
    offset = this.finalize_sect_item(this.sect_meta, offset);
    offset = this.finalize_sect_item(this.sect_name, offset);
    offset = this.finalize_sect_item(this.sect_ordr, offset);
    offset = this.finalize_sect_item(this.sect_strs, offset);
    offset = this.finalize_sect_item(this.sect_tran, offset);
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

  private emitElements(file: Uint8Array) {
    if(this.sect_elem) {
      for(let str of this.sect_elem.strings) {
        if(str.items.length > 0) {
          let COMP_PLUS_ELEM_ELEMENTS = new r.Array(this.file.COMP_PLUS_ELEM_ELEMENT, str.items.length);
          file.set(COMP_PLUS_ELEM_ELEMENTS.toBuffer(str.items), str.offset + this.sect_elem._offset);
        }
      }
    }
  }

}