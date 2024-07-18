import * as r from 'restructure';
import { KMXPlusFile } from "../kmx-plus.js";
import { constants, SectionIdent } from '@keymanapp/ldml-keyboard-constants';
import { BUILDER_SECTION } from './builder-section.js';
import { BUILDER_SECT, build_sect } from './build-sect.js';
import { BUILDER_DISP, build_disp } from './build-disp.js';
import { BUILDER_ELEM, build_elem } from './build-elem.js';
import { BUILDER_KEYS, build_keys } from './build-keys.js';
import { BUILDER_LAYR, build_layr } from './build-layr.js';
import { BUILDER_LIST, build_list } from './build-list.js';
import { BUILDER_LOCA, build_loca } from './build-loca.js';
import { BUILDER_META, build_meta } from './build-meta.js';
import { BUILDER_STRS, build_strs } from './build-strs.js';
import { BUILDER_TRAN, build_tran } from './build-tran.js';
import { BUILDER_USET, build_uset } from './build-uset.js';
import { BUILDER_VARS, build_vars } from './build-vars.js';

type BUILDER_BKSP = BUILDER_TRAN;
// type BUILDER_FINL = BUILDER_TRAN;

type SectionBuilders = {
  // [id in SectionIdent]: BUILDER_SECTION;
  sect?: BUILDER_SECT;
  bksp?: BUILDER_BKSP;
  disp?: BUILDER_DISP;
  elem?: BUILDER_ELEM;
  keys?: BUILDER_KEYS;
  layr?: BUILDER_LAYR;
  list?: BUILDER_LIST;
  loca?: BUILDER_LOCA;
  meta?: BUILDER_META;
  strs?: BUILDER_STRS;
  tran?: BUILDER_TRAN;
  uset?: BUILDER_USET;
  vars?: BUILDER_VARS;
};

export default class KMXPlusBuilder {
  private file: KMXPlusFile;
  //private writeDebug: boolean;

  sect : SectionBuilders = {

  };

  constructor(file: KMXPlusFile, _writeDebug: boolean) {
    this.file = file;
    //this.writeDebug = _writeDebug;
  }

  public compile(): Uint8Array {
    const fileSize = this.build();
    let file: Uint8Array = new Uint8Array(fileSize);

    this.emitSection(file, this.file.COMP_PLUS_SECT, this.sect.sect);
    // Keep the rest of these in order.
    this.emitSection(file, this.file.COMP_PLUS_BKSP, this.sect.bksp);
    this.emitSection(file, this.file.COMP_PLUS_DISP, this.sect.disp);
    this.emitSection(file, this.file.COMP_PLUS_ELEM, this.sect.elem);
    this.emitElements(file);
    this.emitSection(file, this.file.COMP_PLUS_KEYS, this.sect.keys);
    this.emitSection(file, this.file.COMP_PLUS_LAYR, this.sect.layr);
    this.emitSection(file, this.file.COMP_PLUS_LIST, this.sect.list);
    this.emitSection(file, this.file.COMP_PLUS_LOCA, this.sect.loca);
    this.emitSection(file, this.file.COMP_PLUS_META, this.sect.meta);
    this.emitSection(file, this.file.COMP_PLUS_STRS, this.sect.strs);
    this.emitStrings(file);
    this.emitSection(file, this.file.COMP_PLUS_TRAN, this.sect.tran);
    this.emitSection(file, this.file.COMP_PLUS_USET, this.sect.uset);
    this.emitSection(file, this.file.COMP_PLUS_VARS, this.sect.vars);

    return file;
  }

  private build() {
    // Required sections: sect, strs, loca, meta

    // We must prepare the strs, list, and elem sections early so that other sections can
    // reference them. However, they will be emitted in alpha order.
    this.sect.strs = build_strs(this.file.kmxplus.strs);
    this.sect.list = build_list(this.file.kmxplus.list, this.sect.strs);
    this.sect.uset = build_uset(this.file.kmxplus, this.sect.strs);
    this.sect.elem = build_elem(this.file.kmxplus.elem, this.sect.strs, this.sect.uset);

    const build_bksp = build_tran;

    this.sect.bksp = build_bksp(this.file.kmxplus.bksp, this.sect.strs, this.sect.elem);
    this.sect.disp = build_disp(this.file.kmxplus, this.sect.strs);
    this.sect.keys = build_keys(this.file.kmxplus, this.sect.strs, this.sect.list);
    this.sect.layr = build_layr(this.file.kmxplus, this.sect.strs, this.sect.list);
    this.sect.loca = build_loca(this.file.kmxplus, this.sect.strs);
    this.sect.meta = build_meta(this.file.kmxplus, this.sect.strs);
    this.sect.tran = build_tran(this.file.kmxplus.tran, this.sect.strs, this.sect.elem);
    this.sect.uset = build_uset(this.file.kmxplus, this.sect.strs);
    this.sect.vars = build_vars(this.file.kmxplus, this.sect.strs, this.sect.elem, this.sect.list);

    // Finalize the sect (index) section

    this.sect.sect = build_sect();
    this.finalize_sect(); // must be done last
    return this.sect.sect.total;
  }

  private finalize_sect() {
    // 'sect' section

    this.sect.sect.count = 0;

    Object.keys(constants.section).forEach((sectstr : string) => {
      const sect : SectionIdent = constants.section[<SectionIdent>sectstr];
      if(this.sect[sect] && sect !== 'sect') {
        this.sect.sect.count++;
      }
    });

    this.sect.sect.size = constants.length_sect + constants.length_sect_item * this.sect.sect.count;

    let offset = this.sect.sect.size;
    // Note: in order! Everyone's here except 'sect' which is at offset 0
    offset = this.finalize_sect_item(this.sect.bksp, offset);
    offset = this.finalize_sect_item(this.sect.disp, offset);
    offset = this.finalize_sect_item(this.sect.elem, offset);
    offset = this.finalize_sect_item(this.sect.keys, offset);
    offset = this.finalize_sect_item(this.sect.layr, offset);
    offset = this.finalize_sect_item(this.sect.list, offset);
    offset = this.finalize_sect_item(this.sect.loca, offset);
    offset = this.finalize_sect_item(this.sect.meta, offset);
    offset = this.finalize_sect_item(this.sect.strs, offset);
    offset = this.finalize_sect_item(this.sect.tran, offset);
    offset = this.finalize_sect_item(this.sect.uset, offset);
    offset = this.finalize_sect_item(this.sect.vars, offset);

    this.sect.sect.total = offset;
  }

  private finalize_sect_item(sect: BUILDER_SECTION, offset: number): number {
    if(!sect) {
      // Don't include null sections
      return offset;
    }
    sect._offset = offset;
    this.sect.sect.items.push({sect: sect.ident, offset: offset});
    return offset + sect.size;
  }

  private emitSection(file: Uint8Array, comp: any, sect: BUILDER_SECTION) {
    if(sect) {
      const buf = comp.toBuffer(sect);
      if (buf.length > sect.size) {
        // buf.length may be < sect.size if there is a variable part (i.e. elem)
        throw new RangeError(`Internal Error: Section ${constants.str_section_id(sect.ident)} claimed size ${sect.size} but produced buffer of size ${buf.length}.`);
      }
      file.set(buf, sect._offset);
    }
  }

  private emitStrings(file: Uint8Array) {
    for(let item of this.sect.strs.items) {
      if(item._value === '') {
        // We have a special case for the zero-length string
        let sbuf = r.uint16le;
        file.set(sbuf.toBuffer(0), item.offset + this.sect.strs._offset);
      } else {
        let sbuf = new r.String(null, 'utf16le');
        file.set(sbuf.toBuffer(item._value), item.offset + this.sect.strs._offset);
      }
    }
  }

  private emitElements(file: Uint8Array) {
    if(this.sect.elem) {
      for(let str of this.sect.elem.strings) {
        if(str.items.length > 0) {
          let COMP_PLUS_ELEM_ELEMENTS = new r.Array(this.file.COMP_PLUS_ELEM_ELEMENT, str.items.length);
          file.set(COMP_PLUS_ELEM_ELEMENTS.toBuffer(str.items), str.offset + this.sect.elem._offset);
        }
      }
    }
  }
}
