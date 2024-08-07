import { KMXPlus, KMX } from '@keymanapp/common-types';
import * as r from 'restructure';
import KMXPlusBuilder from './kmx-plus-builder/kmx-plus-builder.js';

import KMXPlusFile = KMXPlus.KMXPlusFile;
import KMXFile = KMX.KMXFile;
import GROUP = KMX.GROUP;
import KEY = KMX.KEY;
import STORE = KMX.STORE;
import BUILDER_COMP_KEYBOARD = KMX.BUILDER_COMP_KEYBOARD;
import BUILDER_COMP_KEYBOARD_KMXPLUSINFO = KMX.BUILDER_COMP_KEYBOARD_KMXPLUSINFO;
import BUILDER_COMP_STORE = KMX.BUILDER_COMP_STORE;
import BUILDER_COMP_GROUP = KMX.BUILDER_COMP_GROUP;
import BUILDER_COMP_KEY = KMX.BUILDER_COMP_KEY;

export default class KMXBuilder {
  file: KMXFile;
  base_keyboard: number = 0;
  base_kmxplus: number = 0;
  comp_header: BUILDER_COMP_KEYBOARD;
  comp_kmxplus: BUILDER_COMP_KEYBOARD_KMXPLUSINFO;
  comp_stores: {base: number, store: STORE, obj: BUILDER_COMP_STORE}[] = [];
  comp_groups: {base: number, group: GROUP, obj: BUILDER_COMP_GROUP, keys: {base: number, key: KEY, obj: BUILDER_COMP_KEY}[]}[] = [];
  comp_kmxplus_data: Uint8Array;
  writeDebug: boolean = false;

  constructor(file: KMXFile, writeDebug: boolean) {
    this.file = file;
    this.writeDebug = writeDebug;
  }

  calculateStringOffsetAndSize(string: string, base: number, requireString: boolean = false) {
    if(string.length == 0 && !requireString) {
      // Zero length strings take up no space in the file, and
      // are treated as a 'null string'
      return [0, base];
    }
    return [base, base + string.length * 2 + 2]; // include trailing zero
  }

  private build() {
    this.base_keyboard = 0;
    this.base_kmxplus = 0;

    //  Header

    this.comp_header = {
      dwIdentifier: KMXFile.FILEID_COMPILED,
      dwFileVersion: KMXFile.VERSION_170,
      dwCheckSum: 0,  // Deprecated in Keyman 16.0
      KeyboardID: 0,
      IsRegistered: 1,
      version: 0,
      cxStoreArray: 0,
      cxGroupArray: 0,
      dpStoreArray: 0,
      dpGroupArray: 0,
      StartGroup_ANSI: 0xFFFFFFFF,
      StartGroup_Unicode: 0xFFFFFFFF,
      dwFlags: 0,
      dwHotKey: 0,
      dpBitmapOffset: 0,
      dwBitmapSize: 0
    };

    let size = KMXFile.COMP_KEYBOARD_SIZE;

    if(this.file instanceof KMXPlusFile) {
      // Reserve space for KMXPlus header; we'll come back and fill in details
      // once we know base kmx file size.
      this.comp_header.dwFlags |= KMXFile.KF_KMXPLUS;
      this.base_kmxplus = size;
      size += KMXFile.COMP_KEYBOARD_KMXPLUSINFO_SIZE;
    }

    // Stores

    this.comp_header.cxStoreArray = this.file.keyboard.stores.length;
    this.comp_header.dpStoreArray = this.comp_header.cxStoreArray ? size : 0;
    let storeBase = size;
    size += this.file.keyboard.stores.length * KMXFile.COMP_STORE_SIZE;
    for(const store of this.file.keyboard.stores) {
      const comp_store: BUILDER_COMP_STORE = {
        dwSystemID: store.dwSystemID,
        dpName: 0,
        dpString: 0
      };
      this.comp_stores.push({base: storeBase, store: store, obj: comp_store});
      if(this.writeDebug /*TODO: || store.isOption*/) {
        [comp_store.dpName, size] = this.calculateStringOffsetAndSize(store.dpName, size);
      }

      [comp_store.dpString, size] = this.calculateStringOffsetAndSize(store.dpString, size);
      storeBase += KMXFile.COMP_STORE_SIZE;
    }

    // Groups

    this.comp_header.cxGroupArray = this.file.keyboard.groups.length;
    this.comp_header.dpGroupArray = this.comp_header.cxGroupArray ? size : 0;
    let groupBase = size;
    size += this.file.keyboard.groups.length * KMXFile.COMP_GROUP_SIZE;
    for(const group of this.file.keyboard.groups) {
      const comp_group: BUILDER_COMP_GROUP = {
        dpName: 0,
        dpKeyArray: 0,
        dpMatch: 0,
        dpNoMatch: 0,
        cxKeyArray: group.keys.length,
        fUsingKeys: group.fUsingKeys ? 1 : 0
      };

      const comp_keys: {base: number, key: KEY, obj: BUILDER_COMP_KEY}[] = [];

      this.comp_groups.push({base: groupBase, group: group, obj: comp_group, keys: comp_keys});

      if(this.writeDebug) {
        [comp_group.dpName, size] = this.calculateStringOffsetAndSize(group.dpName, size);
      }
      [comp_group.dpMatch, size] = this.calculateStringOffsetAndSize(group.dpMatch, size);
      [comp_group.dpNoMatch, size] = this.calculateStringOffsetAndSize(group.dpNoMatch, size);

      // Keys within a group

      comp_group.dpKeyArray = group.keys.length ? size : 0;

      let keyBase = size;
      size += group.keys.length * KMXFile.COMP_KEY_SIZE;
      for(const key of group.keys) {
        const comp_key: BUILDER_COMP_KEY = {
          Key: key.Key,
          _padding: 0,
          Line: key.Line,
          ShiftFlags: key.ShiftFlags,
          dpOutput: 0,
          dpContext: 0
        };
        comp_keys.push({base: keyBase, key: key, obj: comp_key});
        [comp_key.dpOutput, size] = this.calculateStringOffsetAndSize(key.dpOutput, size, true);
        [comp_key.dpContext, size] = this.calculateStringOffsetAndSize(key.dpContext, size, true);
        keyBase += KMXFile.COMP_KEY_SIZE;
      }

      groupBase += KMXFile.COMP_GROUP_SIZE;
    }

    size += this.buildBitmap();
    size += this.buildKMXPlus(size);

    return size;
  }

  buildBitmap() {
    // TODO
    return 0;
  }

  buildKMXPlus(base: number) {
    if(!(this.file instanceof KMXPlusFile)) {
      return 0;
    }

    const plusbuilder: KMXPlusBuilder = new KMXPlusBuilder(this.file, this.writeDebug);
    this.comp_kmxplus_data = plusbuilder.compile();
    this.comp_kmxplus = {
      dpKMXPlus: base,
      dwKMXPlusSize: this.comp_kmxplus_data.length
    };

    return this.comp_kmxplus.dwKMXPlusSize;
  }

  setString(file: Uint8Array, pos: number, str: string, requireString: boolean = false): void {
    if(requireString && !str.length) {
      // Just write zero terminator, as r.String for a zero-length string
      // seems to fail.
      const sbuf = r.uint16le;
      file.set(sbuf.toBuffer(0), pos);
    }
    else if(pos && str.length) {
      const sbuf = new r.String(null, 'utf16le'); // null-terminated string
      file.set(sbuf.toBuffer(str), pos);
    }
  }

  compile(): Uint8Array {
    const fileSize = this.build();

    const file: Uint8Array = new Uint8Array(fileSize);

    // Write headers

    const header = this.file.COMP_KEYBOARD.toBuffer(this.comp_header);
    file.set(header, this.base_keyboard);

    if(this.file instanceof KMXPlusFile) {
      const kmxplus: Uint8Array = this.file.COMP_KEYBOARD_KMXPLUSINFO.toBuffer(this.comp_kmxplus);
      file.set(kmxplus, this.base_kmxplus);
    }

    // Write store array and data

    for(const store of this.comp_stores) {
      file.set(this.file.COMP_STORE.toBuffer(store.obj), store.base);
      if(this.writeDebug) {
        this.setString(file, store.obj.dpName, store.store.dpName);
      }
      this.setString(file, store.obj.dpString, store.store.dpString);
    }

    // Write group array and data

    for(const group of this.comp_groups) {
      file.set(this.file.COMP_GROUP.toBuffer(group.obj), group.base);
      if(this.writeDebug) {
        this.setString(file, group.obj.dpName, group.group.dpName);
      }
      this.setString(file, group.obj.dpMatch, group.group.dpMatch);
      this.setString(file, group.obj.dpNoMatch, group.group.dpNoMatch);

      for(const key of group.keys) {
        file.set(this.file.COMP_KEY.toBuffer(key.obj), key.base);
        // for back-compat reasons, these are never NULL strings
        this.setString(file, key.obj.dpContext, key.key.dpContext, true);
        this.setString(file, key.obj.dpOutput, key.key.dpOutput, true);
      }
    }

    // Write KMXPlus data stream

    if(this.file instanceof KMXPlusFile) {
      file.set(this.comp_kmxplus_data, this.comp_kmxplus.dpKMXPlus);
    }

    return file;
  }
}
