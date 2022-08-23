import KMXFile from './kmx';

class MEM_KEYBOARD_HEADER {
  dwIdentifier: number;
  dwFileVersion: number;
  dwCheckSum: number;
  KeyboardID: number;
  IsRegistered: number;
  version: number;

  cxStoreArray: number;
  cxGroupArray: number;

  dpStoreArray: number;
  dpGroupArray: number;

  StartGroup_ANSI: number;
  StartGroup_Unicode: number;

  dwFlags: number;

  dwHotKey: number;

  dpBitmapOffset: number;
  dwBitmapSize: number;
};

class MEM_KEYBOARD_KMXPLUSINFO {
  dpKMXPlus: number;
  dwKMXPlusSize: number;
};

export default class KMXBuilder {
  file: KMXFile;
  constructor(file: KMXFile) {
    this.file = file;
  }

  compiledHeader(): MEM_KEYBOARD_HEADER {
    return {
      dwIdentifier: KMXFile.FILEID_COMPILED,
      dwFileVersion: KMXFile.VERSION_160,
      dwCheckSum: 0,
      KeyboardID: 0,
      IsRegistered: 1,
      version: 0,
      cxStoreArray: this.file.keyboard.stores.length,
      cxGroupArray: this.file.keyboard.groups.length,
      dpStoreArray: 0,
      dpGroupArray: 0,
      StartGroup_ANSI: 0xFFFFFFFF,
      StartGroup_Unicode: 0xFFFFFFFF,
      dwFlags: this.file.keyboard.isKMXPlus ? KMXFile.KF_KMXPLUS : 0,
      dwHotKey: 0,
      dpBitmapOffset: 0,
      dwBitmapSize: 0
    };
  }

  compiledKMXPlusHeader(): MEM_KEYBOARD_KMXPLUSINFO {
    return {
      dpKMXPlus: 0,
      dwKMXPlusSize: 0
    };
  }

  concat(a: Uint8Array, b: Uint8Array): Uint8Array {
    let c = new Uint8Array(a.length + b.length);
    c.set(a);
    c.set(b, a.length);
    return c;
  }

  compile(): Uint8Array {
    let file: Uint8Array = this.file.COMP_KEYBOARD.toBuffer(this.compiledHeader());
    if(this.file.keyboard.isKMXPlus) {
      let kmxplus: Uint8Array = this.file.COMP_KEYBOARD_KMXPLUSINFO.toBuffer(this.compiledKMXPlusHeader());
      file = this.concat(file, kmxplus);
    }
    // TODO once we have the final buffer, we need to do a CRC32 of the entire contents
    // TODO suggest using a single buffer that grows exponentially to save masses of allocs
    // header.set
    return file;
  }
}