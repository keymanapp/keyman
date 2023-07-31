import { KMXFile, BUILDER_COMP_KEYBOARD, KEYBOARD, STORE } from "./kmx.js";
import * as r from 'restructure';

export class KmxFileReader {
  private readonly rString = new r.String(null, 'utf16le');

  private readString(source: Uint8Array, offset: number): string {
    if(offset == 0) {
      // the file has a null string here, which is not the same as an empty
      // string ('')
      return null;
    }
    return this.rString.fromBuffer(source.slice(offset));
  }

  public read(source: Uint8Array): KEYBOARD {
    let binaryKeyboard: BUILDER_COMP_KEYBOARD;
    let kmx = new KMXFile();
    binaryKeyboard = kmx.COMP_KEYBOARD.fromBuffer(source);
    if(binaryKeyboard.dwIdentifier != KMXFile.FILEID_COMPILED) {
      return null;
    }

    // TODO: all header fields

    let result = new KEYBOARD();
    let offset = binaryKeyboard.dpStoreArray;
    for(let i = 0; i < binaryKeyboard.cxStoreArray; i++) {
      let binaryStore = kmx.COMP_STORE.fromBuffer(source.slice(offset));
      let store = new STORE();
      store.dwSystemID = binaryStore.dwSystemID;
      store.dpName = this.readString(source, binaryStore.dpName);
      store.dpString = this.readString(source, binaryStore.dpString);
      result.stores.push(store);
      offset += KMXFile.COMP_STORE_SIZE;
    }

    // TODO: groups

    // TODO: KMXPlusFile

    return result;
  }
};