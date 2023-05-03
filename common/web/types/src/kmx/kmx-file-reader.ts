import { KMXFile, BUILDER_COMP_KEYBOARD, KEYBOARD, STORE, GROUP, KEY } from "./kmx.js";
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

    let result = new KEYBOARD();
    result.fileVersion = binaryKeyboard.dwFileVersion;
    result.flags = binaryKeyboard.dwFlags;
    result.hotkey = binaryKeyboard.dwHotKey;
    result.keyboardVersion = '0'; //TODO
    result.startGroup = {
      ansi: binaryKeyboard.StartGroup_ANSI,
      unicode: binaryKeyboard.StartGroup_Unicode,
      newContext: -1, //TODO
      postKeystroke: -1 // TODO
    }

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

    offset = binaryKeyboard.dpGroupArray;
    for(let i = 0; i < binaryKeyboard.cxGroupArray; i++) {
      let binaryGroup = kmx.COMP_GROUP.fromBuffer(source.slice(offset));
      let group = new GROUP();
      group.dpMatch = this.readString(source, binaryGroup.dpMatch);
      group.dpName = this.readString(source, binaryGroup.dpName);
      group.dpNoMatch = this.readString(source, binaryGroup.dpNoMatch);
      group.fUsingKeys = binaryGroup.fUsingKeys;
      group.keys = [];

      let keyOffset = binaryGroup.dpKeyArray;
      for(let j = 0; j < binaryGroup.cxKeyArray; j++) {
        let binaryKey = kmx.COMP_KEY.fromBuffer(source.slice(keyOffset));
        let key = new KEY();
        key.Key = binaryKey.Key;
        key.Line = binaryKey.Line;
        key.ShiftFlags = binaryKey.ShiftFlags;
        key.dpContext = this.readString(source, binaryKey.dpContext);
        key.dpOutput = this.readString(source, binaryKey.dpOutput);
        group.keys.push(key);
        keyOffset += KMXFile.COMP_KEY_SIZE;
      }

      result.groups.push(group);
      offset += KMXFile.COMP_GROUP_SIZE;
    }

    // TODO: KMXPlusFile

    return result;
  }
};