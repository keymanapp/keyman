import { KMXFile, BUILDER_COMP_KEYBOARD, KEYBOARD, STORE, GROUP, KEY } from "./kmx.js";
import * as r from 'restructure';
import { KeymanTypesError } from "../util/errors.js";

export class KmxFileReaderError extends KeymanTypesError {

}

export class KmxFileReader {
  private readonly rString = new r.String(null, 'utf16le');

  private readString(source: Uint8Array, offset: number): string {
    if(offset == 0) {
      // the file has a null string here, which is not the same as an empty
      // string ('')
      return null;
    }
    // The following two lines are equivalent to :
    //   return this.rString.fromBuffer(source.slice(offset));
    // but is much faster because it is a read-only view into
    // the data rather than a copy
    const data = new Uint8Array(source.buffer, source.byteOffset + offset);
    return this.rString.fromBuffer(data);
  }

  private isValidCodeUse(s: string, keyboard: KEYBOARD): boolean {
    return (
      s.length == 3 &&
      s.charCodeAt(0) == KMXFile.UC_SENTINEL &&
      s.charCodeAt(1) == KMXFile.CODE_USE &&
      s.charCodeAt(2) > 0 &&
      s.charCodeAt(2) <= keyboard.groups.length
    );
  }

  private processSystemStore(store: STORE, result: KEYBOARD): void {
    switch(store.dwSystemID) {
      case KMXFile.TSS_MNEMONIC:
        result.isMnemonic = store.dpString == '1';
        break;
      case KMXFile.TSS_KEYBOARDVERSION:
        result.keyboardVersion = store.dpString;
        break;
      case KMXFile.TSS_TARGETS:
        result.targets = store.dpString;
        break;
      case KMXFile.TSS_BEGIN_NEWCONTEXT:
        if(!this.isValidCodeUse(store.dpString, result)) {
          throw new KmxFileReaderError(`Invalid TSS_BEGIN_NEWCONTEXT system store`);
        }
        result.startGroup.newContext = store.dpString.charCodeAt(2) - 1;
        break;
      case KMXFile.TSS_BEGIN_POSTKEYSTROKE:
        if(!this.isValidCodeUse(store.dpString, result)) {
          throw new KmxFileReaderError(`Invalid TSS_BEGIN_POSTKEYSTROKE system store`);
        }
        result.startGroup.postKeystroke = store.dpString.charCodeAt(2) - 1;
        break;
    }
  }

  public read(source: Uint8Array): KEYBOARD {
    let binaryKeyboard: BUILDER_COMP_KEYBOARD;
    let kmx = new KMXFile();
    binaryKeyboard = kmx.COMP_KEYBOARD.fromBuffer(source);
    if(binaryKeyboard.dwIdentifier != KMXFile.FILEID_COMPILED) {
      throw new KmxFileReaderError(`Not a .kmx file: header does not contain FILEID_COMPILED`);
    }

    let result = new KEYBOARD();
    result.fileVersion = binaryKeyboard.dwFileVersion;
    result.flags = binaryKeyboard.dwFlags;
    result.hotkey = binaryKeyboard.dwHotKey;
    result.startGroup = {
      ansi: binaryKeyboard.StartGroup_ANSI == 0xFFFFFFFF ? -1 : binaryKeyboard.StartGroup_ANSI,
      unicode: binaryKeyboard.StartGroup_Unicode == 0xFFFFFFFF ? -1 : binaryKeyboard.StartGroup_Unicode,
      newContext: -1,
      postKeystroke: -1
    }

    // Informative data

    result.keyboardVersion = '';
    result.isMnemonic = false;
    result.targets = 'desktop';

    this.readStores(binaryKeyboard, kmx, source, result);
    this.readGroupsAndRules(binaryKeyboard, kmx, source, result);

    // Process system stores once we have all stores and groups loaded
    for(let store of result.stores) {
      this.processSystemStore(store, result);
    }

    // TODO: KMXPlusFile

    // Validate startGroup offsets
    let gp: keyof KEYBOARD['startGroup'];
    for(gp in result.startGroup) {
      if(result.startGroup[gp] < -1 || result.startGroup[gp] >= result.groups.length) {
        throw new KmxFileReaderError(`Invalid begin group reference`);
      }
    }

    return result;
  }

  private readGroupsAndRules(binaryKeyboard: BUILDER_COMP_KEYBOARD, kmx: KMXFile, source: Uint8Array, result: KEYBOARD) {
    let offset = binaryKeyboard.dpGroupArray;
    for (let i = 0; i < binaryKeyboard.cxGroupArray; i++) {
      const data = new Uint8Array(source.buffer, source.byteOffset + offset);
      let binaryGroup = kmx.COMP_GROUP.fromBuffer(data);
      let group = new GROUP();
      group.dpMatch = this.readString(source, binaryGroup.dpMatch);
      group.dpName = this.readString(source, binaryGroup.dpName);
      group.dpNoMatch = this.readString(source, binaryGroup.dpNoMatch);
      group.fUsingKeys = binaryGroup.fUsingKeys;
      group.keys = [];

      let keyOffset = binaryGroup.dpKeyArray;
      for (let j = 0; j < binaryGroup.cxKeyArray; j++) {
        const keyData = new Uint8Array(source.buffer, source.byteOffset + keyOffset);
        let binaryKey = kmx.COMP_KEY.fromBuffer(keyData);
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
  }

  private readStores(binaryKeyboard: BUILDER_COMP_KEYBOARD, kmx: KMXFile, source: Uint8Array, result: KEYBOARD): void {
    let offset = binaryKeyboard.dpStoreArray;
    for (let i = 0; i < binaryKeyboard.cxStoreArray; i++) {
      const data = new Uint8Array(source.buffer, source.byteOffset + offset);
      let binaryStore = kmx.COMP_STORE.fromBuffer(data);
      let store = new STORE();
      store.dwSystemID = binaryStore.dwSystemID;
      store.dpName = this.readString(source, binaryStore.dpName);
      store.dpString = this.readString(source, binaryStore.dpString);
      result.stores.push(store);
      offset += KMXFile.COMP_STORE_SIZE;
    }
  }
};