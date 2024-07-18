import KVKFile, { BUILDER_KVK_FILE, BUILDER_KVK_HEADER_IDENTIFIER, BUILDER_KVK_HEADER_VERSION } from "./kvk-file.js";
import { VisualKeyboard, VisualKeyboardKey } from "./visual-keyboard.js";

export default class KvkFileReader {
  public read(source: Uint8Array): VisualKeyboard {
    let binary: BUILDER_KVK_FILE;
    let kvk = new KVKFile();
    binary = kvk.KVK_FILE.fromBuffer(source);
    if(binary.header.identifier != BUILDER_KVK_HEADER_IDENTIFIER ||
        binary.header.version != BUILDER_KVK_HEADER_VERSION) {
      return null;
    }
    let result = new VisualKeyboard();
    result.header.version = binary.header.version;
    result.header.flags = binary.header.flags;
    result.header.associatedKeyboard = binary.header.associatedKeyboard.str;
    result.header.ansiFont.name = binary.header.ansiFont.name.str;
    result.header.ansiFont.size = binary.header.ansiFont.size;
    result.header.unicodeFont.name = binary.header.unicodeFont.name.str;
    result.header.unicodeFont.size = binary.header.unicodeFont.size;
    for(let binaryKey of binary.keys) {
      let key: VisualKeyboardKey = {
        flags: binaryKey.flags,
        vkey: binaryKey.vkey,
        shift: binaryKey.shift,
        text: binaryKey.text.str
      };
      if(binaryKey.bitmapSize) {
        key.bitmap = new Uint8Array(binaryKey.bitmapData);
      }

      result.keys.push(key);
    }
    return result;
  }
};