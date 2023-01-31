import KVKFile, { BUILDER_KVK_FILE, BUILDER_KVK_HEADER_IDENTIFIER, BUILDER_KVK_HEADER_VERSION, BUILDER_KVK_KEY, BUILDER_KVK_STRING } from "./kvk-file.js";
import { VisualKeyboard } from "./visual-keyboard.js";

export default class KvkFileWriter {
  /**
   * Writes the visual keyboard to a binary .kvk format byte array.
   * @param source VisualKeyboard
   * @returns Uint8Array, the .kvk file
   */
  write(source: VisualKeyboard): Uint8Array {
    const binary = this.build(source);
    const kvk = new KVKFile();
    const file: Uint8Array = new Uint8Array(kvk.KVK_FILE.size(binary));
    const data = kvk.KVK_FILE.toBuffer(binary);
    file.set(data, 0);
    return file;
  }

  private build(source: VisualKeyboard) {
    const binary: BUILDER_KVK_FILE = {
      header: {
        identifier: BUILDER_KVK_HEADER_IDENTIFIER,
        version: BUILDER_KVK_HEADER_VERSION,
        associatedKeyboard: {len:0,str:''},
        flags: source.header.flags,
        ansiFont:{
          color: source.header.ansiFont.color,
          size: source.header.ansiFont.size,
          name: {len:0,str:''}
        },
        unicodeFont:{
          color: source.header.unicodeFont.color,
          size: source.header.unicodeFont.size,
          name: {len:0,str:''}
        },
      },
      keyCount: source.keys.length,
      keys:[]
    };

    this.setString(binary.header.associatedKeyboard, source.header.associatedKeyboard);
    this.setString(binary.header.ansiFont.name, source.header.ansiFont.name);
    this.setString(binary.header.unicodeFont.name, source.header.unicodeFont.name);

    for(let sourceKey of source.keys) {
      const binaryKey: BUILDER_KVK_KEY = {
        flags: sourceKey.flags,
        vkey: sourceKey.vkey,
        shift: sourceKey.shift,
        text: { len: 0, str: '' },
        bitmap: 0
      };
      this.setString(binaryKey.text, sourceKey.text || '');
      binary.keys.push(binaryKey);
    }

    return binary;
  }

  /**
   * Fills a kvk string from a source string. Note that the format includes both
   * a length word and zero termination.
   *
   * @param str
   * @param value
   * @returns number
   */
   private setString(str: BUILDER_KVK_STRING, value: string): void {
    str.len = value.length + 1;
    str.str = value;
  }
};
