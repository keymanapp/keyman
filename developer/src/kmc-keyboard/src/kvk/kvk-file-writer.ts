import KVKFile, { BUILDER_KVK_FILE, BUILDER_KVK_HEADER_IDENTIFIER, BUILDER_KVK_HEADER_VERSION, BUILDER_KVK_KEY, BUILDER_KVK_STRING } from "./kvk-file.js";
import { VisualKeyboard } from "./visual-keyboard.js";

export default class KvkFileWriter {
  private source: VisualKeyboard;

  constructor(source: VisualKeyboard) {
    this.source = source;
  }

  /**
   * Fills a kvk string from a source string. Returns byte size of s as UTF-16
   * zero terminated string for KVK format Note that the format includes both a
   * length word and zero termination.
   * @param str
   * @param value
   * @returns number
   */
   private setString(str: BUILDER_KVK_STRING, value: string): void {
    str.len = value.length + 1;
    str.str = value;
  }

  private build() {
    let binary: BUILDER_KVK_FILE = {
      header: {
        identifier: BUILDER_KVK_HEADER_IDENTIFIER,
        version: BUILDER_KVK_HEADER_VERSION,
        associatedKeyboard: {len:0,str:''},
        flags: this.source.header.flags,
        ansiFont:{
          color: this.source.header.ansiFont.color,
          size: this.source.header.ansiFont.size,
          name: {len:0,str:''}
        },
        unicodeFont:{
          color: this.source.header.unicodeFont.color,
          size: this.source.header.unicodeFont.size,
          name: {len:0,str:''}
        },
      },
      keyCount: this.source.keys.length,
      keys:[]
    };

    this.setString(binary.header.associatedKeyboard, this.source.header.associatedKeyboard);
    this.setString(binary.header.ansiFont.name, this.source.header.ansiFont.name);
    this.setString(binary.header.unicodeFont.name, this.source.header.unicodeFont.name);

    for(let sourceKey of this.source.keys) {
      const binaryKey: BUILDER_KVK_KEY = {
        flags: sourceKey.flags,
        vkey: sourceKey.vkey,
        shift: sourceKey.shift,
        text: { len: 0, str: '' },
        bitmap: 0
      };
      this.setString(binaryKey.text, sourceKey.text);
      binary.keys.push(binaryKey);
    }

    return binary;
  }

  compile(): Uint8Array {
    const binary = this.build();
    const kvk = new KVKFile();
    const file: Uint8Array = new Uint8Array(kvk.KVK_FILE.size(binary));
    const data = kvk.KVK_FILE.toBuffer(binary);
    file.set(data, 0);
    return file;
  }
};