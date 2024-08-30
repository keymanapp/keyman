// .kvks xml format
// See VisualKeyboardLoaderXML.pas, not the same as VisualKeyboardImportXML.pas!

export default interface KVKSourceFile {
  /**
   * <visualkeyboard> -- the root element.
   */
  visualkeyboard: KVKSVisualKeyboard;
}

export interface KVKSVisualKeyboard {
  header?: KVKSHeader;
  encoding?: KVKSEncoding[];
};

export interface KVKSHeader {
  version?: string;
  kbdname?: string;
  flags?: KVKSFlags;
  layout?: string;
};

export interface KVKSFlags {
  key102?: string;
  displayunderlying?: string;
  useunderlying?: string;
  usealtgr?: string;
};

export interface KVKSEncoding {
  name?: string;
  fontname?: string;
  fontsize?: string;
  layer?: KVKSLayer[];
};

export interface KVKSLayer {
  shift?: string;
  key?: KVKSKey[];
};

export interface KVKSKey {
  vkey?: string;
  bitmap?: string;
  '#text'?: string;
};

