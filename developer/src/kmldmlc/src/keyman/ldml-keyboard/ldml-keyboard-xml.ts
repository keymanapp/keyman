//
// Conforms to techpreview
//
// The interfaces in this file are designed with reference to the mapped
// structures produced by xml2js when passed a LDML keyboard .xml file.
//
// Using prefix LK for LDML Keyboard
//

export default interface LDMLKeyboardXMLSourceFile {
  /**
   * <keyboard> -- the root element.
   */
  keyboard: LKKeyboard;
}

export interface LKKeyboard {
  locale?: string;
  conformsTo?: string;

  info?: LKInfo;
  names?: LKNames;
  keys?: LKKeys;
  layerMaps?: LKLayerMaps[];
};

export interface LKInfo {
  author?: string;
  indicator?: string;
  layout?: string;
  normalization?: string;
};

export interface LKNames {
  name: LKName[];
};

export interface LKName {
  value?: string;
};

export interface LKKeys {
  key: LKKey[];
};

export interface LKKey {
  id?: string;
  to?: string;
};

export interface LKLayerMaps {
  form?: string;
  layerMap?: LKLayerMap[];
};

export interface LKLayerMap {
  id?: string;
  row?: LKRow[];
};

export interface LKRow {
  keys?: string;
};
