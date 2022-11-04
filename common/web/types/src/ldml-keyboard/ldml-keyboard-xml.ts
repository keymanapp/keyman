//
// Conforms to techpreview
//
// The interfaces in this file are designed with reference to the mapped
// structures produced by xml2js when passed a LDML keyboard .xml file.
//
// Using prefix LK for LDML Keyboard
//

export interface LDMLKeyboardXMLSourceFile {
  /**
   * <keyboard> -- the root element.
   */
  keyboard: LKKeyboard;
}

export interface LKKeyboard {
  locale?: string;
  conformsTo?: string;

  locales?: LKLocales;
  version?: LKVersion;
  info?: LKInfo;
  names?: LKNames;
  settings?: LKSettings;
  keys?: LKKeys;
  displays?: LKDisplays;
  layers?: LKLayers[];
  vkeys?: LKVkeys;
  transforms?: LKTransforms[];
  reorders?: LKReorders;
};

export interface LKLocales {
  locale: LKLocale[];
};

export interface LKLocale {
  id?: string;
};

export interface LKVersion {
  number: string; // semver string
}

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

export interface LKSettings {
  fallback: "omit";
  transformFailure: "omit";
  transformPartial: "hide";
};

export interface LKKeys {
  key: LKKey[];
};

export interface LKKey {
  id?: string;
  to?: string;
  gap?: boolean;
  switch?: string;
};

export interface LKLayers {
  /**
   * `hardware` or `touch`
   */
  form?: string;
  /**
   * `us`, `iso`, `jis`, or `abnt2`
   */
  hardware?: string;
  /**
   * Minimum width in millimeters
   */
  minDeviceWidth?: number;
  layer?: LKLayer[];
};

export interface LKLayer {
  id?: string;
  modifier?: string;
  row?: LKRow[];
};

export interface LKRow {
  keys?: string;
};

export interface LKVkeys {
  vkey?: LKVkey[];
};

export interface LKVkey {
  from?: string;
  to?: string;
};

export interface LKTransforms {
  type?: "simple" | "final";
  transform: LKTransform[];
};

export interface LKTransform {
  from?: string;
  to?: string;
  before?: string;
  error?: "fail";
};

export interface LKReorders {
  reorder: LKReorder[];
};

export interface LKReorder {
  from?: string;
  before?: string;
  order?: string;
  tertiary?: string;
  tertiary_base?: string;
  prebase?: string;
};

export interface LKDisplayOptions {
  baseCharacter?: string;
};

export interface LKDisplay {
  to?: string;
  display?: string;
};

export interface LKDisplays {
  display?: LKDisplay[];
  displayOptions?: LKDisplayOptions;
};
