//
// Conforms to 45
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
  keyboard3: LKKeyboard;
};

export interface LKKeyboard {
  locale?: string;
  conformsTo?: string;

  locales?: LKLocales;
  version?: LKVersion;
  info?: LKInfo;
  settings?: LKSettings;
  keys?: LKKeys;
  flicks?: LKFlicks;
  forms?: LKForms;
  displays?: LKDisplays;
  layers?: LKLayers[];
  variables?: LKVariables;
  transforms?: LKTransforms[];
};

/**
 * This is defined as an interface, but actually is resolved during the reading phase
 */
export interface LKImport {
  /**
   * import base, currently `cldr` is supported
   */
  base?: 'cldr' | '';
  /**
   * path to imported resource, of the form `45/*.xml`
   */
  path: string;
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
  name?: string;
  author?: string;
  layout?: string;
  indicator?: string;
};

export interface LKSettings {
  normalization: "disabled";
};

export interface LKKeys {
  key: LKKey[];
  flicks: LKFlicks[];
};

export interface LKKey {
  id?: string;
  flickId?: string;
  output?: string;
  gap?: boolean;
  layerId?: string;
  longPressKeyIds?: string;
  longPressDefaultKeyId?: string;
  multiTapKeyIds?: string;
  width?: number;
};

export interface LKFlicks {
  flick?: LKFlick[];
};

export interface LKFlick {
  id?: string;
  flickSegment?: LKFlickSegment[];
};

export interface LKFlickSegment {
  directions?: string;
  keyId?: string;
}

export interface LKLayers {
  /**
   * `touch`, or hardware `us`, `iso`, `jis`, `abnt2`
   */
  formId?: string;
  /**
   * Minimum width in millimeters
   */
  minDeviceWidth?: number;
  layer?: LKLayer[];
};

export interface LKLayer {
  id?: string;
  modifiers?: string;
  row?: LKRow[];
};

export interface LKRow {
  keys?: string;
};

export interface LKVariables {
  string?: LKString[];
  set?: LKSet[];
  uset?: LKUSet[];
};

/**
 * Shared interface for all three variable types
 */
export interface Variable {
  id?: string;
  value?: string;
};

export interface LKString extends Variable {};
export interface LKSet extends Variable {};
export interface LKUSet extends Variable {};

export interface LKTransforms {
  type?: "simple" | "backspace";
  transformGroup?: LKTransformGroup[];
};

export interface LKTransform {
  from?: string;
  to?: string;
};

export interface LKTransformGroup {
  // one or the other, not both
  transform?: LKTransform[];
  reorder?: LKReorder[];
};
export interface LKReorder {
  from?: string;
  before?: string;
  order?: string;
  tertiary?: string;
  tertiaryBase?: string;
  preBase?: string;
};

export interface LKDisplayOptions {
  baseCharacter?: string;
};

export interface LKDisplay {
  output?: string;
  display?: string;
  keyId?: string;
};

export interface LKDisplays {
  display?: LKDisplay[];
  displayOptions?: LKDisplayOptions;
};

export interface LKForms {
  form?: LKForm[];
};

export interface LKForm {
  id?: string;
  scanCodes?: LKScanCodes[];
};

export interface LKScanCodes {
  codes?: string;
};

/**
 * Utilities for determining the import status of items
 */
export class ImportStatus {
  /** item came in via implied (spec based) import, such as keys-Latn-implied.xml */
  static impliedImport = Symbol('LDML implied import');
  /** item came in via import */
  static import = Symbol('LDML import');
  /** item came in via local (not CLDR) import */
  static localImport = Symbol('LDML local import');

  /** @returns true if the object was loaded through an implied import */
  static isImpliedImport(o : any) : boolean {
    return o && !!o[ImportStatus.impliedImport];
  }
  /** @returns true if the object was loaded through an explicit import */
  static isImport(o : any) : boolean {
    return o && !!o[ImportStatus.import];
  }
  /** @returns true if the object was loaded through an explicit import */
  static isLocalImport(o : any) : boolean {
    return o && !!o[ImportStatus.localImport];
  }
};

