

export interface KeylayoutXMLSourceFile {
  /**
   * <keyboard> -- the root element.
   */
  keyboard: KL_keyboard;
};
export interface KL_keyboard {
  group?: string;
  id?: string;
  name?: string;
  maxoutS?: string;

  layoutsMM: KL_Layouts[];
  layouts?: KL_Layouts[];
  modifierMap?: KL_modifierMap[];
  keyMapSet?: KL_keyMapSet[];
  actions?: KL_actions[];
  terminators?: KL_terminators[];
};

export interface KL_Layouts {
  layouts?: KL_Layout;
};
export interface KL_Layout {
  first?: string;
  last?: string;
  mapSet?: string;
  modifiers?: string;
};


export interface KL_modifierMap {
  id?: string;
  defaultIndex?: string;

  keyMapSelect?: KL_keyMapSelect[];
};
export interface KL_keyMapSelect {
  mapIndex?: string;
  modifier?: KL_modifier[];
};
export interface KL_modifier {
  keys?: string;
};


export interface KL_keyMapSet {
  id?: string;
  keyMap?: KL_keyMap[];
};
export interface KL_keyMap {
  index?: string;
  key?: KL_key[];
};
export interface KL_key {
  code?: string;
  action?: string;
  output?: string;
};


export interface KL_actions {
  action?: KL_action[];
};
export interface KL_action {
  id?: string;
  when?: KL_when[];
};
export interface KL_when {
  state?: string;
  output?: string;
  next?: string;
};

export interface KL_terminators {
  when?: KL_when[];
};


//--------------------------------------------------------

export interface LDMLKeyboardTestDataXMLSourceFile {
  /**
   * <keyboardTest> -- the root element.
   */
  keyboardTest3: LKTKeyboardTest;   // keyboardTest3 is root 
}

export interface LKTKeyboardTest {    // ubergeordneter node keyboardTest3
  conformsTo?: string;                // keyboardTest3 has attrib?  conformsTo
  info?: LKTInfo;                     // keyboardTest3 has tag  info, rep, tests
  repertoire?: LKTRepertoire[];
  tests?: LKTTests[];
};

export interface LKTInfo {
  author?: string;
  keyboard?: string;
  name?: string;
};

export interface LKTRepertoire {
  name?: string;
  chars?: string;
  type?: string;
};

export interface LKTTests {     // subtag tests has tag ?/attrib? name, test 
  name?: string;
  test?: LKTTest[];
};

export interface LKTTest {      // sub-subtag test has tag ?/attrib? name, startContext,  actions
  name?: string;
  startContext?: LKTStartContext;
  actions?: LKTAnyAction[];  // differs from XML, to represent order of actions
};

/**
 * Test Actions.
 * The expectation is that each LKTAction object will have exactly one non-falsy field.
 */
export interface LKTAction {
  type?: "check" | "emit" | "keystroke" | "backspace";
};

export interface LKTCheck extends LKTAction {
  type: "check";
  result?: string;
};

export interface LKTEmit extends LKTAction {
  type: "emit";
  to?: string;
};

export interface LKTKeystroke extends LKTAction {
  type: "keystroke";
  key?: string;
  flick?: string;
  longPress?: string;
  tapCount?: string;
};
export interface LKTStartContext {
  to?: string;
};

export interface LDMLKeyboardXMLSourceFile {
  /**
   * <keyboard> -- the root element.
   */
  keyboard3: LKKeyboard;
};


export interface LKTBackspace extends LKTAction {
  type: "backspace";
}

export type LKTAnyAction = LKTCheck | LKTEmit | LKTKeystroke | LKTBackspace;










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

export interface LKString extends Variable { };
export interface LKSet extends Variable { };
export interface LKUSet extends Variable { };

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
  static isImpliedImport(o: any): boolean {
    return o && !!o[ImportStatus.impliedImport];
  }
  /** @returns true if the object was loaded through an explicit import */
  static isImport(o: any): boolean {
    return o && !!o[ImportStatus.import];
  }
  /** @returns true if the object was loaded through an explicit import */
  static isLocalImport(o: any): boolean {
    return o && !!o[ImportStatus.localImport];
  }
};

