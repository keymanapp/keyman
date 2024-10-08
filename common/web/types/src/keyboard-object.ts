/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { OutputTarget } from './outputTarget.interface.js';
import { TouchLayoutPlatform as LayoutFormFactorSpec } from './keyman-touch-layout/keyman-touch-layout-file.js';

export type ComplexKeyboardStore = (string | { t: 'd', d: number } | { ['t']: 'b' })[];

type KeyEvent = {};

/**
 * Stores preprocessed properties of a keyboard for quick retrieval later.
 */
export class CacheTag {
  stores: { [storeName: string]: ComplexKeyboardStore };

  constructor() {
    this.stores = {};
  }
}

export interface EncodedVisualKeyboard {
  /** Represents CSS font styling to use for VisualKeyboard text */
  F: string;
  /** Should there be a 102nd key? */
  K102?: boolean,
  /**
   * Keyboard Layer Specification: an object-based map of layer name to the keycaps for its
   * 65 keys.  The 65 keys are ordered from left to right, then top to bottom.
   *
   * The key ID corresponding to each index of the array is specified within `Codes.dfltCodes`.
   * Entries corresponding to `K_*` in `Codes.dfltCodes` are reserved for future use.
   */
  KLS?: { [layerName: string]: string[] },
  /**
   * @deprecated
   * The older form for data in KLS - defines keycaps for 'default' keys, then 'shift' keys,
   * in a single concatenated array.
   */
  BK?: string[];
}

export type LayoutSpec = {
  "desktop"?: LayoutFormFactorSpec,
  "phone"?: LayoutFormFactorSpec,
  "tablet"?: LayoutFormFactorSpec
}

export type KeyboardObject = {
  /**
   * Used internally by Keyman Engine for Web to hold preprocessed stores.
   */
  _kmw?: CacheTag;

  /**
   * group-start:  the function triggering processing for the keyboard's
   * "Unicode" start group, corresponding to `begin Unicode > use(_____)` in
   * Keyman keyboard language.
   * @param outputTarget  The context to which the keystroke applies
   * @param keystroke     The full, pre-processed keystroke triggering
   * keyboard-rule application.
   */
  gs(outputTarget: OutputTarget, keystroke: KeyEvent): boolean;

  /**
   * group-newcontext:  the function triggering processing for the keyboard's
   * "NewContext" start group, corresponding to `begin NewContext > use(_____)`
   * in Keyman keyboard language.
   * @param outputTarget  The new context to be used with future keystrokes
   * @param keystroke     A 'null' `KeyEvent` providing current modifier + state information.
   */
  gn?(outputTarget: OutputTarget, keystroke: KeyEvent): boolean;

  /**
   * group-postkeystroke:  the function triggering processing for the keyboard's
   * "PostKeystroke" start group, corresponding to `begin PostKeystroke >
   * use(_____)` in Keyman keyboard language.
   * @param outputTarget  The context altered by a recent keystroke.  As a
   * precondition, all changes due to `gs` / `begin Unicode` should already be
   * applied.
   * @param keystroke     A 'null' `KeyEvent` providing current modifier + state information.
   */
  gpk?(outputTarget: OutputTarget, keystroke: KeyEvent): boolean;

  /**
   * Keyboard ID:  the uniquely-identifying name for this keyboard.  Includes the standard
   * `Keyboard_` prefix.  May be 'namespaced' with a prefix corresponding to a package name
   *  within app/webview.
   */
  KI: string;
  /**
   * Keyboard Name:  the human-readable name of the keyboard.
   */
  KN: string;
  /**
   * Encoded data usable to construct a desktop/hardware-oriented on-screen keyboard.
   */
  KV: EncodedVisualKeyboard;
  /**
   * Keyboard Language Code: set within select keyboards.
   *
   * Currently, it's only used to determine the need for CJK-picker support.  Is missing
   * in most compiled keyboards.
   */
  KLC?: string;
  /**
   * @deprecated
   * Keyboard Language Code: set within select keyboards.
   *
   * Currently, it's only used to determine the need for CJK-picker support.
   * Is (probably) an older name of KLC with the identical purpose.  Is missing
   * in most compiled keyboards.
   */
  LanguageCode?: string;
  /**
   * Keyboard CSS: provides the definition for custom keyboard style sheets
   */
  KCSS?: string;
  /**
   * Keyboard is RTL: a simple flag noting if the keyboard's script is RTL.
   */
  KRTL?: boolean;
  /**
   * Keyboard Modifier BitMask:  a set of bitflags indicating which modifiers
   * the keyboard's rules utilize.  See also: `ModifierKeyConstants`.
   */
  KMBM?: number;
  /**
   * Keyboard Supplementary plane:  set to 1 if the keyboard uses non-BMP Unicode
   * characters.
   */
  KS?: number;
  /**
   * Keyman Visual Keyboard Layout:  defines the touch-layout definitions used for
   * 'phone' and 'tablet' form-factors.
   */
  KVKL?: LayoutSpec;
  /**
   * Keyboard is Mnemonic: set to 1 if the keyboard uses a mnemonic layout.
   */
  KM?: number;
  /**
   * KeyBoard VERsion: the version of this keyboard.
   */
  KBVER?: string;
  /**
   * Keyman VERsion:  the version of Keyman Developer used to compile this keyboard.
   */
  KVER?: string;
  /**
   * Keyman Variable Stores: an array of the names of all variable stores used by the
   * keyboard.
   */
  KVS?: (`s${number}`)[];
  /**
   * Keyboard Help: HTML help text, as specified by either the &kmw_helptext or &kmw_helpfile system stores.
   *
   * Reference: https://help.keyman.com/developer/language/reference/kmw_helptext,
   *            https://help.keyman.com/developer/language/reference/kmw_helpfile
   */
  KH?: string;
  /**
   * Keyboard Virtual Key Dictionary: the Developer-compiled, minified dictionary of virtual-key codes
   */
  KVKD?: string;
  /**
   * Keyboard Display Underlying:  set to 1 if the desktop form of the keyboard
   * should show the US QWERTY underlying keycaps.  These may also appear on
   * touch layouts if set and no touch-layout information is available.
   */
  KDU?: number;
  /**
   * Virtual Key Dictionary: the engine pre-processed, unminified dictionary.  This is built within
   * Keyman Engine for Web at runtime as needed based on the definitions in `KVKD`.
   */
  VKDictionary?: Record<string, number>,
  /**
   * Keyboard Help File: Embedded JS script designed for use with a keyboard's
   * HTML help text.  Always defined within the file referenced by &kmw_embedjs
   * in a keyboard's source, though that file may also contain _other_ script
   * definitions as well.  (`KHF` must be explicitly defined within that file.)
   * @param e  Will be provided with the root element (a <div>) of the On-Screen Keyboard.
   * @returns
   */
  KHF?: (e: any) => string;

  /**
   * Keyboard Notify Shift:  Provided by CJK-picker keyboards to properly
   * interface them with Keyman Engine for Web.
   * @param       {number}    _PCommand     event code (16,17,18) or 0; 16-18
   * correspond to modifier codes when pressed, while 0 corresponds to loss of focus
   * @param       {Object}    _PTarget      target element
   * @param       {number}    _PData        1 or 0
   * @returns
   */
  KNS?: (_PCommand: number, _PTarget: OutputTarget, _PData: number) => void;
} & Record<`s${number}`, string>

