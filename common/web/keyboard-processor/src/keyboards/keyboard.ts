import Codes from "../text/codes.js";
import { EncodedVisualKeyboard, LayoutSpec, Layouts } from "./defaultLayouts.js";
import { ActiveKey, ActiveLayout, ActiveSubKey } from "./activeLayout.js";
import KeyEvent from "../text/keyEvent.js";
import type OutputTarget from "../text/outputTarget.js";
import { ModifierKeyConstants, TouchLayout } from "@keymanapp/common-types";
type TouchLayoutSpec = TouchLayout.TouchLayoutPlatform & { isDefault?: boolean};

import { Version, DeviceSpec } from "@keymanapp/web-utils";
import StateKeyMap from "./stateKeyMap.js";

type ComplexKeyboardStore = ( string | { t: 'd', d: number } | { ['t']: 'b' })[];

/**
 * Stores preprocessed properties of a keyboard for quick retrieval later.
 */
class CacheTag {
  stores: {[storeName: string]: ComplexKeyboardStore};

  constructor() {
    this.stores = {};
  }
}

export enum LayoutState {
  NOT_LOADED = undefined,
  POLYFILLED = 1,
  CALIBRATED = 2
}

export interface VariableStoreDictionary {
  [name: string]: string;
};

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


/**
 * Acts as a wrapper class for Keyman keyboards compiled to JS, providing type information
 * and keyboard-centered functionality in an object-oriented way without modifying the
 * wrapped keyboard itself.
 */
export default class Keyboard {
  public static DEFAULT_SCRIPT_OBJECT: KeyboardObject = {
    'gs': function(outputTarget: OutputTarget, keystroke: KeyEvent) { return false; }, // no matching rules; rely on defaultRuleOutput entirely
    'KI': '', // The currently-existing default keyboard ID; we already have checks that focus against this.
    'KN': '',
    'KV': Layouts.DEFAULT_RAW_SPEC,
    'KM': 0 // May not be the best default, but this matches current behavior when there is no activeKeyboard.
  }

  /**
   * This is the object provided to KeyboardInterface.registerKeyboard - that is, the keyboard
   * being wrapped.
   *
   * TODO:  Make this private instead.  But there are a LOT of references that must be rooted out first.
   */
  public readonly scriptObject: KeyboardObject;
  private layoutStates: {[layout: string]: LayoutState};

  constructor(keyboardScript: any) {
    if(keyboardScript) {
      this.scriptObject = keyboardScript;
    } else {
      this.scriptObject = Keyboard.DEFAULT_SCRIPT_OBJECT;
    }
    this.layoutStates = {};
  }

  /**
   * Calls the keyboard's `gs` function, which represents the keyboard source's begin Unicode group.
   */
  process(outputTarget: OutputTarget, keystroke: KeyEvent): boolean {
    return this.scriptObject['gs'](outputTarget, keystroke);
  }

  /**
   * Calls the keyboard's `gn` function, which represents the keyboard source's begin newContext group.
   */
  processNewContextEvent(outputTarget: OutputTarget, keystroke: KeyEvent): boolean {
    return this.scriptObject['gn'] ? this.scriptObject['gn'](outputTarget, keystroke) : false;
  }

  /**
   * Calls the keyboard's `gpk` function, which represents the keyboard source's begin postKeystroke group.
   */
  processPostKeystroke(outputTarget: OutputTarget, keystroke: KeyEvent): boolean {
    return this.scriptObject['gpk'] ? this.scriptObject['gpk'](outputTarget, keystroke) : false;
  }

  get isHollow(): boolean {
    return this.scriptObject == Keyboard.DEFAULT_SCRIPT_OBJECT;
  }

  get id(): string {
    return this.scriptObject['KI'];
  }

  get name(): string {
    return this.scriptObject['KN'];
  }

  /**
   * Cache variable store values
   *
   * Primarily used for predictive text to prevent variable store
   * values from being changed in 'fat finger' processing.
   *
   * KVS is available in keyboards compiled with Keyman Developer 15
   * and later versions. See #2924.
   *
   * @returns an object with each property referencing a variable store
   */
  get variableStores(): VariableStoreDictionary {
    const storeNames = this.scriptObject['KVS'];
    let values: VariableStoreDictionary = {};
    if(Array.isArray(storeNames)) {
      for(let store of storeNames) {
        values[store] = this.scriptObject[store];
      }
    }
    return values;
  }

  /**
   * Restore variable store values from cache
   *
   * KVS is available in keyboards compiled with Keyman Developer 15
   * and later versions. See #2924.
   *
   * @param values  name-value pairs for each store value
   */
  set variableStores(values: VariableStoreDictionary) {
    const storeNames = this.scriptObject['KVS'];
    if(Array.isArray(storeNames)) {
      for(let store of storeNames) {
        // If the value is not present in the cache, don't overwrite it;
        // while this is not used in initial implementation, we could use
        // it in future to update a single variable store value rather than
        // the whole cache.
        if(typeof values[store] == 'string') {
          this.scriptObject[store] = values[store];
        }
      }
    }
  }

  private get _legacyLayoutSpec() {
    return this.scriptObject['KV'];  // used with buildDefaultLayout; layout must be constructed at runtime.
  }

  // May return null if no layouts exist or have been initialized.
  private get _layouts(): LayoutSpec {
    return this.scriptObject['KVKL'];  // This one is compiled by Developer's visual keyboard layout editor.
  }

  private set _layouts(value: LayoutSpec) {
    this.scriptObject['KVKL'] = value;
  }

  get compilerVersion(): Version {
    return new Version(this.scriptObject['KVER']);
  }

  get isMnemonic(): boolean {
    return !!this.scriptObject['KM'];
  }

  get definesPositionalOrMnemonic(): boolean {
    return typeof this.scriptObject['KM'] != 'undefined';
  }

  /**
   * HTML help text, as specified by either the &kmw_helptext or &kmw_helpfile system stores.
   *
   * Reference: https://help.keyman.com/developer/language/reference/kmw_helptext,
   *            https://help.keyman.com/developer/language/reference/kmw_helpfile
   */
  get helpText(): string {
    return this.scriptObject['KH'];
  }

  /**
   * Embedded JS script designed for use with a keyboard's HTML help text.  Always defined
   * within the file referenced by &kmw_embedjs in a keyboard's source, though that file
   * may also contain _other_ script definitions as well.  (`KHF` must be explicitly defined
   * within that file.)
   */
  get hasScript(): boolean {
    return !!this.scriptObject['KHF'];
  }

  /**
   * Embeds a custom script for use by the OSK, which may be interactive (like with sil_euro_latin).
   * Note:  this must be called AFTER any contents of `helpText` have been inserted into the DOM.
   * (See sil_euro_latin's source -> sil_euro_latin_js.txt)
   *
   * Reference: https://help.keyman.com/developer/language/reference/kmw_embedjs
   */
  embedScript(e: any) {
    // e:  Expects the OSKManager's _Box element.  We don't add type info here b/c it would
    //     reference the DOM.
    this.scriptObject['KHF'](e);
  }

  get oskStyling(): string {
    return this.scriptObject['KCSS'];
  }

  /**
   * true if this keyboard uses a (legacy) pick list (Chinese, Japanese, Korean, etc.)
   *
   * TODO:  Make a property on keyboards (say, `isPickList` / `KPL`) to signal this when we
   *        get around to better, generalized picker-list support.
   */
  get isCJK(): boolean { // I3363 (Build 301)
    var lg: string;
    if(typeof(this.scriptObject['KLC']) != 'undefined') {
      lg = this.scriptObject['KLC'];
    } else if(typeof(this.scriptObject['LanguageCode']) != 'undefined') {
      lg = this.scriptObject['LanguageCode'];
    }

    // While some of these aren't proper BCP-47 language codes, the CJK keyboards predate our use of BCP-47.
    // So, we preserve the old ISO 639-3 codes, as that's what the keyboards are matching against.
    return ((lg == 'cmn') || (lg == 'jpn') || (lg == 'kor'));
  }

  get isRTL(): boolean {
    return !!this.scriptObject['KRTL'];
  }

  /**
   * Obtains the currently-active modifier bitmask for the active keyboard.
   */
  get modifierBitmask(): number {
    // NON_CHIRAL is the default bitmask if KMBM is not defined.
    // We always need a bitmask to compare against, as seen in `isChiral`.
    return this.scriptObject['KMBM'] || Codes.modifierBitmasks['NON_CHIRAL'];
  }

  get isChiral(): boolean {
    return !!(this.modifierBitmask & Codes.modifierBitmasks['IS_CHIRAL']);
  }

  get desktopFont(): string {
    if(this.scriptObject['KV']) {
      return this.scriptObject['KV']['F'];
    } else {
      return null;
    }
  }

  private get cacheTag(): CacheTag {
    let tag = this.scriptObject['_kmw'];

    if(!tag) {
      tag = new CacheTag();
      this.scriptObject['_kmw'] = tag;
    }

    return tag;
  }

  get explodedStores(): {[storeName: string]: ComplexKeyboardStore} {
    return this.cacheTag.stores;
  }

  /**
   * Signifies whether or not a layout or OSK should include AltGr / Right-alt emulation for this keyboard.
   * @param   {Object=}   keyLabels
   * @return  {boolean}
   */
  get emulatesAltGr(): boolean {
    // If we're not chiral, we're not emulating.
    if(!this.isChiral) {
      return false;
    }

    if(this._legacyLayoutSpec == null) {
      return false;
    }

    // Only exists in KMW 10.0+, but before that Web had no chirality support, so... return false.
    let layers = this._legacyLayoutSpec['KLS'];
    if(!layers) {
      return false;
    }

    var emulationMask = ModifierKeyConstants.LCTRLFLAG | ModifierKeyConstants.LALTFLAG;
    var unshiftedEmulationLayer = layers[Layouts.getLayerId(emulationMask)];
    var shiftedEmulationLayer = layers[Layouts.getLayerId(ModifierKeyConstants.K_SHIFTFLAG | emulationMask)];

    // buildDefaultLayout ensures that these are aliased to the original modifier set being emulated.
    // As a result, we can directly test for reference equality.
    //
    // This allows us to still return `true` after creating the layers for emulation; during keyboard
    // construction, the two layers should be null for AltGr emulation to succeed.
    if(unshiftedEmulationLayer != null &&
        unshiftedEmulationLayer != layers[Layouts.getLayerId(ModifierKeyConstants.RALTFLAG)]) {
      return false;
    }

    if(shiftedEmulationLayer != null &&
        shiftedEmulationLayer != layers[Layouts.getLayerId(ModifierKeyConstants.RALTFLAG | ModifierKeyConstants.K_SHIFTFLAG)]) {
      return false;
    }

    // It's technically possible for the OSK to not specify anything while allowing chiral input.  A last-ditch catch:
    var bitmask = this.modifierBitmask;
    if((bitmask & emulationMask) != emulationMask) {
      // At least one of the emulation modifiers is never used by the keyboard!  We can confirm everything's safe.
      return true;
    }

    if(unshiftedEmulationLayer == null && shiftedEmulationLayer == null) {
      // We've run out of things to go on; we can't detect if chiral AltGr emulation is intended or not.
      // TODO:  handle this again!
      // if(!osk.altGrWarning) {
      //   console.warn("Could not detect if AltGr emulation is safe, but defaulting to active emulation!")
      //   // Avoid spamming the console with warnings on every call of the method.
      //   osk.altGrWarning = true;
      // }
      return true;
    }
    return true;
  }

  get usesSupplementaryPlaneChars(): boolean {
    let kbd = this.scriptObject;
    // I3319 - SMP extension, I3363 (Build 301)
    return kbd && ((kbd['KS'] && kbd['KS'] == 1) || kbd['KN'] == 'Hieroglyphic');
  }

  get version(): string {
    return this.scriptObject['KBVER'] || '';
  }

  usesDesktopLayoutOnDevice(device: DeviceSpec) {
    if(this.scriptObject['KVKL']) {
      // A custom mobile layout is defined... but are we using it?
      return device.formFactor == DeviceSpec.FormFactor.Desktop;
    } else {
      return true;
    }
  }

  /**
   * @param       {number}    _PCommand     event code (16,17,18) or 0
   * @param       {Object}    _PTarget      target element
   * @param       {number}    _PData        1 or 0
   * Notifies keyboard of keystroke or other event
   */
  notify(_PCommand: number, _PTarget: OutputTarget, _PData: number) { // I2187
    // Good example use case - the Japanese CJK-picker keyboard
    if(typeof(this.scriptObject['KNS']) == 'function') {
      this.scriptObject['KNS'](_PCommand, _PTarget, _PData);
    }
  }

  private findOrConstructLayout(formFactor: DeviceSpec.FormFactor): TouchLayoutSpec {
    if(this._layouts) {
      // Search for viable layouts.  `null` is allowed for desktop form factors when help text is available,
      // so we check explicitly against `undefined`.
      if(this._layouts[formFactor] !== undefined) {
        return this._layouts[formFactor];
      } else if(formFactor == DeviceSpec.FormFactor.Phone && this._layouts[DeviceSpec.FormFactor.Tablet]) {
        return this._layouts[DeviceSpec.FormFactor.Phone] = this._layouts[DeviceSpec.FormFactor.Tablet];
      } else if(formFactor == DeviceSpec.FormFactor.Tablet && this._layouts[DeviceSpec.FormFactor.Phone]) {
        return this._layouts[DeviceSpec.FormFactor.Tablet] = this._layouts[DeviceSpec.FormFactor.Phone];
      }
    }

    // No pre-built layout available; time to start constructing it via defaults.
    // First, if we have non-default keys specified by the ['BK'] array, we've got
    // enough to work with to build a default layout.
    let rawSpecifications: any = null;  // TODO:  better typing, same type as this._legacyLayoutSpec.
    if(this._legacyLayoutSpec != null && this._legacyLayoutSpec['KLS']) { // KLS is only specified whenever there are non-default keys.
      rawSpecifications = this._legacyLayoutSpec;
    } else if(this._legacyLayoutSpec != null && this._legacyLayoutSpec['BK'] != null) {
      var keyCaps=this._legacyLayoutSpec['BK'];
      for(var i=0; i<keyCaps.length; i++) {
        if(keyCaps[i].length > 0) {
          rawSpecifications = this._legacyLayoutSpec;
          break;
        }
      }
    }

    // If we don't have key definitions to use for a layout but also lack help text or are a touch-based layout,
    // we make a default layout anyway.  We have to show display something usable.
    if(!rawSpecifications && (this.helpText == '' || formFactor != DeviceSpec.FormFactor.Desktop)) {
      rawSpecifications = {'F':'Tahoma', 'BK': Layouts.dfltText};
    }

    // Regardless of success, we'll want to initialize the field that backs the property;
    // may as well cache the default layout we just built, or a 'null' if it shouldn't exist..
    if(!this._layouts) {
      this._layouts = {};
    }

    // Final check - do we construct a layout, or is this a case where helpText / insertHelpHTML should take over?
    if(rawSpecifications) {
      // Now to generate a layout from our raw specifications.
      let layout: TouchLayoutSpec = this._layouts[formFactor] = Layouts.buildDefaultLayout(rawSpecifications, this, formFactor);
      layout.isDefault = true;
      return layout;
    } else {
      // The fact that it doesn't exist will indicate that help text/HTML should be inserted instead.
      this._layouts[formFactor] = null; // provides a cached value for the check at the top of this method.
      return null;
    }
  }

  /**
   * Returns an ActiveLayout object representing the keyboard's layout for this form factor.  May return null if a custom desktop "help" OSK is defined, as with sil_euro_latin.
   *
   * In such cases, please use either `helpText` or `insertHelpHTML` instead.
   * @param formFactor {string} The desired form factor for the layout.
   */
  public layout(formFactor: DeviceSpec.FormFactor): ActiveLayout {
    let rawLayout = this.findOrConstructLayout(formFactor);

    if(rawLayout) {
      // Prevents accidentally reprocessing layouts; it's a simple enough check.
      if(this.layoutStates[formFactor] == LayoutState.NOT_LOADED) {
        const layout = ActiveLayout.polyfill(rawLayout, this, formFactor);
        this.layoutStates[formFactor] = LayoutState.POLYFILLED;
        return layout;
      } else {
        return rawLayout as unknown as ActiveLayout;
      }

    } else {
      return null;
    }
  }

  public refreshLayouts() {
    let formFactors = [ DeviceSpec.FormFactor.Desktop, DeviceSpec.FormFactor.Phone, DeviceSpec.FormFactor.Tablet ];

    let _this = this;

    formFactors.forEach(function(form) {
      // Currently doesn't work if we reset it to POLYFILLED, likely due to how 'calibration'
      // currently works.
      _this.layoutStates[form] = LayoutState.NOT_LOADED;
    });
  }

  public markLayoutCalibrated(formFactor: DeviceSpec.FormFactor) {
    if(this.layoutStates[formFactor] != LayoutState.NOT_LOADED) {
      this.layoutStates[formFactor] = LayoutState.CALIBRATED;
    }
  }

  public getLayoutState(formFactor: DeviceSpec.FormFactor) {
    return this.layoutStates[formFactor];
  }


  constructNullKeyEvent(device: DeviceSpec, stateKeys?: StateKeyMap): KeyEvent {
    stateKeys = stateKeys || {
      K_CAPS: false,
      K_NUMLOCK: false,
      K_SCROLL: false
    }

    const keyEvent = KeyEvent.constructNullKeyEvent(device);
    this.setSyntheticEventDefaults(keyEvent, stateKeys);
    return keyEvent;
  }

  constructKeyEvent(key: ActiveKey | ActiveSubKey, device: DeviceSpec, stateKeys: StateKeyMap): KeyEvent {
    // Make a deep copy of our preconstructed key event, filling it out from there.
    const Lkc = key.baseKeyEvent;
    Lkc.device = device;

    if(this.isMnemonic) {
      Lkc.setMnemonicCode(key.layer.indexOf('shift') != -1, stateKeys['K_CAPS']);
    }

    // Performs common pre-analysis for both 'native' and 'embedded' OSK key & subkey input events.
    // This part depends on the keyboard processor's active state.
    this.setSyntheticEventDefaults(Lkc, stateKeys);

    // If it's a state key modifier, trigger its effects as part of the
    // keystroke.
    const bitmap = {
      'K_CAPS': Codes.stateBitmasks.CAPS,
      'K_NUMLOCK': Codes.stateBitmasks.NUM_LOCK,
      'K_SCROLL': Codes.stateBitmasks.SCROLL_LOCK
    };
    const bitmask = bitmap[Lkc.kName as keyof typeof bitmap];

    if(bitmask) {
      Lkc.Lstates ^= bitmask;
      Lkc.LmodifierChange = true;
    }

    return Lkc;
  }

  setSyntheticEventDefaults(Lkc: KeyEvent, stateKeys: StateKeyMap) {
    // Set the flags for the state keys - for desktop devices. For touch
    // devices, the only state key in use currently is Caps Lock, which is set
    // when the 'caps' layer is active in ActiveKey::constructBaseKeyEvent.
    if(!Lkc.device.touchable) {
      /*
       * For desktop-style keyboards, start from a blank slate.  They have a 'default'
       * (implicit 'NO_CAPS') layer but not a 'caps' layer.  With caps set, it just
       * highlights the key on the 'default' layer instead.
       *
       * We should never set both `CAPS` and `NO_CAPS` at the same time, and
       * same for the other modifiers.
       */
      Lkc.Lstates = 0;
      Lkc.Lstates |= stateKeys['K_CAPS']    ? ModifierKeyConstants.CAPITALFLAG : ModifierKeyConstants.NOTCAPITALFLAG;
      Lkc.Lstates |= stateKeys['K_NUMLOCK'] ? ModifierKeyConstants.NUMLOCKFLAG : ModifierKeyConstants.NOTNUMLOCKFLAG;
      Lkc.Lstates |= stateKeys['K_SCROLL']  ? ModifierKeyConstants.SCROLLFLAG : ModifierKeyConstants.NOTSCROLLFLAG;
    }

    // Set LisVirtualKey to false to ensure that nomatch rule does fire for U_xxxx keys
    if(Lkc.kName && Lkc.kName.substr(0,2) == 'U_') {
      Lkc.LisVirtualKey=false;
    }

    // Get code for non-physical keys (T_KOKAI, U_05AB etc)
    if(typeof Lkc.Lcode == 'undefined') {
      Lkc.Lcode = this.getVKDictionaryCode(Lkc.kName);// Updated for Build 347
      if(!Lkc.Lcode) {
        // Special case for U_xxxx keys. This vk code will never be used
        // in a keyboard, so we use this to ensure that keystroke processing
        // occurs for the key.
        Lkc.Lcode = 1;
      }
    }

    // Handles modifier states when the OSK is emulating rightalt through the leftctrl-leftalt layer.
    if((Lkc.Lmodifiers & Codes.modifierBitmasks['ALT_GR_SIM']) == Codes.modifierBitmasks['ALT_GR_SIM'] && this.emulatesAltGr) {
      Lkc.Lmodifiers &= ~Codes.modifierBitmasks['ALT_GR_SIM'];
      Lkc.Lmodifiers |= ModifierKeyConstants.RALTFLAG;
    }
  }

  /**
   * @summary Look up a custom virtual key code in the virtual key code dictionary KVKD.
   * On first run, will build the dictionary.
   *
   * `VKDictionary` is constructed from the keyboard's `KVKD` member. This list is constructed
   * at compile-time and is a list of 'additional' virtual key codes, starting at 256 (i.e.
   * outside the range of standard virtual key codes). These additional codes are both
   * `[T_xxx]` and `[U_xxxx]` custom key codes from the Keyman keyboard language. However,
   * `[U_xxxx]` keys only generate an entry in `KVKD` if there is a corresponding rule that
   * is associated with them in the keyboard rules. If the `[U_xxxx]` key code is only
   * referenced as the id of a key in the touch layout, then it does not get an entry in
   * the `KVKD` property.
   *
   * @private
   * @param       {string}      keyName   custom virtual key code to lookup in the dictionary
   * @return      {number}                key code > 255 on success, or 0 if not found
   */
  getVKDictionaryCode(keyName: string) {
    const dict = this.scriptObject['VKDictionary'] || {} as KeyboardObject['VKDictionary'];
    if(!this.scriptObject['VKDictionary']) {
      if(typeof this.scriptObject['KVKD'] == 'string') {
        // Build the VK dictionary
        // TODO: Move the dictionary build into the compiler -- so compiler generates code such as following.
        // Makes the VKDictionary member unnecessary.
        //       this.KVKD={"K_ABC":256,"K_DEF":257,...};
        const s=this.scriptObject['KVKD'].split(' ');
        for(var i=0; i<s.length; i++) {
          dict[s[i].toUpperCase()]=i+256; // We force upper-case since virtual keys should be case-insensitive.
        }
      }
      this.scriptObject['VKDictionary']=dict;
    }

    const res=dict[keyName.toUpperCase()];
    return res ? res : 0;
  }
}
