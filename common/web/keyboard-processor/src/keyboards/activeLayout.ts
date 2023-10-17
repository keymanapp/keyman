import Codes from "../text/codes.js";
import KeyEvent, { KeyEventSpec } from "../text/keyEvent.js";
import KeyMapping from "../text/keyMapping.js";
import { Layouts } from "./defaultLayouts.js";
import type { LayoutKey, LayoutSubKey, LayoutRow, LayoutLayer, LayoutFormFactor, ButtonClass } from "./defaultLayouts.js";
import type Keyboard from "./keyboard.js";

import { TouchLayout } from "@keymanapp/common-types";
import TouchLayoutDefaultHint = TouchLayout.TouchLayoutDefaultHint;
import TouchLayoutFlick = TouchLayout.TouchLayoutFlick;
import { type DeviceSpec } from "@keymanapp/web-utils";

// TS 3.9 changed behavior of getters to make them
// non-enumerable by default. This broke our 'polyfill'
// functions which depended on enumeration to copy the
// relevant props over.
// https://github.com/microsoft/TypeScript/pull/32264#issuecomment-677718191
function Enumerable(
  target: unknown,
  propertyKey: string,
  descriptor: PropertyDescriptor
) {
    descriptor.enumerable = true;
};

/**
 * Designed for use by call-by-reference objects during keyboard-load preprocessing
 * to note properties of a keyboard that are only specified at lower levels of the
 * layout object.
 */
interface AnalysisMetadata {
  hasFlicks: boolean;
  hasMultitaps: boolean;
  hasLongpresses: boolean;
}

export class ActiveKeyBase {
  static readonly DEFAULT_PAD=15;          // Padding to left of key, in virtual units
  static readonly DEFAULT_RIGHT_MARGIN=15; // Padding to right of right-most key, in virtual units
  static readonly DEFAULT_KEY_WIDTH=100;   // Width of a key, if not specified, in virtual units

  // Defines key defaults
  static readonly DEFAULT_KEY = {
    text: '',
    width: ActiveKeyBase.DEFAULT_KEY_WIDTH,
    sp: 0,
    pad: ActiveKeyBase.DEFAULT_PAD
  };

  /** WARNING - DO NOT USE DIRECTLY outside of @keymanapp/keyboard-processor! */
  id: TouchLayout.TouchLayoutKeyId;

  // These are fine.
  width?: number;
  pad?: number;

  layer: string;
  displayLayer: string;
  nextlayer: string;
  sp?: ButtonClass;

  private _baseKeyEvent: KeyEvent;
  isMnemonic: boolean = false;

  /**
   * Only available on subkeys, but we don't distinguish between base keys and subkeys
   * at this level yet in KMW.
   */
  default?: boolean;

  proportionalPad: number;
  proportionalX: number;
  proportionalWidth: number;

  // While they're only valid on ActiveKey, spec'ing them here makes references more concise within the OSK.
  sk?: ActiveKey[];
  multitap?: ActiveSubKey[];
  flick?: TouchLayout.TouchLayoutFlick;

  // Keeping things simple here, as this was added LATE in 14.0 beta.
  // Could definitely extend in the future to instead return an object
  // that denotes the 'nature' of the key.
  // - isUnicode
  // - isHardwareKey
  // - etc.

  // Reference for the terminology in the comments below:
  // https://help.keyman.com/developer/current-version/guides/develop/creating-a-touch-keyboard-layout-for-amharic-the-nitty-gritty

  /**
   * Matches the key code as set within Keyman Developer for the layout.
   * For example, K_R or U_0020.  Denotes either physical keys or virtual keys with custom output,
   * with no additional metadata like layer or active modifiers.
   *
   * Is used to determine the keycode for input events, rule-matching, and keystroke processing.
   */
  @Enumerable
  public get baseKeyID(): string {
    if(typeof this.id === 'undefined') {
      return undefined;
    }

    return this.id;
  }

  @Enumerable
  public get isPadding(): boolean {
    // Does not include 9 (class:  blank) as that may be an intentional 'catch' for misplaced
    // keystrokes.
    return this.sp == Layouts.buttonClasses.HIDDEN; // Button class: hidden.
  }

  /**
   * A unique identifier based on both the key ID & the 'desktop layer' to be used for the key.
   *
   * Allows diambiguation of scenarios where the same key ID is used twice within a layer, but
   * with different innate modifiers.  (Refer to https://github.com/keymanapp/keyman/issues/4617)
   * The 'desktop layer' may be omitted if it matches the key's display layer.
   *
   * Examples, given a 'default' display layer, matching keys to Keyman keyboard language:
   *
   * ```
   * "K_Q"
   * + [K_Q]
   * "K_Q+shift"
   * + [K_Q SHIFT]
   * ```
   *
   * Useful when the active layer of an input-event is already known.
   */
  @Enumerable
  public get coreID(): string {
    if(typeof this.id === 'undefined') {
      return undefined;
    }

    let baseID = this.id || '';

    if(this.displayLayer != this.layer) {
      baseID = baseID + '+' + this.layer;
    }

    return baseID;
  }

  /**
   * A keyboard-unique identifier to be used for any display elements representing this key
   * in user interfaces and/or on-screen keyboards.
   *
   * Distinguishes between otherwise-identical keys on different layers of an OSK.
   * Includes identifying information about the key's display layer.
   *
   * Examples, given a 'default' display layer, matching keys to Keyman keyboard language:
   *
   * ```
   * "default-K_Q"
   * + [K_Q]
   * "default-K_Q+shift"
   * + [K_Q SHIFT]
   * ```
   *
   * Useful when only the active keyboard is known about an input event.
   */
  @Enumerable
  public get elementID(): string {
    if(typeof this.id === 'undefined') {
      return undefined;
    }

    return this.displayLayer + '-' + this.coreID;
  }

  @Enumerable
  public get baseKeyEvent(): KeyEvent {
    return new KeyEvent(this._baseKeyEvent);
  }

  /**
   * Converts key IDs of the U_* form to their corresponding UTF-16 text.
   * If an ID not matching the pattern is received, returns null.
   * @param id
   * @returns
   */
  static unicodeIDToText(id: string, errorCallback?: (codeAsString: string) => void) {
    if(!id || id.substring(0,2) != 'U_') {
      return null;
    }

    let result = '';
    const codePoints = id.substring(2).split('_');
    for(let codePoint of codePoints) {
      const codePointValue = parseInt(codePoint, 16);
      if (((0x0 <= codePointValue) && (codePointValue <= 0x1F)) ||
          ((0x80 <= codePointValue) && (codePointValue <= 0x9F)) ||
          isNaN(codePointValue)) {
        if(errorCallback) {
          errorCallback(codePoint);
        }
        continue;
      } else {
        // String.fromCharCode() is inadequate to handle the entire range of Unicode
        // Someday after upgrading to ES2015, can use String.fromCodePoint()
        result += String.kmwFromCharCode(codePointValue);
      }
    }
    return result ? result : null;
  }

  static sanitize(rawKey: LayoutKey) {
    if(typeof rawKey.width == 'string') {
      rawKey.width = parseInt(rawKey.width, 10);
    }
    // Handles NaN cases as well as 'set to 0' cases; both are intentional here.
    rawKey.width ||= ActiveKey.DEFAULT_KEY_WIDTH;

    if(typeof rawKey.pad == 'string') {
      rawKey.pad = parseInt(rawKey.pad, 10);
    }
    rawKey.pad ||= ActiveKey.DEFAULT_PAD;

    if(typeof rawKey.sp == 'string') {
      rawKey.sp = Number.parseInt(rawKey.sp, 10) as ButtonClass;
    }
    rawKey.sp ||= 0; // The default button class.
  }

  static polyfill(key: LayoutKey, keyboard: Keyboard, layout: ActiveLayout, displayLayer: string, analysisFlagObj?: AnalysisMetadata) {
    analysisFlagObj ||= {
      hasFlicks: false,
      hasLongpresses: false,
      hasMultitaps: false
    }

    // Add class functions to the existing layout object, allowing it to act as an ActiveLayout.
    let dummy = new ActiveKeyBase();
    let proto = Object.getPrototypeOf(dummy);

    for(let prop in dummy) {
      if(!key.hasOwnProperty(prop)) {
        let descriptor = Object.getOwnPropertyDescriptor(proto, prop);
        if(descriptor) {
          // It's a computed property!  Copy the descriptor onto the key's object.
          Object.defineProperty(key, prop, descriptor);
        } else {
          key[prop] = dummy[prop];
        }
      }
    }

    // Ensure subkeys are also properly extended.
    if(key.sk) {
      analysisFlagObj.hasLongpresses = true;
      for(let subkey of key.sk) {
        ActiveSubKey.polyfill(subkey, keyboard, layout, displayLayer, analysisFlagObj);
      }
    }

    // Also multitap keys.
    if(key.multitap) {
      analysisFlagObj.hasMultitaps = true;
      for(let mtKey of key.multitap) {
        ActiveSubKey.polyfill(mtKey, keyboard, layout, displayLayer, analysisFlagObj);
      }
    }


    if(key.flick) {
      analysisFlagObj.hasFlicks = true;
      for(let flickKey in key.flick) {
        ActiveSubKey.polyfill(key.flick[flickKey as keyof TouchLayoutFlick], keyboard, layout, displayLayer, analysisFlagObj);
      }
    }

    let aKey = key as ActiveKey;
    aKey.displayLayer = displayLayer;
    aKey.layer = aKey.layer || displayLayer;

    // Compute the key's base KeyEvent properties for use in future event generation
    aKey.constructBaseKeyEvent(keyboard, layout, displayLayer);
  }

  private constructBaseKeyEvent(keyboard: Keyboard, layout: ActiveLayout, displayLayer: string) {
    // Get key name and keyboard shift state (needed only for default layouts and physical keyboard handling)
    // Note - virtual keys should be treated case-insensitive, so we force uppercasing here.
    let layer = this.layer || displayLayer || '';
    let keyName= this.id ? this.id.toUpperCase() : null;

    // Start:  mirrors _GetKeyEventProperties

    // First check the virtual key, and process shift, control, alt or function keys
    let props: KeyEventSpec = {
      // Override key shift state if specified for key in layout (corrected for popup keys KMEW-93)
      Lmodifiers: Codes.getModifierState(layer),
      Lstates: Codes.getStateFromLayer(layer),
      Lcode: keyName ? Codes.keyCodes[keyName] : 0,
      LisVirtualKey: true,
      vkCode: 0,
      kName: keyName,
      kLayer: layer,
      kbdLayer: displayLayer,
      kNextLayer: this.nextlayer,
      device: null,
      isSynthetic: true
    };

    let Lkc: KeyEvent = new KeyEvent(props);

    if(layout.keyboard) {
      let keyboard = layout.keyboard;

      // Include *limited* support for mnemonic keyboards (Sept 2012)
      // If a touch layout has been defined for a mnemonic keyout, do not perform mnemonic mapping for rules on touch devices.
      if(keyboard.isMnemonic && !(layout.isDefault && layout.formFactor != 'desktop')) {
        if(Lkc.Lcode != Codes.keyCodes['K_SPACE']) { // exception required, March 2013
          // Jan 2019 - interesting that 'K_SPACE' also affects the caps-state check...
          Lkc.vkCode = Lkc.Lcode;
          this.isMnemonic = true;
        }
      } else {
        Lkc.vkCode=Lkc.Lcode;
      }

      // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
      if(!keyboard.definesPositionalOrMnemonic) {
        // Not the best pattern, but currently safe - we don't look up any properties of any of the
        // arguments in this use case, and the object's scope is extremely limited.
        Lkc.Lcode = KeyMapping._USKeyCodeToCharCode(keyboard.constructKeyEvent(null, null, {
          K_CAPS: false,
          K_NUMLOCK: false,
          K_SCROLL: false
        }));
        Lkc.LisVirtualKey=false;
      }
    }

    this._baseKeyEvent = Lkc;
  }
}


export class ActiveKey extends ActiveKeyBase implements LayoutKey {
  public getSubkey(coreID: string): ActiveKey {
    if(this.sk) {
      for(let key of this.sk) {
        if(key.coreID == coreID) {
          return key;
        }
      }
    }

    return null;
  }
}


export class ActiveSubKey extends ActiveKeyBase implements LayoutSubKey {

}

export class ActiveRow implements LayoutRow {
  // Identify key labels (e.g. *Shift*) that require the special OSK font
  static readonly SPECIAL_LABEL=/\*\w+\*/;

  id: number;
  key: ActiveKey[];

  /**
   * Used for calculating fat-fingering offsets.
   */
  proportionalY: number;

  private constructor() {

  }

  static sanitize(rawRow: LayoutRow) {
    for(const key of rawRow.key) {
      // Test for a trailing comma included in spec, added as null object by IE
      // It has only ever appeared at the end of a row's spec.
      if(key == null) {
        rawRow.key.length = rawRow.key.length-1;
      } else {
        ActiveKey.sanitize(key);
      }
    }

    if(typeof rawRow.id == 'string') {
      rawRow.id = Number.parseInt(rawRow.id, 10);
    }
  }

  static polyfill(
    row: LayoutRow,
    keyboard: Keyboard,
    layout: ActiveLayout,
    displayLayer: string,
    totalWidth: number,
    proportionalY: number,
    analysisFlagObj: AnalysisMetadata
  ) {
    // Apply defaults, setting the width and other undefined properties for each key
    let keys=row['key'];
    for(let j=0; j<keys.length; j++) {
      let key=keys[j];
      for(var tp in ActiveKey.DEFAULT_KEY) {
        if(typeof key[tp] != 'string' && typeof key[tp] != 'number') {
          key[tp]=ActiveKey.DEFAULT_KEY[tp];
        }
      }

      // Modify the key type for special keys with non-standard labels
      // to allow the keyboard font to ovveride the SpecialOSK font.
      // Blank keys are no longer reclassed - can use before/after CSS to add text
      switch(key['sp']) {
        case Layouts.buttonClasses['SHIFT']:
          if(!ActiveRow.SPECIAL_LABEL.test(key['text']) && key['text'] != '') {
            key.sp=Layouts.buttonClasses['SPECIAL'];
          }
          break;
        case Layouts.buttonClasses['SHIFT-ON']:
          if(!ActiveRow.SPECIAL_LABEL.test(key['text']) && key['text'] != '') {
            key.sp=Layouts.buttonClasses['SPECIAL-ON'];
          }
          break;
      }

      ActiveKey.polyfill(key, keyboard, layout, displayLayer, analysisFlagObj);
    }

    /* The calculations here are effectively 'virtualized'.  When used with the OSK, the VisualKeyboard
      * will overwrite these values with their true runtime geometry.
      *
      * These calculations approximate those of the actual OSK (without fitting to a specific resolution)
      * and are intended for use with layout testing (while headless) in the future.
      */

    let setProportions = function(key: ActiveKey, padPc: number, keyPc: number, totalPc: number) {
      key.proportionalPad   = padPc;
      key.proportionalWidth = keyPc;
      key.proportionalX     = (totalPc + padPc + (keyPc/2));
    }

    // Calculate percentage-based scalings by summing defined widths and scaling each key to %.
    // Save each percentage key width as a separate member (do *not* overwrite layout specified width!)
    let totalPercent=0;
    for(let j=0; j<keys.length-1; j++) {
      const key = keys[j] as ActiveKey; // already 'polyfilled' in prior loop

      // compute center's default x-coord (used in headless modes), assign 'proportional' props
      setProportions(key, key.pad/totalWidth, key.width/totalWidth, totalPercent);

      // These values are set on the key as part of the prior call.
      totalPercent += key.proportionalPad;
      totalPercent += key.proportionalWidth;
    }

    // Allow for right OSK margin (15 layout units)
    let rightMargin = ActiveKey.DEFAULT_RIGHT_MARGIN/totalWidth;

    if(keys.length > 0) {
      const finalKey = keys[keys.length-1] as ActiveKey;

      // If a single key, and padding is negative, add padding to right align the key
      if(keys.length == 1 && finalKey.pad < 0) {
        const keyPercent = finalKey.width/totalWidth;
        const padPercent = 1-(totalPercent + keyPercent + rightMargin);

        // compute center's default x-coord (used in headless modes)
        setProportions(finalKey, padPercent, keyPercent, totalPercent);
      } else {
        const padPercent = finalKey.pad/totalWidth;
        const keyPercent = 1-(totalPercent + padPercent + rightMargin);

        // compute center's default x-coord (used in headless modes)
        setProportions(finalKey, padPercent, keyPercent, totalPercent);
      }
    }

    // Add class functions to the existing layout object, allowing it to act as an ActiveLayout.
    let dummy = new ActiveRow();
    for(let key in dummy) {
      if(!row.hasOwnProperty(key)) {
        row[key] = dummy[key];
      }
    }

    let aRow = row as ActiveRow;
    aRow.proportionalY = proportionalY;
  }

  populateKeyMap(map: {[keyId: string]: ActiveKey}) {
    this.key.forEach(function(key: ActiveKey) {
      if(key.coreID) {
        map[key.coreID] = key;
      }
    });
  }
}

export class ActiveLayer implements LayoutLayer {
  row: ActiveRow[];
  id: string;

  // These already exist on the objects, pre-polyfill...
  // but they still need to be proactively declared on this type.
  capsKey?: ActiveKey;
  numKey?: ActiveKey;
  scrollKey?: ActiveKey;

  totalWidth: number;

  defaultKeyProportionalWidth: number;
  rowProportionalHeight: number;

  /**
   * Facilitates mapping key id strings to their specification objects.
   */
  keyMap: {[keyId: string]: ActiveKey};

  constructor() {

  }

  static sanitize(rawLayer: LayoutLayer) {
    for(const row of rawLayer.row) {
      ActiveRow.sanitize(row);
    }
  }

  static polyfill(layer: LayoutLayer, keyboard: Keyboard, layout: ActiveLayout, analysisFlagObj: AnalysisMetadata) {
    layer.aligned=false;

    // Create a DIV for each row of the group
    let rows=layer['row'];

    // Calculate the maximum row width (in layout units)
    let totalWidth=0;
    for(const row of rows) {
      let width=0;
      const keys=row['key'];

      for(const key of keys) {
        // So long as `sanitize` is called first, these coercions are safe.
        width += (key.width as number) + (key.pad as number);
      }

      if(width > totalWidth) {
        totalWidth = width;
      }
    }

    // Add default right margin
    if(layout.formFactor == 'desktop') {
      totalWidth += 5; // TODO: resolve difference between touch and desktop; why don't we use ActiveKey.DEFAULT_RIGHT_MARGIN?
    } else {
      totalWidth += ActiveKey.DEFAULT_RIGHT_MARGIN;
    }

    let rowCount = layer.row.length;
    for(let i=0; i<rowCount; i++) {
      // Calculate proportional y-coord of row.  0 is at top with highest y-coord.
      let rowProportionalY = (i + 0.5) / rowCount;
      ActiveRow.polyfill(layer.row[i], keyboard, layout, layer.id, totalWidth, rowProportionalY, analysisFlagObj);
    }

    // Add class functions and properties to the existing layout object, allowing it to act as an ActiveLayout.
    let dummy = new ActiveLayer();
    for(let key in dummy) {
      if(!layer.hasOwnProperty(key)) {
        layer[key] = dummy[key];
      }
    }

    let aLayer = layer as ActiveLayer;
    aLayer.totalWidth = totalWidth;
    aLayer.defaultKeyProportionalWidth = ActiveKey.DEFAULT_KEY.width / totalWidth;
    aLayer.rowProportionalHeight = 1.0 / rowCount;
    aLayer.keyMap = aLayer.constructKeyMap();
  }

  private constructKeyMap(): {[keyId: string]: ActiveKey} {
    let map: {[keyId: string]: ActiveKey} = {};
    this.row.forEach(function(row: ActiveRow) {
      row.populateKeyMap(map);
    });

    return map;
  }

  getKey(keyId: string) {
    // Keys usually are specified in a "long form" prefixed with their layer's ID.
    if(keyId.indexOf(this.id + '-') == 0) {
      keyId = keyId.replace(this.id + '-', '');
    }

    let idComponents = keyId.split('::');
    if(idComponents.length > 1) {
      let baseKey = this.keyMap[idComponents[0]];
      return baseKey.getSubkey(idComponents[1]);
    } else {
      return this.keyMap[keyId];
    }
  }
}

export class ActiveLayout implements LayoutFormFactor{
  layer: ActiveLayer[];
  font: string;
  keyLabels: boolean;
  isDefault?: boolean;
  keyboard: Keyboard;
  formFactor: DeviceSpec.FormFactor;
  defaultHint: TouchLayoutDefaultHint;

  hasFlicks: boolean = false;
  hasLongpresses: boolean = false;
  hasMultitaps: boolean = false;

  /**
   * Facilitates mapping layer id strings to their specification objects.
   */
  layerMap: {[layerId: string]: ActiveLayer};

  private constructor() {

  }

  getLayer(layerId: string): ActiveLayer {
    return this.layerMap[layerId];
  }

  /**
   * Refer to https://github.com/keymanapp/keyman/issues/254, which mentions
   * KD-11 from a prior issue-tracking system from the closed-source days that
   * resulted in an unintended extra empty row.
   *
   * It'll be pretty rare to see a keyboard affected by the bug, but we don't
   * 100% control all keyboards out there, so it's best we make sure the edge
   * case is covered.
   *
   * @param layers The layer group to be loaded for the form factor.  Will be
   *               mutated by this operation.
   */
  static correctLayerEmptyRowBug(layers: LayoutLayer[]) {
    for(let n=0; n<layers.length; n++) {
      let layer=layers[n];
      let rows=layer['row'];
      let i: number;
      for(i=rows.length-1; i>=0; i--) {
        if(!Array.isArray(rows[i]['key']) || rows[i]['key'].length == 0) {
          rows.splice(i, 1)
        }
      }
    }
  }

  static sanitize(rawLayout: LayoutFormFactor) {
    ActiveLayout.correctLayerEmptyRowBug(rawLayout.layer);

    for(const layer of rawLayout.layer) {
      ActiveLayer.sanitize(layer);
    }
  }

  /**
   *
   * @param layout
   * @param formFactor
   */
  static polyfill(layout: LayoutFormFactor, keyboard: Keyboard, formFactor: DeviceSpec.FormFactor): ActiveLayout {
    if(layout == null) {
      throw new Error("Cannot build an ActiveLayout for a null specification.");
    }

    const analysisMetadata: AnalysisMetadata = {
      hasFlicks: false,
      hasLongpresses: false,
      hasMultitaps: false
    };

    /* Standardize the layout object's data types.
      *
      * In older versions of KMW, some numeric properties were long represented as strings instead,
      * and that lives on within a _lot_ of keyboards.  The data should be sanitized before it
      * is processed by this method.
      */
    this.sanitize(layout);

    // Create a separate OSK div for each OSK layer, only one of which will ever be visible
    var n: number;
    let layerMap: {[layerId: string]: ActiveLayer} = {};

    let layers=layout.layer;

    // Add class functions to the existing layout object, allowing it to act as an ActiveLayout.
    let dummy = new ActiveLayout();
    for(let key in dummy) {
      if(!layout.hasOwnProperty(key)) {
        layout[key] = dummy[key];
      }
    }

    let aLayout = layout as ActiveLayout;
    aLayout.keyboard = keyboard;
    aLayout.formFactor = formFactor;

    for(n=0; n<layers.length; n++) {
      ActiveLayer.polyfill(layers[n], keyboard, aLayout, analysisMetadata);
      layerMap[layers[n].id] = layers[n] as ActiveLayer;
    }

    aLayout.hasFlicks = analysisMetadata.hasFlicks;
    aLayout.hasLongpresses = analysisMetadata.hasLongpresses;
    aLayout.hasMultitaps = analysisMetadata.hasMultitaps;

    aLayout.layerMap = layerMap;

    return aLayout;
  }
}
