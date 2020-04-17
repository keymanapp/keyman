/// <reference path="defaultLayouts.ts" />
/// <reference path="activeLayout.ts" />
/// <reference path="../text/kbdInterface.ts" />

namespace com.keyman.keyboards {
  /**
   * Stores preprocessed properties of a keyboard for quick retrieval later.
   */
  class CacheTag {
    stores: {[storeName: string]: text.ComplexKeyboardStore};

    constructor() {
      this.stores = {};
    }
  }

  export enum LayoutState {
    NOT_LOADED = undefined,
    POLYFILLED = 1,
    CALIBRATED = 2
  }

  /**
   * Acts as a wrapper class for Keyman keyboards compiled to JS, providing type information
   * and keyboard-centered functionality in an object-oriented way without modifying the 
   * wrapped keyboard itself.
   */
  export class Keyboard {
    public static DEFAULT_SCRIPT_OBJECT = {
      'gs': function(outputTarget, keystroke) { return false; }, // no matching rules; rely on defaultRuleOutput entirely
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
    public readonly scriptObject: any;
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
     * Calls the keyboard's `gs` function, which represents the keyboard source's group(main).
     */
    process(outputTarget: text.OutputTarget, keystroke: text.KeyEvent): boolean {
      return this.scriptObject['gs'](outputTarget, keystroke);
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

    get displaysUnderlyingKeys(): boolean {
      // Returns false if undefined or false-like (including 0), true otherwise.
      return !!this.scriptObject['KDU'];
    }

    // TODO:  Better typing.
    private get _legacyLayoutSpec(): any {
      return this.scriptObject['KV'];  // used with buildDefaultLayout; layout must be constructed at runtime.
    }

    // May return null if no layouts exist or have been initialized.
    private get _layouts(): {[formFactor: string]: LayoutFormFactor} {
      return this.scriptObject['KVKL'];  // This one is compiled by Developer's visual keyboard layout editor.
    }

    private set _layouts(value) {
      this.scriptObject['KVKL'] = value;
    }

    get compilerVersion(): utils.Version {
      return new utils.Version(this.scriptObject['KVER']);
    }

    get isMnemonic(): boolean {
      return !!this.scriptObject['KM'];
    }

    get definesPositionalOrMnemonic(): boolean {
      return typeof this.scriptObject['KM'] != 'undefined';
    }

    /**
     * HTML help text which is a one liner intended for the status bar of the desktop OSK originally.
     * 
     * Reference: https://help.keyman.com/developer/language/reference/kmw_helptext
     */
    get helpText(): string {
      return this.scriptObject['KH'];
    }

    get hasHelpHTML(): boolean {
      return !!this.scriptObject['KHF'];
    }

    /**
     * Replaces the OSK with custom HTML, which may be interactive (like with sil_euro_latin).
     * 
     * Reference: https://help.keyman.com/developer/language/reference/kmw_helpfile
     */
    insertHelpHTML(e: any) {
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
      return this.scriptObject['KMBM'] || text.Codes.modifierBitmasks['NON_CHIRAL'];
    }

    get isChiral(): boolean {
      return !!(this.modifierBitmask & text.Codes.modifierBitmasks['IS_CHIRAL']);
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

    get explodedStores(): {[storeName: string]: text.ComplexKeyboardStore} {
      return this.cacheTag.stores;
    }

    /**
     * Signifies whether or not a layout or OSK should include AltGr / Right-alt emulation for this keyboard.
     * @param   {Object=}   keyLabels
     * @return  {boolean}
     */
    get emulatesAltGr(): boolean {
      let modifierCodes = text.Codes.modifierCodes;

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

      var emulationMask = modifierCodes['LCTRL'] | modifierCodes['LALT'];
      var unshiftedEmulationLayer = layers[Layouts.getLayerId(emulationMask)];
      var shiftedEmulationLayer = layers[Layouts.getLayerId(modifierCodes['SHIFT'] | emulationMask)];
      
      // buildDefaultLayout ensures that these are aliased to the original modifier set being emulated.
      // As a result, we can directly test for reference equality.
      //
      // This allows us to still return `true` after creating the layers for emulation; during keyboard
      // construction, the two layers should be null for AltGr emulation to succeed.
      if(unshiftedEmulationLayer != null && 
          unshiftedEmulationLayer != layers[Layouts.getLayerId(modifierCodes['RALT'])]) {
        return false;
      }

      if(shiftedEmulationLayer != null && 
          shiftedEmulationLayer != layers[Layouts.getLayerId(modifierCodes['RALT'] | modifierCodes['SHIFT'])]) {
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

    usesDesktopLayoutOnDevice(device: text.EngineDeviceSpec) {
      if(this.scriptObject['KVKL']) {
        // A custom mobile layout is defined... but are we using it?
        return device.formFactor == text.FormFactor.Desktop;
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
    notify(_PCommand: number, _PTarget: text.OutputTarget, _PData: number) { // I2187
      // Good example use case - the Japanese CJK-picker keyboard
      if(typeof(this.scriptObject['KNS']) == 'function') {
        this.scriptObject['KNS'](_PCommand, _PTarget, _PData);
      }
    }

    private findOrConstructLayout(formFactor: text.FormFactor): LayoutFormFactor {
      if(this._layouts) {
        // Search for viable layouts.  `null` is allowed for desktop form factors when help text is available,
        // so we check explicitly against `undefined`.
        if(this._layouts[formFactor] !== undefined) {
          return this._layouts[formFactor];
        } else if(formFactor == text.FormFactor.Phone && this._layouts[text.FormFactor.Tablet]) {
          return this._layouts[text.FormFactor.Phone] = this._layouts[text.FormFactor.Tablet];
        } else if(formFactor == text.FormFactor.Tablet && this._layouts[text.FormFactor.Phone]) {
          return this._layouts[text.FormFactor.Tablet] = this._layouts[text.FormFactor.Phone];
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
      if(!rawSpecifications && (this.helpText == '' || formFactor != text.FormFactor.Desktop)) {
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
        let layout = this._layouts[formFactor] = Layouts.buildDefaultLayout(rawSpecifications, this, formFactor);
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
    public layout(formFactor: text.FormFactor): ActiveLayout {
      let rawLayout = this.findOrConstructLayout(formFactor);

      if(rawLayout) {
        // Prevents accidentally reprocessing layouts; it's a simple enough check.
        if(this.layoutStates[formFactor] == LayoutState.NOT_LOADED) {
          rawLayout = ActiveLayout.polyfill(rawLayout, this, formFactor);
          this.layoutStates[formFactor] = LayoutState.POLYFILLED;
        }

        return rawLayout as ActiveLayout;
      } else {
        return null;
      }
    }

    public markLayoutCalibrated(formFactor: text.FormFactor) {
      if(this.layoutStates[formFactor] != LayoutState.NOT_LOADED) {
        this.layoutStates[formFactor] = LayoutState.CALIBRATED;
      }
    }

    public getLayoutState(formFactor: text.FormFactor) {
      return this.layoutStates[formFactor];
    }
  }
}