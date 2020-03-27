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

  /**
   * Acts as a wrapper class for Keyman keyboards compiled to JS, providing type information
   * and keyboard-centered functionality in an object-oriented way without modifying the 
   * wrapped keyboard itself.
   */
  export class Keyboard {
    /**
     * This is the object provided to KeyboardInterface.registerKeyboard - that is, the keyboard
     * being wrapped.
     * 
     * TODO:  Make this private instead.  But there are a LOT of references that must be rooted out first.
     */
    public readonly scriptObject: any;

    constructor(keyboardScript: any) {
      this.scriptObject = keyboardScript;
    }

    /**
     * Calls the keyboard's `gs` function, which represents the keyboard source's group(main).
     */
    process(outputTarget: text.OutputTarget, keystroke: text.KeyEvent): boolean {
      return this.scriptObject['gs'](outputTarget, keystroke);
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
    get legacyLayoutSpec(): any {
      return this.scriptObject['KV'];  // used with buildDefaultLayout; layout must be constructed at runtime.
    }

    // TODO:  Better typing.
    // May return null.
    get layouts(): any {
      return this.scriptObject['KVKL'];  // This one is compiled by Developer's visual keyboard layout editor.
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
  }
}