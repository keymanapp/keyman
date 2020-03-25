namespace com.keyman.keyboards {
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

    // TODO:  Provide public property-retrieving methods on this class, rather than as part of
    //        the KeyboardManager object.
  }
}