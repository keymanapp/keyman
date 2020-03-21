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

    get oskStyling(): string {
      return this.scriptObject['KCSS'];
    }

    // TODO:  Provide public property-retrieving methods on this class, rather than as part of
    //        the KeyboardManager object.
  }
}