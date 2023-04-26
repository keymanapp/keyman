/// <reference path="preProcessor.ts" />

namespace com.keyman.dom {
  /*
  * Note that for many of the actual events represented by methods in this file, `this` is replaced
  * automatically by JavaScript's event handling system.  As such, many 'wrapper' variants of the events
  * exist to restore the object-oriented hierarchy below.
  *
  */

  export class CommonDOMStates {
    _Selection = null;
    _SelectionControl: any = null;   // Type behavior is as with activeElement and the like.

    changed: boolean;         // Tracks if the element has been edited since gaining focus.
    swallowKeypress: boolean; // Notes if a keypress should be swallowed; used when handing mnemonics.

    /* ----------------------- Static event-related methods ------------------------ */

  }

  /**
   * Declares a base, non-touch oriented implementation of all relevant DOM-related event handlers and state functions.
   */
  export class DOMEventHandlers {
    // TODO:  resolve/refactor out!
    protected keyman: KeymanBase;

    // This is only static within a given initialization of KeymanWeb.  Perhaps it would be best as an initialization
    // parameter and member field?
    static states: CommonDOMStates = new CommonDOMStates();

    constructor(keyman: KeymanBase) {
      this.keyman = keyman;
    }

    // End of I3363 (Build 301) additions
  }

  // -------------------------------------------------------------------------
}
