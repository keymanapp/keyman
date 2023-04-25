/// <reference path="preProcessor.ts" />

namespace com.keyman.dom {
  /*
  * Note that for many of the actual events represented by methods in this file, `this` is replaced
  * automatically by JavaScript's event handling system.  As such, many 'wrapper' variants of the events
  * exist to restore the object-oriented hierarchy below.
  *
  */

  export class CommonDOMStates {
    _DisableInput: boolean = false;         // Should input be disabled?
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

    // Universal DOM event handlers (both desktop + touch)

    /**
     * Function     _KeyDown
     * Scope        Private
     * Description  Processes keydown event and passes data to keyboard.
     *
     * Note that the test-case oriented 'recorder' stubs this method to facilitate keystroke
     * recording for use in test cases.  If changing this function, please ensure the recorder is
     * not affected.
     */
    _KeyDown: (e: KeyboardEvent) => boolean = function(this: DOMEventHandlers, e: KeyboardEvent): boolean {
      var activeKeyboard = this.keyman.core.activeKeyboard;
      var util = this.keyman.util;

      if(DOMEventHandlers.states._DisableInput || activeKeyboard == null) {
        return true;
      }

      // Prevent mapping element is readonly or tagged as kmw-disabled
      var el=util.eventTarget(e) as HTMLElement;
      if(util.device.touchable) {
        if(el && typeof el.kmwInput != 'undefined' && el.kmwInput == false) {
          return true;
        }
      } else if(el?.className?.indexOf('kmw-disabled') >= 0) {
        return true;
      }

      return PreProcessor.keyDown(e);
    }.bind(this);

    /**
     * Function     _KeyPress
     * Scope        Private
     * Description Processes keypress event (does not pass data to keyboard)
     */
    _KeyPress: (e: KeyboardEvent) => boolean = function(this: DOMEventHandlers, e: KeyboardEvent): boolean {
      if(DOMEventHandlers.states._DisableInput || this.keyman.core.activeKeyboard == null) {
        return true;
      }

      return PreProcessor.keyPress(e);
    }.bind(this);

    /**
     * Function     _KeyUp
     * Scope        Private
     * Description Processes keyup event and passes event data to keyboard
     */
    _KeyUp: (e: KeyboardEvent) => boolean = function(this: DOMEventHandlers, e: KeyboardEvent): boolean {
      var Levent = PreProcessor._GetKeyEventProperties(e, false);
      if(Levent == null) {
        return true;
      }

      let outputTarget = PreProcessor.getEventOutputTarget(e) as dom.targets.OutputTarget;
      var inputEle = outputTarget.getElement();

      // Since this part concerns DOM element + browser interaction management, we preprocess it for
      // browser form commands before passing control to the Processor module.
      if(Levent.Lcode == 13) {
        var ignore = false;
        if(outputTarget instanceof inputEle.ownerDocument.defaultView.HTMLTextAreaElement) {
          ignore = true;
        }

        if(inputEle.base && inputEle.base instanceof inputEle.base.ownerDocument.defaultView.HTMLTextAreaElement) {
          ignore = true;
        }

        if(!ignore) {
          // For input fields, move to next input element
          if(inputEle instanceof inputEle.ownerDocument.defaultView.HTMLInputElement) {
            if(inputEle.type == 'search' || inputEle.type == 'submit') {
              inputEle.form.submit();
            } else {
              this.keyman.domManager.moveToNext(false);
            }
          }
          return true;
        }
      }

      return PreProcessor.keyUp(e);
    }.bind(this);
  }

  // -------------------------------------------------------------------------
}
