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
    _IgnoreNextSelChange: number = 0;       // when a visual keyboard key is mouse-down, ignore the next sel change because this stuffs up our history
    _IgnoreBlurFocus: boolean = false;      // Used to temporarily ignore focus changes
    _Selection = null;
    _SelectionControl: any = null;   // Type behavior is as with activeElement and the like.

    _activeElement: HTMLElement;
    _lastActiveElement: HTMLElement;

    focusing: boolean;
    focusTimer: number;

    changed: boolean;         // Tracks if the element has been edited since gaining focus.
    swallowKeypress: boolean; // Notes if a keypress should be swallowed; used when handing mnemonics.

    /* ----------------------- Static event-related methods ------------------------ */

    setFocusTimer(): void {
      this.focusing=true;

      this.focusTimer = window.setTimeout(function() {
        this.focusing=false;
      }.bind(this), 50)
    }
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

    //TODO: add more complete description of what ControlFocus really does
    /**
     * Respond to KeymanWeb-aware input element receiving focus
     */
    _ControlFocus: (e: FocusEvent) => boolean = function(this: DOMEventHandlers, e: FocusEvent): boolean {
      var Ltarg: HTMLElement;

      Ltarg = this.keyman.util.eventTarget(e) as HTMLElement;
      if (Ltarg == null) {
        return true;
      }

      if(Ltarg['body']) {
        Ltarg = Ltarg['body']; // Occurs in Firefox for design-mode iframes.
      }

      // Or if not a remappable input field
      if(Ltarg.ownerDocument && Ltarg instanceof Ltarg.ownerDocument.defaultView.HTMLInputElement) {
        var et=Ltarg.type.toLowerCase();
        if(!(et == 'text' || et == 'search')) {
          return true;
        }
      }

      // We condition on 'priorElement' below as a check to allow KMW to set a default active keyboard.
      var priorElement = DOMEventHandlers.states._lastActiveElement;

      if (Ltarg.nodeType == 3) { // defeat Safari bug
        Ltarg = Ltarg.parentNode as HTMLElement;
      }

      var LfocusTarg = Ltarg;

      if(Ltarg.ownerDocument && Ltarg instanceof Ltarg.ownerDocument.defaultView.HTMLIFrameElement) { //**TODO: check case reference
        this.keyman.domManager._AttachToIframe(Ltarg as HTMLIFrameElement);
        Ltarg=Ltarg.contentWindow.document.body;
      }

      // Must set before _Blur / _Focus to avoid infinite recursion due to complications
      // in setActiveKeyboard behavior with managed keyboard settings.
      this.keyman.domManager.lastActiveElement = Ltarg;
      this.keyman.domManager.activeElement = Ltarg;  // I3363 (Build 301)

      if(focusAssistant.restoringFocus) {
        this._BlurKeyboardSettings(Ltarg);
      } else {
        this._FocusKeyboardSettings(Ltarg, priorElement ? false : true);
      }

      // Always do the common focus stuff, instantly returning if we're in an editable iframe.
      if(this._CommonFocusHelper(Ltarg)) {
        return true;
      };

      // Set element directionality (but only if element is empty)
      if(Ltarg.ownerDocument && Ltarg instanceof Ltarg.ownerDocument.defaultView.HTMLElement) {
        this.keyman.domManager._SetTargDir(Ltarg);
      }

      //Execute external (UI) code needed on focus if required
      this.doControlFocused(LfocusTarg, this.keyman.domManager.lastActiveElement);

      return true;
    }.bind(this);

    /**
     * Function     doControlFocused
     * Scope        Private
     * @param       {Object}            _target         element gaining focus
     * @param       {Object}            _activeControl  currently active control
     * @return      {boolean}
     * Description  Execute external (UI) code needed on focus
     */
    doControlFocused(_target: HTMLElement, _activeControl: HTMLElement): boolean {
      var p={};
      p['target']=_target;
      p['activeControl']=_activeControl;

      return this.keyman.util.callEvent('kmw.controlfocused',p);
    }

    /**
     * Respond to KMW losing focus on event
     */
    _ControlBlur: (e: FocusEvent) => boolean = function(this: DOMEventHandlers, e: FocusEvent): boolean {
      let Ltarg = this.keyman.util.eventTarget(e) as HTMLElement;
      if (Ltarg == null) {
        return true;
      }

      if(Ltarg['body']) {
        Ltarg = Ltarg['body']; // Occurs in Firefox for design-mode iframes.
      }

      if(DOMEventHandlers.states._IgnoreNextSelChange) {
        // If a keyboard calls saveFocus() (KSF), then ignore the
        // next selection change
        DOMEventHandlers.states._IgnoreNextSelChange--;
        e.cancelBubble = true;
        e.stopPropagation();
        return true;
      }

      if(DOMEventHandlers.states._IgnoreBlurFocus) {
        // Prevent triggering other blur-handling events (as possible)
        e.cancelBubble = true;
        e.stopPropagation();
        return true;
      }

      if (Ltarg.nodeType == 3) { // defeat Safari bug
        Ltarg = Ltarg.parentNode as HTMLElement;
      }

      if(Ltarg.ownerDocument) {
        if(Ltarg instanceof Ltarg.ownerDocument.defaultView.HTMLIFrameElement) {
          Ltarg=Ltarg.contentWindow.frameElement as HTMLElement;
        }
      }

      ////keymanweb._SelectionControl = null;
      if(this.keyman.domManager.lastActiveElement) {
        this._BlurKeyboardSettings(this.keyman.domManager.lastActiveElement);
      }

      // Now that we've handled all prior-element maintenance, update the active and 'last-active element'.
      this.keyman.domManager.activeElement = null; // I3363 (Build 301)
      this.keyman.domManager.lastActiveElement = Ltarg;

      /* If the KeymanWeb UI is active as a user changes controls, all UI-based effects should be restrained to this control in case
      * the user is manually specifying languages on a per-control basis.
      */
      focusAssistant.restoringFocus = false;

      let maintainingFocus = focusAssistant.maintainingFocus;
      let activeKeyboard = com.keyman.singleton.core.activeKeyboard;
      if(!maintainingFocus && activeKeyboard) {
        activeKeyboard.notify(0, Utils.getOutputTarget(Ltarg as HTMLElement), 0);  // I2187
      }

      this.doControlBlurred(Ltarg, e, maintainingFocus);

      this.doChangeEvent(Ltarg);
      this.keyman['resetContext']();

      return true;
    }.bind(this);

    /**
     * Function     doControlBlurred
     * Scope        Private
     * @param       {Object}            _target       element losing focus
     * @param       {Event}             _event        event object
     * @param       {(boolean|number)}  _isActivating activation state
     * @return      {boolean}
     * Description  Execute external (UI) code needed on blur
     */
    doControlBlurred(_target: HTMLElement, _event: Event, _isActivating: boolean|number): boolean {
      var p={};
      p['target']=_target;
      p['event']=_event;
      p['isActivating']=_isActivating;

      return this.keyman.util.callEvent('kmw.controlblurred',p);
    }

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

    doChangeEvent(_target: HTMLElement) {
      if(DOMEventHandlers.states.changed) {
        let event = new Event('change', {"bubbles": true, "cancelable": false});
        _target.dispatchEvent(event);
      }

      DOMEventHandlers.states.changed = false;
    }

    _Click: (e: MouseEvent) => boolean = function(this: DOMEventHandlers, e: MouseEvent): boolean {
      let target = e.target as HTMLElement;
      if(target && target['base']) {
        target = target['base'];
      }

      //console.log('processNewContextEvent called from click');
      com.keyman.singleton.core.processNewContextEvent(dom.Utils.getOutputTarget(target));

      return true;
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
