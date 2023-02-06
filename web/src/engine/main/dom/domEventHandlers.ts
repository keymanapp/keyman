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

      if(this.keyman.uiManager.justActivated) {
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
      this.keyman.uiManager.justActivated = false;

      var isActivating = this.keyman.uiManager.isActivating;
      let activeKeyboard = com.keyman.singleton.core.activeKeyboard;
      if(!isActivating && activeKeyboard) {
        activeKeyboard.notify(0, Utils.getOutputTarget(Ltarg as HTMLElement), 0);  // I2187
      }

      this.doControlBlurred(Ltarg, e, isActivating);

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
     * Function             _BlurKeyboardSettings
     * Description          Stores the last active element's keyboard settings.  Should be called
     *                      whenever a KMW-enabled page element loses control.
     */
    _BlurKeyboardSettings(lastElem: HTMLElement, PInternalName?: string, PLgCode?: string) {
      var keyboardID = this.keyman.core.activeKeyboard ? this.keyman.core.activeKeyboard.id : '';
      var langCode = this.keyman.keyboardManager.getActiveLanguage();

      if(PInternalName !== undefined && PLgCode !== undefined) {
        keyboardID = PInternalName;
        langCode = PLgCode;
      }

      if(lastElem && lastElem._kmwAttachment.keyboard != null) {
        lastElem._kmwAttachment.keyboard = keyboardID;
        lastElem._kmwAttachment.languageCode = langCode;
      } else {
        this.keyman.globalKeyboard = keyboardID;
        this.keyman.globalLanguageCode = langCode;
      }
    }

    /**
     * Function             _FocusKeyboardSettings
     * @param   {boolean}   blockGlobalChange   A flag indicating if the global keyboard setting should be ignored for this call.
     * Description          Restores the newly active element's keyboard settings.  Should be called
     *                      whenever a KMW-enabled page element gains control, but only once the prior
     *                      element's loss of control is guaranteed.
     */
    _FocusKeyboardSettings(lastElem: HTMLElement, blockGlobalChange: boolean) {
      if(lastElem && lastElem._kmwAttachment.keyboard != null) {
        this.keyman.keyboardManager.setActiveKeyboard(lastElem._kmwAttachment.keyboard,
          lastElem._kmwAttachment.languageCode);
      } else if(!blockGlobalChange) {
        this.keyman.keyboardManager.setActiveKeyboard(this.keyman.globalKeyboard, this.keyman.globalLanguageCode);
      }

       // Now that we've fully entered the new context, invalidate the context so we can generate initial predictions from it.
      if(this.keyman.modelManager) {
        let outputTarget = dom.Utils.getOutputTarget(lastElem);
        this.keyman.core.languageProcessor.invalidateContext(outputTarget, this.keyman.core.keyboardProcessor?.layerId);
      }
    }

    /**
     * Function             _CommonFocusHelper
     * @param   {Element}   target
     * @returns {boolean}
     * Description          Performs common state management for the various focus events of KeymanWeb.
     *                      The return value indicates whether (true) or not (false) the calling event handler
     *                      should be terminated immediately after the call.
     */
    _CommonFocusHelper(target: HTMLElement): boolean {
      let keyman = com.keyman.singleton;
      var uiManager = this.keyman.uiManager;

      if(target.ownerDocument && target instanceof target.ownerDocument.defaultView.HTMLIFrameElement) {
        if(!this.keyman.domManager._IsEditableIframe(target, 1)) {
          DOMEventHandlers.states._DisableInput = true;
          return true;
        }
      }
      DOMEventHandlers.states._DisableInput = false;

      const outputTarget = dom.Utils.getOutputTarget(target);

      // Ensure that focused element is visible above the keyboard
      if(keyman.util.device.touchable && outputTarget) {
        if(this instanceof DOMTouchHandlers) {
          (this as DOMTouchHandlers).scrollBody(target);
        }
      }

      let activeKeyboard = keyman.core.activeKeyboard;
      if(!uiManager.justActivated) {
        if(target && outputTarget) {
          outputTarget.deadkeys().clear();
        }

        if(activeKeyboard) {
          activeKeyboard.notify(0, outputTarget, 1);  // I2187
        }
      }

      if(!uiManager.justActivated && DOMEventHandlers.states._SelectionControl != target) {
        uiManager.isActivating = false;
      }
      uiManager.justActivated = false;

      DOMEventHandlers.states._SelectionControl = target;

      if(target && outputTarget) {
        // Call the current keyboard's newContext handler;
        // timeout is required in order to get the current
        // selection, which is not ready at time of focus event,
        // at least on Chrome
        window.setTimeout(() => {
          //console.log('processNewContextEvent called from focus');
          com.keyman.singleton.core.processNewContextEvent(outputTarget);
        });
      }

      if(keyman.core.languageProcessor.isActive) {
        keyman.core.languageProcessor.predictFromTarget(outputTarget, keyman.core.keyboardProcessor?.layerId);
      }
      return false;
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

  /**
   * Defines numerous functions for handling and modeling touch-based aliases.
   */
  export class DOMTouchHandlers extends DOMEventHandlers {
    firstTouch: {
      x: number;
      y: number;
    };


    constructor(keyman: KeymanBase) {
      super(keyman);
    }

    /**
     * Close OSK and remove simulated caret on losing focus
     */
    cancelInput(): void {
      this.keyman.domManager.activeElement = null;
      this.keyman.domManager.lastActiveElement = null;
      this.keyman.osk.hideNow();
    };

    /**
     * Handle the touch end event for an input element
     */
    dragEnd: (e: TouchEvent) => void = function(this: DOMTouchHandlers, e: TouchEvent) {
      e.stopPropagation();
      this.firstTouch = null;
    }.bind(this);

    /**
     * Scroll the document body vertically to bring the active input into view
     *
     * @param       {Object}      e        input field object being focussed
     */
    scrollBody(e: HTMLElement): void {
      var osk = this.keyman.osk;

      if(!e || !osk) {
        return;
      }

      // Get the absolute position of the caret
      const y = dom.Utils.getAbsoluteY(e);
      const t = window.pageYOffset;
      let dy = 0;
      if(y < t) {
        dy=y-t;
      } else {
        dy=y-t-(window.innerHeight-osk._Box.offsetHeight-e.offsetHeight-2);
        if(dy < 0) dy=0;
      }
      // Hide OSK, then scroll, then re-anchor OSK with absolute position (on end of scroll event)
      if(dy != 0) {
        window.scrollTo(0,dy+window.pageYOffset);
      }
    }
  }
}
