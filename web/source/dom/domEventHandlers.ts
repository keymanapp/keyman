/// <reference path="touchAliasElement.ts" />
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
    
    activeElement: HTMLElement;
    lastActiveElement: HTMLElement;

    focusing: boolean;
    focusTimer: number;

    changed: boolean;         // Tracks if the element has been edited since gaining focus.
    swallowKeypress: boolean; // Notes if a keypress should be swallowed; used when handing mnemonics.

    /* ----------------------- Static event-related methods ------------------------ */

    setFocusTimer(): void {
      this.focusing=true;

      this.focusTimer = window.setTimeout(function() {
        this.focusing=false;
      }.bind(this), 1000)
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

    /**
     * Handle receiving focus by simulated input field
     */       
    setFocus: (e?: TouchEvent|MSPointerEvent) => void = function(e?: TouchEvent|MSPointerEvent): void {
      // Touch-only handler.
    }.bind(this);
    
    /** 
     * Handles touch-based loss of focus events.
     */
    setBlur: (e: FocusEvent) => void = function(e: FocusEvent) {
      // Touch-only handler.
    }.bind(this);

    // End of I3363 (Build 301) additions

    // Universal DOM event handlers (both desktop + touch)

    //TODO: add more complete description of what ControlFocus really does
    /**
     * Respond to KeymanWeb-aware input element receiving focus 
     */    
    _ControlFocus: (e: FocusEvent) => boolean = function(this: DOMEventHandlers, e: FocusEvent): boolean {
      var Ltarg: HTMLElement, Ln;
      var device = this.keyman.util.device;
      var osk = this.keyman.osk;

      e = this.keyman._GetEventObject<FocusEvent>(e);     // I2404 - Manage IE events in IFRAMEs
      Ltarg = this.keyman.util.eventTarget(e) as HTMLElement;
      if (Ltarg == null) {
        return true;
      }

      if(Ltarg['body']) {
        Ltarg = Ltarg['body']; // Occurs in Firefox for design-mode iframes.
      }
    
      // Prevent any action if a protected input field
      if(device.touchable && (Ltarg.className == null || Ltarg.className.indexOf('keymanweb-input') < 0)) {
        return true;
      }

      // Or if not a remappable input field
      var en=Ltarg.nodeName.toLowerCase();
      if(Ltarg.ownerDocument && Ltarg instanceof Ltarg.ownerDocument.defaultView.HTMLInputElement) {
        var et=Ltarg.type.toLowerCase();
        if(!(et == 'text' || et == 'search')) {
          return true;
        }
      } else if(Ltarg.ownerDocument && Ltarg.ownerDocument.designMode == 'on') {
        // continue; don't block this one!
      } else if((device.touchable || !Ltarg.isContentEditable) 
          && !(Ltarg.ownerDocument && Ltarg instanceof Ltarg.ownerDocument.defaultView.HTMLTextAreaElement)) {
        return true;
      }

      DOMTouchHandlers.states.activeElement = Ltarg;  // I3363 (Build 301)  

      if (Ltarg.nodeType == 3) { // defeat Safari bug
        Ltarg = Ltarg.parentNode as HTMLElement;
      }
        
      var LfocusTarg = Ltarg;

      // Ensure that focussed element is visible above the keyboard
      if(Ltarg.className == null || Ltarg.className.indexOf('keymanweb-input') < 0) {
        if(this instanceof DOMTouchHandlers) {
          (this as DOMTouchHandlers).scrollBody(Ltarg);
        }
      }
          
      if(Ltarg.ownerDocument && Ltarg instanceof Ltarg.ownerDocument.defaultView.HTMLIFrameElement) { //**TODO: check case reference
        this.keyman.domManager._AttachToIframe(Ltarg as HTMLIFrameElement);
        Ltarg=Ltarg.contentWindow.document.body;
      }

      //??keymanweb._Selection = null;

      // We condition on 'priorElement' below as a check to allow KMW to set a default active keyboard.
      var priorElement = DOMEventHandlers.states.lastActiveElement;
      DOMEventHandlers.states.lastActiveElement = Ltarg;

      if(this.keyman.uiManager.justActivated) {
        this._BlurKeyboardSettings();
      } else {
        this._FocusKeyboardSettings(priorElement ? false : true);
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
      this.doControlFocused(LfocusTarg, DOMEventHandlers.states.lastActiveElement);
    
      // Force display of OSK for touch input device, or if a CJK keyboard, to ensure visibility of pick list
      if(device.touchable) {
        osk._Enabled = true;
      } else {
        // Conditionally show the OSK when control receives the focus
        if(osk.ready) {
          if(this.keyman.isCJK()) {
            osk._Enabled = true;
          }
          if(osk._Enabled) {
            osk._Show();
          } else {
            osk._Hide(false);
          }
        }
      }

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
      var Ltarg: HTMLElement;  

      e = this.keyman._GetEventObject<FocusEvent>(e);   // I2404 - Manage IE events in IFRAMEs
      Ltarg = this.keyman.util.eventTarget(e) as HTMLElement;
      if (Ltarg == null) {
        return true;
      }

      if(Ltarg['body']) {
        Ltarg = Ltarg['body']; // Occurs in Firefox for design-mode iframes.
      }

      if(DOMEventHandlers.states._IgnoreBlurFocus) {
        // Prevent triggering other blur-handling events (as possible)
        e.cancelBubble = true;
        e.stopPropagation();
        return true;
      }

      // Hide the touch device input caret, if applicable  I3363 (Build 301)
      if(dom.Utils.instanceof(DOMEventHandlers.states.activeElement, "TouchAliasElement")) {
        let lastAlias = <TouchAliasElement> DOMEventHandlers.states.activeElement;
        lastAlias.hideCaret();
      }

      DOMEventHandlers.states.activeElement = null; // I3363 (Build 301)
      
      if (Ltarg.nodeType == 3) { // defeat Safari bug
        Ltarg = Ltarg.parentNode as HTMLElement;
      }

      if(Ltarg.ownerDocument) {
        if(Ltarg instanceof Ltarg.ownerDocument.defaultView.HTMLIFrameElement) {
          Ltarg=Ltarg.contentWindow.frameElement as HTMLElement;
        }
      }
      
      ////keymanweb._SelectionControl = null;    
      this._BlurKeyboardSettings();

      // Now that we've handled all prior-element maintenance, update the 'last active element'.
      DOMEventHandlers.states.lastActiveElement = Ltarg;

      /* If the KeymanWeb UI is active as a user changes controls, all UI-based effects should be restrained to this control in case
      * the user is manually specifying languages on a per-control basis.
      */
      this.keyman.uiManager.justActivated = false;
      
      var isActivating = this.keyman.uiManager.isActivating;
      let activeKeyboard = com.keyman.singleton.core.activeKeyboard;
      if(!isActivating && activeKeyboard) {
        activeKeyboard.notify(0, Utils.getOutputTarget(Ltarg as HTMLElement), 0);  // I2187
      }

      //e = this.keyman._GetEventObject<FocusEvent>(e);   // I2404 - Manage IE events in IFRAMEs  //TODO: is this really needed again????
      this.doControlBlurred(Ltarg, e, isActivating);

      // Hide the OSK when the control is blurred, unless the UI is being temporarily selected
      if(this.keyman.osk.ready && !isActivating) {
        this.keyman.osk._Hide(false);
      }

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
    _BlurKeyboardSettings(PInternalName?: string, PLgCode?: string) {
      var keyboardID = this.keyman.core.activeKeyboard ? this.keyman.core.activeKeyboard.id : '';
      var langCode = this.keyman.keyboardManager.getActiveLanguage();
      
      if(PInternalName !== undefined && PLgCode !== undefined) {
        keyboardID = PInternalName;
        langCode = PLgCode;
      }
      
      var lastElem = DOMEventHandlers.states.lastActiveElement;

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
    _FocusKeyboardSettings(blockGlobalChange: boolean) {
      var lastElem = DOMEventHandlers.states.lastActiveElement;

      if(lastElem && lastElem._kmwAttachment.keyboard != null) {
        this.keyman.keyboardManager.setActiveKeyboard(lastElem._kmwAttachment.keyboard,
          lastElem._kmwAttachment.languageCode);
      } else if(!blockGlobalChange) {
        this.keyman.keyboardManager.setActiveKeyboard(this.keyman.globalKeyboard, this.keyman.globalLanguageCode);
      }

       // Now that we've fully entered the new context, invalidate the context so we can generate initial predictions from it.
      if(this.keyman.modelManager) {
        this.keyman.core.languageProcessor.invalidateContext();
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
      //TODO: the logic of the following line doesn't look right!!  Both variables are true, but that doesn't make sense!
      //_Debug(keymanweb._IsIEEditableIframe(Ltarg,1) + '...' +keymanweb._IsMozillaEditableIframe(Ltarg,1));
      if(target.ownerDocument && target instanceof target.ownerDocument.defaultView.HTMLIFrameElement) {
        if(!this.keyman.domManager._IsIEEditableIframe(target, 1) ||
            !this.keyman.domManager._IsMozillaEditableIframe(target, 1)) {
          DOMEventHandlers.states._DisableInput = true; 
          return true;
        }
      }
      DOMEventHandlers.states._DisableInput = false; 

      let activeKeyboard = keyman.core.activeKeyboard;
      if(!uiManager.justActivated) {
        if(target && Utils.getOutputTarget(target)) {
          Utils.getOutputTarget(target).deadkeys().clear();
        }
        
        if(activeKeyboard) {
          activeKeyboard.notify(0, Utils.getOutputTarget(target), 1);  // I2187
        }
      }
    
      if(!uiManager.justActivated && DOMEventHandlers.states._SelectionControl != target) {
        uiManager.isActivating = false;
      }
      uiManager.justActivated = false;

      DOMEventHandlers.states._SelectionControl = target;

      if(keyman.core.languageProcessor.isActive) {
        keyman.core.languageProcessor.predictFromTarget(Utils.getOutputTarget(target));
      }
      return false;
    }

    /**
     * Function   _SelectionChange
     * Scope      Private
     * Description Respond to selection change event 
     */
    _SelectionChange: () => boolean = function(this: DOMEventHandlers): boolean {
      if(DOMEventHandlers.states._IgnoreNextSelChange) {
        DOMEventHandlers.states._IgnoreNextSelChange--;
      } 
      return true;
    }.bind(this);


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
      var osk = this.keyman.osk;
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
      } else if(el && el.className.indexOf('kmw-disabled') >= 0) {
        return true; 
      }
      
      // Or if OSK not yet ready (for any reason)
      if(!osk.ready) {
        return true;
      }
      
      return PreProcessor.keyDown(e);
    }.bind(this);

    doChangeEvent(_target: HTMLElement) {
      if(DOMEventHandlers.states.changed) {
        var event: Event;
        if(typeof Event == 'function') {
          event = new Event('change', {"bubbles": true, "cancelable": false});
        } else { // IE path
          event = document.createEvent("HTMLEvents");
          event.initEvent('change', true, false);
        }

        // Ensure that touch-aliased elements fire as if from the aliased element.
        if(_target['base'] && _target['base']['kmw_ip']) {
          _target = _target['base'];
        }
        _target.dispatchEvent(event);
      }

      DOMEventHandlers.states.changed = false;
    }

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
      var osk = this.keyman.osk;

      var Levent = PreProcessor._GetKeyEventProperties(e, false);
      if(Levent == null || !osk.ready) {
        return true;
      }
      var inputEle = (Levent.Ltarg as targets.OutputTarget).getElement();

      // Since this part concerns DOM element + browser interaction management, we preprocess it for
      // browser form commands before passing control to the Processor module.
      if(Levent.Lcode == 13) {
        var ignore = false;
        if(Levent.Ltarg instanceof inputEle.ownerDocument.defaultView.HTMLTextAreaElement) {
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
     * Handle receiving focus by simulated input field 
     *      
     */       
    setFocus: (e?: TouchEvent|MSPointerEvent) => void = function(this: DOMTouchHandlers, e?: TouchEvent|MSPointerEvent): void {
      DOMEventHandlers.states.setFocusTimer();

      var tEvent: {
        clientX: number;
        clientY: number;
        target?: EventTarget;
      };

      if(e && dom.Utils.instanceof(e, "TouchEvent")) {
        tEvent=(e as TouchEvent).touches[0];
      } else { // Allow external code to set focus and thus display the OSK on touch devices if required (KMEW-123)
        tEvent={clientX:0, clientY:0}
        // Will usually be called from setActiveElement, which should define DOMEventHandlers.states.lastActiveElement
        if(DOMEventHandlers.states.lastActiveElement) {
          tEvent.target = DOMEventHandlers.states.lastActiveElement['kmw_ip'];
        // but will default to first input or text area on page if DOMEventHandlers.states.lastActiveElement is null
        } else {
          tEvent.target = this.keyman.domManager.sortedInputs[0]['kmw_ip'];
        }
      }

      this.setFocusWithTouch(tEvent);
    }.bind(this);
      
    setFocusWithTouch(tEvent: {clientX: number, clientY: number, target?: EventTarget}) {
      var osk = this.keyman.osk;

      var touchX=tEvent.clientX,touchY=tEvent.clientY;
      var tTarg=tEvent.target as HTMLElement;
      var scroller: HTMLElement;

      // Identify the scroller element
      if(tTarg && dom.Utils.instanceof(tTarg, "HTMLSpanElement")) {
        scroller=tTarg.parentNode as HTMLElement;
      } else if(tTarg && (tTarg.className != null && tTarg.className.indexOf('keymanweb-input') >= 0)) {
        scroller=tTarg.firstChild as HTMLElement;
      } else {
        scroller=tTarg;
      }

      // And the actual target element        
      var target=scroller.parentNode as TouchAliasElement;

      // Move the caret and refocus if necessary     
      if(DOMEventHandlers.states.activeElement != target) {
        // Hide the KMW caret
        let prevTarget = <TouchAliasElement> DOMEventHandlers.states.activeElement;
        if(prevTarget) {
          prevTarget.hideCaret();
        }
        DOMEventHandlers.states.activeElement=target;
        // The issue here is that touching a DIV does not actually set the focus for iOS, even when enabled to accept focus (by setting tabIndex=0)
        // We must explicitly set the focus in order to remove focus from any non-KMW input
        target.focus();  //Android native browsers may not like this, but it is needed for Chrome, Safari
      }  
      
      // Correct element directionality if required
      this.keyman.domManager._SetTargDir(target);  
      // What we really want to do is to blur any active element that is not a KMW input, 
      // but the following line does not work as might be expected, even though the correct element is referenced.
      // It is as though blur is ignored if focus is supposed to have been moved, even if it has not in fact been moved?
      //if(document.activeElement.nodeName != 'DIV' && document.activeElement.nodeName != 'BODY') document.activeElement.blur();
      
      // And display the OSK if not already visible
      if(osk.ready && !osk._Visible) {
        osk._Show();
      }
      
      // If clicked on DIV, set caret to end of text
      if(tTarg && dom.Utils.instanceof(tTarg, "TouchAliasElement")) {
        var x,cp;
        x=dom.Utils.getAbsoluteX(scroller.firstChild as HTMLElement);        
        if(target.dir == 'rtl') { 
          x += (scroller.firstChild as HTMLElement).offsetWidth;        
          cp=(touchX > x ? 0 : 100000);
        } else {
          cp=(touchX<x ? 0 : 100000);
        }
    
        target.setTextCaret(cp);
        target.scrollInput();        
      } else { // Otherwise, if clicked on text in SPAN, set at touch position
        var caret,cp,cpMin,cpMax,x,y,dy,yRow,iLoop;
        caret=scroller.childNodes[1]; //caret span
        cpMin=0;
        cpMax=target.getText()._kmwLength();
        cp=target.getTextCaret();
        dy=document.body.scrollTop;

        // Vertical scrolling
        if(target.base instanceof target.base.ownerDocument.defaultView.HTMLTextAreaElement) {
          yRow=Math.round(target.base.offsetHeight/(target.base as HTMLTextAreaElement).rows);     
          for(iLoop=0; iLoop<16; iLoop++)
          {
            y=dom.Utils.getAbsoluteY(caret)-dy;  //top of caret            
            if(y > touchY && cp > cpMin && cp != cpMax) {cpMax=cp; cp=Math.round((cp+cpMin)/2);}
            else if(y < touchY-yRow && cp < cpMax && cp != cpMin) {cpMin=cp; cp=Math.round((cp+cpMax)/2);}
            else break;
            target.setTextCaret(cp);
          }

          while(dom.Utils.getAbsoluteY(caret)-dy > touchY && cp > cpMin) {
            target.setTextCaret(--cp);
          }

          while(dom.Utils.getAbsoluteY(caret)-dy < touchY-yRow && cp < cpMax) {
            target.setTextCaret(++cp);
          }
        }

        // Caret repositioning for horizontal scrolling of RTL text

        // snapOrder - 'snaps' the touch location in a manner corresponding to the 'ltr' vs 'rtl' orientation.
        // Think of it as performing a floor() function, but the floor depends on the origin's direction.
        var snapOrder;
        if(target.dir == 'rtl') {  // I would use arrow functions, but IE doesn't like 'em.
          snapOrder = function(a, b) {
            return a < b; 
          };
        } else {
          snapOrder = function(a, b) { 
            return a > b; 
          };
        }

        for(iLoop=0; iLoop<16; iLoop++) {
          x=dom.Utils.getAbsoluteX(caret);  //left of caret            
          if(snapOrder(x, touchX) && cp > cpMin && cp != cpMax) {
            cpMax=cp; 
            cp=Math.round((cp+cpMin)/2);
          } else if(!snapOrder(x, touchX) && cp < cpMax && cp != cpMin) {
            cpMin=cp; 
            cp=Math.round((cp+cpMax)/2);
          } else {
            break;
          }
          target.setTextCaret(cp);
        }

        while(snapOrder(dom.Utils.getAbsoluteX(caret), touchX) && cp > cpMin) {
          target.setTextCaret(--cp);
        }
        while(!snapOrder(dom.Utils.getAbsoluteX(caret), touchX) && cp < cpMax) {
          target.setTextCaret(++cp);
        }
      }

      /**
       * This event will trigger before keymanweb.setBlur is triggered.  Now that we're allowing independent keyboard settings
       * for controls, we have to act here to preserve the outgoing control's keyboard settings.
       *
       * If we 'just activated' the KeymanWeb UI, we need to save the new keyboard change as appropriate.
       */  
      this._BlurKeyboardSettings();

      // With the attachment API update, we now directly track the old legacy control behavior.
      DOMEventHandlers.states.lastActiveElement = target;
      target.showCaret();

      /**
       * If we 'just activated' the KeymanWeb UI, we need to save the new keyboard change as appropriate.
       * If not, we need to activate the control's preferred keyboard.
       */
      this._FocusKeyboardSettings(false);
      
      // Always do the common focus stuff, instantly returning if we're in an editable iframe.
      // This parallels the if-statement in _ControlFocus - it may be needed as this if-statement in the future,
      // despite its present redundancy.
      if(this._CommonFocusHelper(target)) {
        return;
      }
    }

    /**
     * Close OSK and remove simulated caret on losing focus
     */          
    cancelInput(): void {
      if(DOMEventHandlers.states.activeElement && Utils.instanceof(DOMEventHandlers.states.activeElement, "TouchAliasElement")) {
        (DOMEventHandlers.states.activeElement as TouchAliasElement).hideCaret();
      }
      DOMEventHandlers.states.activeElement=null; 
      this.keyman.osk.hideNow();
    };

    /**
     * Handle losing focus from simulated input field 
     */
    setBlur: (e: FocusEvent) => void = function(this: DOMTouchHandlers, e: FocusEvent) {
      // This works OK for iOS, but may need something else for other platforms
      var elem: HTMLElement;

      if(('relatedTarget' in e) && e.relatedTarget) {
        elem = e.relatedTarget as HTMLElement;
      }

      this.keyman['resetContext']();

      if(elem) {
        this.doChangeEvent(elem);
        if(elem.nodeName != 'DIV' || elem.className.indexOf('keymanweb-input') == -1) {
          this.cancelInput(); 
          return;
        }
      }

      //Hide the OSK
      if(!DOMEventHandlers.states.focusing) {
        this.cancelInput();
      }
    }.bind(this);

    /**
     * Display and position a scrollbar in the input field if needed
     * 
     * @param   {Object}  e   input DIV element (copy of INPUT or TEXTAREA)
     */
    setScrollBar(e: HTMLElement) {
      // Display the scrollbar if necessary.  Added TEXTAREA condition to correct rotation issue KMW-5.  Fixed for 310 beta.
      var scroller=<HTMLElement>e.childNodes[0], sbs=(<HTMLElement>e.childNodes[1]).style;
      if((scroller.offsetWidth > e.offsetWidth || scroller.offsetLeft < 0) && (e.base.nodeName != 'TEXTAREA')) {
        sbs.height='4px';
        sbs.width=100*(e.offsetWidth/scroller.offsetWidth)+'%';
        sbs.left=100*(-scroller.offsetLeft/scroller.offsetWidth)+'%';
        sbs.top='0';
        sbs.visibility='visible';  
      } else if(scroller.offsetHeight > e.offsetHeight || scroller.offsetTop < 0) {
        sbs.width='4px';
        sbs.height=100*(e.offsetHeight/scroller.offsetHeight)+'%';
        sbs.top=100*(-scroller.offsetTop/scroller.offsetHeight)+'%';
        sbs.left='0';    
        sbs.visibility='visible';        
      } else {
        sbs.visibility='hidden';
      }
    }                    

    /**
     * Handle the touch move event for an input element
     */         
    dragInput: (e: TouchEvent|MouseEvent) => void = function(this: DOMTouchHandlers, e: TouchEvent|MouseEvent) {
      // Prevent dragging window 
      e.preventDefault();
      e.stopPropagation();      

      // Identify the target from the touch list or the event argument (IE 10 only)
      var target: HTMLElement;
      
      if(dom.Utils.instanceof(e, "TouchEvent")) {
        target = (e as TouchEvent).targetTouches[0].target as HTMLElement;
      } else {
        target = e.target as HTMLElement;
      }
      if(target == null) {
        return;
      }
      
      // Identify the input element from the touch event target (touched element may be contained by input)
      if(target.className == null || target.className.indexOf('keymanweb-input') < 0) target=<HTMLElement> target.parentNode;
      if(target.className == null || target.className.indexOf('keymanweb-input') < 0) target=<HTMLElement> target.parentNode;
      if(target.className == null || target.className.indexOf('keymanweb-input') < 0) return;
      
      var x, y;

      if(dom.Utils.instanceof(e, "TouchEvent")) {
        x = (e as TouchEvent).touches[0].screenX;
        y = (e as TouchEvent).touches[0].screenY;
      } else {
        x = (e as MouseEvent).screenX;
        y = (e as MouseEvent).screenY;
      }
                
      // Allow content of input elements to be dragged horizontally or vertically
      if(typeof this.firstTouch == 'undefined' || this.firstTouch == null) {
        this.firstTouch={x:x,y:y};
      } else {
        var x0=this.firstTouch.x,y0=this.firstTouch.y,
          scroller=target.firstChild as HTMLElement,dx,dy,x1;
        
        if(target.base.nodeName == 'TEXTAREA') {
          var yOffset=parseInt(scroller.style.top,10);
          if(isNaN(yOffset)) yOffset=0;
          dy=y0-y;
          if(dy < -4 || dy > 4) {
            scroller.style.top=(yOffset<dy?yOffset-dy:0)+'px';
            this.firstTouch.y=y;  
          } 
        } else {
          var xOffset=parseInt(scroller.style.left,10);
          if(isNaN(xOffset)) xOffset=0;
          dx=x0-x;
          if(dx < -4 || dx > 4)
          {
            // Limit dragging beyond the defined text (to avoid dragging the text completely out of view)
            var xMin=0, xMax= dom.Utils.getAbsoluteX(target)+target.offsetWidth-scroller.offsetWidth-32;
            if(target.base.dir == 'rtl')xMin=16; else xMax=xMax-24;            
            x1=xOffset-dx;
            if(x1 > xMin) x1=xMin;
            if(x1 < xMax) x1=xMax;
            scroller.style.left=x1+'px';
            this.firstTouch.x=x;       
          }    
        }
      }
      this.setScrollBar(target);
    }.bind(this);

    /**
     * Scroll the document body vertically to bring the active input into view
     * 
     * @param       {Object}      e        simulated input field object being focussed
     */         
    scrollBody(e: HTMLElement): void {
      var osk = this.keyman.osk;
      var util = this.keyman.util;

      if(!e || e.className == null || e.className.indexOf('keymanweb-input') < 0 || !osk.ready) {
        return;
      }

      // Get the absolute position of the caret
      var s2=<HTMLElement>e.firstChild.childNodes[1], y=dom.Utils.getAbsoluteY(s2), t=window.pageYOffset,dy=0;
      if(y < t) {
        dy=y-t;
      } else {
        dy=y-t-(window.innerHeight-osk._Box.offsetHeight-s2.offsetHeight-2);
        if(dy < 0) dy=0;
      }    
      // Hide OSK, then scroll, then re-anchor OSK with absolute position (on end of scroll event)
      if(dy != 0) {
        window.scrollTo(0,dy+window.pageYOffset);
      }
    }
  }
}
