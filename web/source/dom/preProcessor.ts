namespace com.keyman.dom {
  export class PreProcessor {
    /**
     * Function     _GetEventKeyCode
     * Scope        Private
     * @param       {Event}       e         Event object
     * Description  Finds the key code represented by the event.
     */
    static _GetEventKeyCode(e: KeyboardEvent) {
      if (e.keyCode) {
        return e.keyCode;
      } else if (e.which) {
        return e.which;
      } else {
        return null;
      }
    }

    /**
     * Function     _GetKeyEventProperties
     * Scope        Private
     * @param       {Event}       e         Event object
     * @param       {boolean=}    keyState  true if call results from a keyDown event, false if keyUp, undefined if keyPress
     * @return      {Object.<string,*>}     KMW keyboard event object: 
     * Description  Get object with target element, key code, shift state, virtual key state 
     *                Ltarg=target element
     *                Lcode=keyCode
     *                Lmodifiers=shiftState
     *                LisVirtualKeyCode e.g. ctrl/alt key
     *                LisVirtualKey     e.g. Virtual key or non-keypress event
     */    
    static _GetKeyEventProperties(e: KeyboardEvent, keyState?: boolean): text.KeyEvent {
      let keyman = com.keyman.singleton;
      let core = keyman.core;
      var s = new text.KeyEvent();

      e = keyman._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      if(e.cancelBubble === true) {
        return null; // I2457 - Facebook meta-event generation mess -- two events generated for a keydown in Facebook contentEditable divs
      }    

      let target = keyman.util.eventTarget(e) as HTMLElement;
      if (target == null) {
        return null;
      } else if (target.nodeType == 3) {// defeat Safari bug
        target = target.parentNode as HTMLElement;
      }
      s.Ltarg = Utils.getOutputTarget(target);

      s.Lcode = this._GetEventKeyCode(e);
      if (s.Lcode == null) {
        return null;
      }

      // Stage 1 - track the true state of the keyboard's modifiers.
      var prevModState = core.keyboardProcessor.modStateFlags, curModState = 0x0000;
      var ctrlEvent = false, altEvent = false;
      
      let keyCodes = text.Codes.keyCodes;
      switch(s.Lcode) {
        case keyCodes['K_CTRL']:      // The 3 shorter "K_*CTRL" entries exist in some legacy keyboards.
        case keyCodes['K_LCTRL']:
        case keyCodes['K_RCTRL']:
        case keyCodes['K_CONTROL']:
        case keyCodes['K_LCONTROL']:
        case keyCodes['K_RCONTROL']:
          ctrlEvent = true;
          break;
        case keyCodes['K_LMENU']:     // The 2 "K_*MENU" entries exist in some legacy keyboards.
        case keyCodes['K_RMENU']:
        case keyCodes['K_ALT']:
        case keyCodes['K_LALT']:
        case keyCodes['K_RALT']:
          altEvent = true;
          break;
      }

      /**
       * Two separate conditions exist that should trigger chiral modifier detection.  Examples below use CTRL but also work for ALT.
       * 
       * 1.  The user literally just pressed CTRL, so the event has a valid `location` property we can utilize.  
       *     Problem: its layer isn't presently activated within the OSK.
       * 
       * 2.  CTRL has been held a while, so the OSK layer is valid, but the key event doesn't tell us the chirality of the active CTRL press.
       *     Bonus issue:  RAlt simulation may cause erasure of this location property, but it should ONLY be empty if pressed in this case.
       *     We default to the 'left' variants since they're more likely to exist and cause less issues with RAlt simulation handling.
       * 
       * In either case, `e.getModifierState("Control")` is set to true, but as a result does nothing to tell us which case is active.
       * 
       * `e.location != 0` if true matches condition 1 and matches condition 2 if false.
       */

      curModState |= (e.getModifierState("Shift") ? 0x10 : 0);

      let modifierCodes = text.Codes.modifierCodes;
      if(e.getModifierState("Control")) {
        curModState |= ((e.location != 0 && ctrlEvent) ? 
          (e.location == 1 ? modifierCodes['LCTRL'] : modifierCodes['RCTRL']) : // Condition 1
          prevModState & 0x0003);                                                       // Condition 2
      }
      if(e.getModifierState("Alt")) {
        curModState |= ((e.location != 0 && altEvent) ? 
          (e.location == 1 ? modifierCodes['LALT'] : modifierCodes['RALT']) :   // Condition 1
          prevModState & 0x000C);                                                       // Condition 2
      }

      // Stage 2 - detect state key information.  It can be looked up per keypress with no issue.
      s.Lstates = 0;
      
      s.Lstates |= e.getModifierState('CapsLock') ? modifierCodes['CAPS'] : modifierCodes['NO_CAPS'];
      s.Lstates |= e.getModifierState('NumLock') ? modifierCodes['NUM_LOCK'] : modifierCodes['NO_NUM_LOCK'];
      s.Lstates |= (e.getModifierState('ScrollLock') || e.getModifierState("Scroll")) // "Scroll" for IE9.
        ? modifierCodes['SCROLL_LOCK'] : modifierCodes['NO_SCROLL_LOCK'];

      // We need these states to be tracked as well for proper OSK updates.
      curModState |= s.Lstates;

      // Stage 3 - Set our modifier state tracking variable and perform basic AltGr-related management.
      s.LmodifierChange = core.keyboardProcessor.modStateFlags != curModState;
      core.keyboardProcessor.modStateFlags = curModState;

      // For European keyboards, not all browsers properly send both key-up events for the AltGr combo.
      var altGrMask = modifierCodes['RALT'] | modifierCodes['LCTRL'];
      if((prevModState & altGrMask) == altGrMask && (curModState & altGrMask) != altGrMask) {
        // We just released AltGr - make sure it's all released.
        curModState &= ~ altGrMask;
      }
      // Perform basic filtering for Windows-based ALT_GR emulation on European keyboards.
      if(curModState & modifierCodes['RALT']) {
        curModState &= ~modifierCodes['LCTRL'];
      }

      let modifierBitmasks = text.Codes.modifierBitmasks;
      // Stage 4 - map the modifier set to the appropriate keystroke's modifiers.
      var activeKeyboard = core.activeKeyboard;
      if(activeKeyboard && activeKeyboard.isChiral) {
        s.Lmodifiers = curModState & modifierBitmasks.CHIRAL;

        // Note for future - embedding a kill switch here would facilitate disabling AltGr / Right-alt simulation.
        if(activeKeyboard.emulatesAltGr && (s.Lmodifiers & modifierBitmasks['ALT_GR_SIM']) == modifierBitmasks['ALT_GR_SIM']) {
          s.Lmodifiers ^= modifierBitmasks['ALT_GR_SIM'];
          s.Lmodifiers |= modifierCodes['RALT'];
        }
      } else {
        // No need to sim AltGr here; we don't need chiral ALTs.
        s.Lmodifiers = 
          (curModState & 0x10) | // SHIFT
          ((curModState & (modifierCodes['LCTRL'] | modifierCodes['RCTRL'])) ? 0x20 : 0) | 
          ((curModState & (modifierCodes['LALT'] | modifierCodes['RALT']))   ? 0x40 : 0); 
      }

      // Mnemonic handling.
      if(activeKeyboard && activeKeyboard.isMnemonic) {
        // The following will never set a code corresponding to a modifier key, so it's fine to do this,
        // which may change the value of Lcode, here.
        text.KeyboardProcessor.setMnemonicCode(s, e.getModifierState("Shift"), e.getModifierState("CapsLock"));
      }

      // The 0x6F used to be 0x60 - this adjustment now includes the chiral alt and ctrl modifiers in that check.
      var LisVirtualKeyCode = (typeof e.charCode != 'undefined' && e.charCode != null  &&  (e.charCode == 0 || (s.Lmodifiers & 0x6F) != 0));
      s.LisVirtualKey = LisVirtualKeyCode || e.type != 'keypress';

      // Physically-typed keys require use of a 'desktop' form factor and thus are based on a virtual "physical" Device.
      s.device = keyman.util.physicalDevice.coreSpec;

      // This is based on a KeyboardEvent, so it's not considered 'synthetic' within web-core.
      s.isSynthetic = false;

      // Other minor physical-keyboard adjustments
      if(activeKeyboard && !activeKeyboard.isMnemonic) {
        // Positional Layout

        /* 13/03/2007 MCD: Swedish: Start mapping of keystroke to US keyboard */
        var Lbase = KeyMapping.languageMap[core.keyboardProcessor.baseLayout];
        if(Lbase && Lbase['k'+s.Lcode]) {
          s.Lcode=Lbase['k'+s.Lcode];
        }
        /* 13/03/2007 MCD: Swedish: End mapping of keystroke to US keyboard */
        
        if(!activeKeyboard.definesPositionalOrMnemonic && !(s.Lmodifiers & 0x60)) {
          // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
          s = {
            Lcode: KeyMapping._USKeyCodeToCharCode(s),
            Ltarg: s.Ltarg,
            Lmodifiers: 0,
            LisVirtualKey: false,
            vkCode: s.Lcode, // Helps to merge OSK and physical keystroke control paths.
            Lstates: s.Lstates,
            kName: '',
            device: keyman.util.physicalDevice.coreSpec,
            isSynthetic: false
          };
        }
      }

      // Check for any browser-based keymapping before returning the object.
      if(!keyman.isEmbedded && s.device.browser == text.Browser.Firefox) {
        // I1466 - Convert the - keycode on mnemonic as well as positional layouts
        // FireFox, Mozilla Suite
        if(KeyMapping.browserMap.FF['k'+s.Lcode]) {
          s.Lcode = KeyMapping.browserMap.FF['k'+s.Lcode];
        }
      }
      
      return s;
    }

    /**
     * Function     keyDown
     * Scope        Public
     * Description  Processes keydown event and passes data to keyboard. 
     * 
     * Note that the test-case oriented 'recorder' stubs this method to facilitate keystroke
     * recording for use in test cases.  If changing this function, please ensure the recorder is
     * not affected.
     */ 
    static keyDown(e: KeyboardEvent): boolean {
      let core = com.keyman.singleton.core;
      DOMEventHandlers.states.swallowKeypress = false;

      // Get event properties  
      var Levent = this._GetKeyEventProperties(e, true);
      if(Levent == null) {
        return true;
      }

      var LeventMatched = (core.processKeyEvent(Levent) != null);

      if(LeventMatched) {
        if(e  &&  e.preventDefault) {
          e.preventDefault();
          e.stopPropagation();
        }

        DOMEventHandlers.states.swallowKeypress = !!Levent.Lcode;
        // Don't swallow backspaces on keypresses; this allows physical BKSP presses to repeat.
        if(Levent.Lcode == 8) {
          DOMEventHandlers.states.swallowKeypress = false;
        }
      } else {
        DOMEventHandlers.states.swallowKeypress = false;
      }

      return !LeventMatched;
    }

    // KeyUp basically exists for two purposes:
    // 1)  To detect browser form submissions (handled in kmwdomevents.ts)
    // 2)  To detect modifier state changes.
    static keyUp(e: KeyboardEvent): boolean {
      let core = com.keyman.singleton.core;
      var Levent = this._GetKeyEventProperties(e, false);
      if(Levent == null) {
        return true;
      }

      return core.keyboardProcessor.doModifierPress(Levent, false);
    }

    static keyPress(e: KeyboardEvent): boolean {
      let keyman = com.keyman.singleton;
      let core = keyman.core;

      var Levent = this._GetKeyEventProperties(e);
      if(Levent == null || Levent.LisVirtualKey) {
        return true;
      }

      // _Debug('KeyPress code='+Levent.Lcode+'; Ltarg='+Levent.Ltarg.tagName+'; LisVirtualKey='+Levent.LisVirtualKey+'; _KeyPressToSwallow='+keymanweb._KeyPressToSwallow+'; keyCode='+(e?e.keyCode:'nothing'));

      /* I732 START - 13/03/2007 MCD: Swedish: Start positional keyboard layout code: prevent keystroke */
      if(!core.activeKeyboard.isMnemonic) {
        if(!DOMEventHandlers.states.swallowKeypress) {
          return true;
        }
        if(Levent.Lcode < 0x20 || ((<any>keyman)._BrowserIsSafari  &&  (Levent.Lcode > 0xF700  &&  Levent.Lcode < 0xF900))) {
          return true;
        }

        e = keyman._GetEventObject<KeyboardEvent>(e);   // I2404 - Manage IE events in IFRAMEs
        if(e) {
          e.returnValue = false;
        }
        return false;
      }
      /* I732 END - 13/03/2007 MCD: Swedish: End positional keyboard layout code */
      
      // Only reached if it's a mnemonic keyboard.
      if(DOMEventHandlers.states.swallowKeypress || core.keyboardInterface.processKeystroke(Levent.Ltarg, Levent)) {
        DOMEventHandlers.states.swallowKeypress = false;
        if(e && e.preventDefault) {
          e.preventDefault();
          e.stopPropagation();
        }
        return false;
      }

      DOMEventHandlers.states.swallowKeypress = false;
      return true;
    }
  }
}