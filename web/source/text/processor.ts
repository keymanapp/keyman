// Establishes key-code definitions.
/// <reference path="codes.ts" />

namespace com.keyman.text {
  export class Processor {

    // Tracks the simulated value for supported state keys, allowing the OSK to mirror a physical keyboard for them.
    // Using the exact keyCode name from the Codes definitions will allow for certain optimizations elsewhere in the code.
    stateKeys = {
      "K_CAPS":false,
      "K_NUMLOCK":false,
      "K_SCROLL":false
    };

    /**
     * Get the default key string. If keyName is U_xxxxxx, use that Unicode codepoint.
     * Otherwise, lookup the  virtual key code (physical keyboard mapping)
     *
     * @param   {string}  keyName Name of the key
     * @param   {number}  n
     * @param   {number}  keyShiftState
     * @param   {boolean} usingOSK
     * @param   {Object=} Lelem
     * @return  {string}
     */
    defaultKeyOutput(keyName: string, n: number, keyShiftState: number, usingOSK: boolean, Lelem?: HTMLElement): string {
      let keyman = com.keyman.singleton;
      let domManager = keyman.domManager;

      var ch = '', checkCodes = false;
      var touchAlias = (Lelem && typeof(Lelem.base) != 'undefined');
      // check if exact match to SHIFT's code.  Only the 'default' and 'shift' layers should have default key outputs.
      if(keyShiftState == 0) {
        checkCodes = true;
      } else if (keyShiftState == Codes.modifierCodes['SHIFT']) {
        checkCodes = true; 
        keyShiftState = 1; // It's used as an index.
      } else {
        console.warn("KMW only defines default key output for the 'default' and 'shift' layers!");
      }

      // If this was triggered by the OSK -or- if it was triggered within a touch-aliased DIV element.
      if(touchAlias || usingOSK) {
        var code = Codes.keyCodes[keyName];
        if(!code) {
          code = n;
        }

        switch(code) {
          case Codes.keyCodes['K_BKSP']:  //Only desktop UI, not touch devices. TODO: add repeat while mouse down for desktop UI
            keyman.interface.defaultBackspace();
            break;
          case Codes.keyCodes['K_TAB']:
            domManager.moveToNext(keyShiftState);
            break;
          case Codes.keyCodes['K_TABBACK']:
            domManager.moveToNext(true);
            break;
          case Codes.keyCodes['K_TABFWD']:
            domManager.moveToNext(false);
            break;
          case Codes.keyCodes['K_ENTER']:
            // Insert new line in text area fields
            if(Lelem.nodeName == 'TEXTAREA' || (typeof Lelem.base != 'undefined' && Lelem.base.nodeName == 'TEXTAREA')) {
              return '\n';
            // Or move to next field from TEXT fields
            } else if(usingOSK) {
              var inputEle: HTMLInputElement;
              if(com.keyman.Util.instanceof(Lelem, "HTMLInputElement")) {
                inputEle = <HTMLInputElement> Lelem;
              } else if(typeof(Lelem.base) != 'undefined' && com.keyman.Util.instanceof(Lelem.base, "HTMLInputElement")) {
                inputEle = <HTMLInputElement> Lelem.base;
              }

              if (inputEle && (inputEle.type == 'search' || inputEle.type == 'submit')) {
                inputEle.disabled=false;
                inputEle.form.submit();
              } else {
                domManager.moveToNext(false);
              }
            }
            break;
          case Codes.keyCodes['K_SPACE']:
            return ' ';
          // break;
          //
          // // Problem:  clusters, and doing them right.
          // // The commented-out code below should be a decent starting point, but clusters make it complex.
          //
          // case VisualKeyboard.keyCodes['K_LEFT']:
          //   if(touchAlias) {
          //     var caretPos = keymanweb.getTextCaret(Lelem);
          //     keymanweb.setTextCaret(Lelem, caretPos - 1 >= 0 ? caretPos - 1 : 0);
          //   }
          //   break;
          // case VisualKeyboard.keyCodes['K_RIGHT']:
          //   if(touchAlias) {
          //     var caretPos = keymanweb.getTextCaret(Lelem);
          //     keymanweb.setTextCaret(Lelem, caretPos + 1);
          //   }
          //   if(code == VisualKeyboard.keyCodes['K_RIGHT']) {
          //     break;
          //   }
          // // Should we include this?  It could be tricky to do correctly...
          // case VisualKeyboard.keyCodes['K_DEL']:
          //   // Move caret right one unit, then backspace.
          //   if(touchAlias) {
          //     var caretPos = keymanweb.getTextCaret(Lelem);
          //     keymanweb.setTextCaret(Lelem, caretPos + 1);
          //     if(caretPos == keymanweb.getTextCaret(Lelem)) {
          //       // Failed to move right - there's nothing to delete.
          //       break;
          //     }
          //     kbdInterface.defaultBackspace();
          //   }
        }
      }

      // TODO:  Refactor the overloading of the 'n' parameter here into separate methods.

      // Test for fall back to U_xxxxxx key id
      // For this first test, we ignore the keyCode and use the keyName
      if((keyName.substr(0,2) == 'U_')) {
        var codePoint = parseInt(keyName.substr(2,6), 16);
        if (((0x0 <= codePoint) && (codePoint <= 0x1F)) || ((0x80 <= codePoint) && (codePoint <= 0x9F))) {
          // Code points [U_0000 - U_001F] and [U_0080 - U_009F] refer to Unicode C0 and C1 control codes.
          // Check the codePoint number and do not allow output of these codes via U_xxxxxx shortcuts.
          console.log("Suppressing Unicode control code: U_00" + codePoint.toString(16));
          return ch;
        } else {
          // String.fromCharCode() is inadequate to handle the entire range of Unicode
          // Someday after upgrading to ES2015, can use String.fromCodePoint()
          ch=String.kmwFromCharCode(codePoint);
        }
        // Hereafter, we refer to keyCodes.
      } else if(checkCodes) { // keyShiftState can only be '1' or '2'.
        try {
          if(n >= Codes.keyCodes['K_0'] && n <= Codes.keyCodes['K_9']) { // The number keys.
            ch = Codes.codesUS[keyShiftState][0][n-Codes.keyCodes['K_0']];
          } else if(n >= Codes.keyCodes['K_A'] && n <= Codes.keyCodes['K_Z']) { // The base letter keys
            ch = String.fromCharCode(n+(keyShiftState?0:32));  // 32 is the offset from uppercase to lowercase.
          } else if(n >= Codes.keyCodes['K_COLON'] && n <= Codes.keyCodes['K_BKQUOTE']) {
            ch = Codes.codesUS[keyShiftState][1][n-Codes.keyCodes['K_COLON']];
          } else if(n >= Codes.keyCodes['K_LBRKT'] && n <= Codes.keyCodes['K_QUOTE']) {
            ch = Codes.codesUS[keyShiftState][2][n-Codes.keyCodes['K_LBRKT']];
          }
        } catch (e) {
          console.error("Error detected with default mapping for key:  code = " + n + ", shift state = " + (keyShiftState == 1 ? 'shift' : 'default'));
        }
      }
      return ch;
    }

    /**
     * Simulate a keystroke according to the touched keyboard button element
     *
     * Note that the test-case oriented 'recorder' stubs this method to facilitate OSK-based input
     * recording for use in test cases.  If changing this function, please ensure the recorder is
     * not affected.
     * 
     * @param       {Object}      e      element touched (or clicked)
     */
    clickKey(e: osk.KeyElement) {
      let keyman = com.keyman.singleton;
      var Lelem = keyman.domManager.getLastActiveElement(), Ls, Le, Lkc;

      var activeKeyboard = keyman.keyboardManager.activeKeyboard;
      let kbdInterface = keyman.interface;
      let formFactor = keyman.util.device.formFactor;

      if(Lelem != null) {
        // Get key name and keyboard shift state (needed only for default layouts and physical keyboard handling)
        // Note - virtual keys should be treated case-insensitive, so we force uppercasing here.
        var layer=e['key'].spec.layer || '', keyName=e['keyId'].toUpperCase(), keyShiftState=this.getModifierState(keyman['osk'].vkbd.layerId);
        var nextLayer: string = e['key'].spec['nextlayer'];

        keyman.domManager.initActiveElement(Lelem);

        // Exclude menu and OSK hide keys from normal click processing
        if(keyName == 'K_LOPT' || keyName == 'K_ROPT') {
          keyman['osk'].vkbd.optionKey(e, keyName, true);
          return true;
        }

        // Turn off key highlighting (or preview)
        keyman['osk'].vkbd.highlightKey(e,false);

        // The default OSK layout for desktop devices does not include nextlayer info, relying on modifier detection here.
        if(formFactor == 'desktop') {
          if(this.selectLayer(keyName, nextLayer)) {
            return true;
          }
        }

        // Prevent any output from 'ghost' (unmapped) keys
        if(keyName != 'K_SPACE') {
          var keyText=(<HTMLElement> e.childNodes[0]).innerHTML;
          //// if(keyText == '' || keyText == '&nbsp;') return true; --> why?
        }

        Ls=Lelem._KeymanWebSelectionStart;
        Le=Lelem._KeymanWebSelectionEnd;
        keyman.uiManager.setActivatingUI(true);
        com.keyman.DOMEventHandlers.states._IgnoreNextSelChange = 100;
        keyman.domManager.focusLastActiveElement();
        if(keyman.domManager._IsMozillaEditableIframe(<HTMLIFrameElement> Lelem,0)) {
          Lelem = (<HTMLIFrameElement> Lelem).contentDocument.documentElement;
        }
        Lelem._KeymanWebSelectionStart=Ls;
        Lelem._KeymanWebSelectionEnd=Le;
        com.keyman.DOMEventHandlers.states._IgnoreNextSelChange = 0;
        // ...end I3363 (Build 301)
        (<any>keyman)._CachedSelectionStart = null; // I3319
        // Deadkey matching continues to be troublesome.
        // Deleting matched deadkeys here seems to correct some of the issues.   (JD 6/6/14)
        kbdInterface._DeadkeyDeleteMatched();      // Delete any matched deadkeys before continuing
        //kbdInterface._DeadkeyResetMatched();       // I3318   (Not needed if deleted first?)

        // First check the virtual key, and process shift, control, alt or function keys
        Lkc = {
          Ltarg:Lelem,
          Lmodifiers:0,
          Lstates:0,
          Lcode: Codes.keyCodes[keyName],
          LisVirtualKey:true
        };

        // Set the flags for the state keys.
        Lkc.Lstates |= this.stateKeys['K_CAPS']    ? Codes.modifierCodes['CAPS'] : Codes.modifierCodes['NO_CAPS'];
        Lkc.Lstates |= this.stateKeys['K_NUMLOCK'] ? Codes.modifierCodes['NUM_LOCK'] : Codes.modifierCodes['NO_NUM_LOCK'];
        Lkc.Lstates |= this.stateKeys['K_SCROLL']  ? Codes.modifierCodes['SCROLL_LOCK'] : Codes.modifierCodes['NO_SCROLL_LOCK'];

        // Set LisVirtualKey to false to ensure that nomatch rule does fire for U_xxxx keys
        if(keyName.substr(0,2) == 'U_') Lkc.LisVirtualKey=false;

        // Get code for non-physical keys (T_KOKAI, U_05AB etc)
        if(typeof Lkc.Lcode == 'undefined') {
          Lkc.Lcode = this.getVKDictionaryCode(keyName);// Updated for Build 347
          if(!Lkc.Lcode) {
            // Special case for U_xxxx keys. This vk code will never be used
            // in a keyboard, so we use this to ensure that keystroke processing
            // occurs for the key.
            Lkc.Lcode = 1; 
          }
        }

        // Override key shift state if specified for key in layout (corrected for popup keys KMEW-93)
        keyShiftState = this.getModifierState(e['key'].spec['layer'] || layer);

        // Define modifiers value for sending to keyboard mapping function
        Lkc.Lmodifiers = keyShiftState;

        // Handles modifier states when the OSK is emulating rightalt through the leftctrl-leftalt layer.
        if((Lkc.Lmodifiers & Codes.modifierBitmasks['ALT_GR_SIM']) == Codes.modifierBitmasks['ALT_GR_SIM'] && osk.Layouts.emulatesAltGr()) {
          Lkc.Lmodifiers &= ~Codes.modifierBitmasks['ALT_GR_SIM'];
          Lkc.Lmodifiers |= Codes.modifierCodes['RALT'];
        }

        // Include *limited* support for mnemonic keyboards (Sept 2012)
        // If a touch layout has been defined for a mnemonic keyout, do not perform mnemonic mapping for rules on touch devices.
        if(activeKeyboard && activeKeyboard['KM'] && !(activeKeyboard['KVKL'] && formFactor != 'desktop')) {
          if(Lkc.Lcode != Codes.keyCodes['K_SPACE']) { // exception required, March 2013
            Lkc.vkCode = Lkc.Lcode;
            // So long as the key name isn't prefixed with 'U_', we'll get a default mapping based on the Lcode value.
            // We need to determine the mnemonic base character - for example, SHIFT + K_PERIOD needs to map to '>'.
            var mappedChar: string = keyman.textProcessor.defaultKeyOutput('K_xxxx', Lkc.Lcode, (layer.indexOf('shift') != -1 ? 0x10 : 0), false, null);
            if(mappedChar) {
              Lkc.Lcode = mappedChar.charCodeAt(0);
            } // No 'else' - avoid remapping control + modifier keys!

            if(this.stateKeys['K_CAPS']) {
              if((Lkc.Lcode >= 65 && Lkc.Lcode <= 90) /* 'A' - 'Z' */ || (Lkc.Lcode >= 97 && Lkc.Lcode <= 122) /* 'a' - 'z' */) {
                Lkc.Lmodifiers ^= 0x10; // Flip the 'shift' bit.
                Lkc.Lcode ^= 0x20; // Flips the 'upper' vs 'lower' bit for the base 'a'-'z' ASCII alphabetics.
              }
            }
          }
        } else {
          Lkc.vkCode=Lkc.Lcode;
        }

        // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
        if(typeof activeKeyboard['KM'] == 'undefined') {
          Lkc.Lcode=keyman.keyMapManager._USKeyCodeToCharCode(Lkc);
          Lkc.LisVirtualKey=false;
        }

        // Pass this key code and state to the keyboard program
        if(!activeKeyboard || (Lkc.Lcode != 0 && !kbdInterface.processKeystroke(keyman.util.device, Lelem, Lkc))) {
          // Restore the virtual key code if a mnemonic keyboard is being used
          Lkc.Lcode=Lkc.vkCode;

          // Handle unmapped keys, including special keys
          switch(keyName) {
            case 'K_CAPS':
            case 'K_NUMLOCK':
            case 'K_SCROLL':
              this.stateKeys[keyName] = ! this.stateKeys[keyName];
              com.keyman.singleton.osk._Show();
              break;
            default:
              // The following is physical layout dependent, so should be avoided if possible.  All keys should be mapped.
              var ch = keyman.textProcessor.defaultKeyOutput(keyName,Lkc.Lcode,keyShiftState,true,Lelem);
              if(ch) {
                kbdInterface.output(0, Lelem, ch);
              }
          }
        }

        // Swap layer as appropriate.
        //this.nextLayer = nextLayer; // Is it safe to remove this?
        this.selectLayer(keyName, nextLayer);

        /* I732 END - 13/03/2007 MCD: End Positional Layout support in OSK */
        Lelem._KeymanWebSelectionStart=null;
        Lelem._KeymanWebSelectionEnd=null;
      }
      
      keyman.uiManager.setActivatingUI(false);	// I2498 - KeymanWeb OSK does not accept clicks in FF when using automatic UI
      return true;
    }

    /**
     * Get modifier key state from layer id
     *
     * @param       {string}      layerId       layer id (e.g. ctrlshift)
     * @return      {number}                    modifier key state (desktop keyboards)
     */
    getModifierState(layerId: string): number {
      var modifier=0;
      if(layerId.indexOf('shift') >= 0) {
        modifier |= Codes.modifierCodes['SHIFT'];
      }

      // The chiral checks must not be directly exclusive due each other to visual OSK feedback.
      var ctrlMatched=false;
      if(layerId.indexOf('leftctrl') >= 0) {
        modifier |= Codes.modifierCodes['LCTRL'];
        ctrlMatched=true;
      } 
      if(layerId.indexOf('rightctrl') >= 0) {
        modifier |= Codes.modifierCodes['RCTRL'];
        ctrlMatched=true;
      } 
      if(layerId.indexOf('ctrl')  >= 0 && !ctrlMatched) {
        modifier |= Codes.modifierCodes['CTRL'];
      }

      var altMatched=false;
      if(layerId.indexOf('leftalt') >= 0) {
        modifier |= Codes.modifierCodes['LALT'];
        altMatched=true;
      } 
      if(layerId.indexOf('rightalt') >= 0) {
        modifier |= Codes.modifierCodes['RALT'];
        altMatched=true;
      } 
      if(layerId.indexOf('alt')  >= 0 && !altMatched) {
        modifier |= Codes.modifierCodes['ALT'];
      }

      return modifier;
    }

    /**
     * @summary Look up a custom virtual key code in the virtual key code dictionary KVKD.  On first run, will build the dictionary.
     *
     * `VKDictionary` is constructed from the keyboard's `KVKD` member. This list is constructed 
     * at compile-time and is a list of 'additional' virtual key codes, starting at 256 (i.e. 
     * outside the range of standard virtual key codes). These additional codes are both 
     * `[T_xxx]` and `[U_xxxx]` custom key codes from the Keyman keyboard language. However, 
     * `[U_xxxx]` keys only generate an entry in `KVKD` if there is a corresponding rule that 
     * is associated with them in the keyboard rules. If the `[U_xxxx]` key code is only 
     * referenced as the id of a key in the touch layout, then it does not get an entry in 
     * the `KVKD` property.
     *
     * @private
     * @param       {string}      keyName   custom virtual key code to lookup in the dictionary
     * @return      {number}                key code > 255 on success, or 0 if not found
     */
    getVKDictionaryCode(keyName: string) {
      let keyman = com.keyman.singleton;
      var activeKeyboard = keyman.keyboardManager.activeKeyboard;
      if(!activeKeyboard['VKDictionary']) {
        var a=[];
        if(typeof activeKeyboard['KVKD'] == 'string') {
          // Build the VK dictionary
          // TODO: Move the dictionary build into the compiler -- so compiler generates code such as following.  
          // Makes the VKDictionary member unnecessary.
          //       this.KVKD={"K_ABC":256,"K_DEF":257,...};
          var s=activeKeyboard['KVKD'].split(' ');
          for(var i=0; i<s.length; i++) {
            a[s[i].toUpperCase()]=i+256; // We force upper-case since virtual keys should be case-insensitive.
          }
        }
        activeKeyboard['VKDictionary']=a;
      }

      var res=activeKeyboard['VKDictionary'][keyName.toUpperCase()];
      return res ? res : 0;
    }

    /**
     * Function     _UpdateVKShift
     * Scope        Private
     * @param       {Object}            e     OSK event
     * @param       {number}            v     keyboard shift state
     * @param       {(boolean|number)}  d     set (1) or clear(0) shift state bits
     * @return      {boolean}                 Always true
     * Description  Update the current shift state within KMW
     */
    _UpdateVKShift(e, v: number, d: boolean|number): boolean {
      var keyShiftState=0, lockStates=0, i;
      let keyman = com.keyman.singleton;

      var lockNames  = ['CAPS', 'NUM_LOCK', 'SCROLL_LOCK'];
      var lockKeys   = ['K_CAPS', 'K_NUMLOCK', 'K_SCROLL'];

      if(e) {
        // read shift states from Pevent
        keyShiftState = e.Lmodifiers;
        lockStates = e.Lstates;

        // Are we simulating AltGr?  If it's a simulation and not real, time to un-simulate for the OSK.
        if(keyman.keyboardManager.isChiral() && osk.Layouts.emulatesAltGr() && 
            (com.keyman.DOMEventHandlers.states.modStateFlags & Codes.modifierBitmasks['ALT_GR_SIM']) == Codes.modifierBitmasks['ALT_GR_SIM']) {
          keyShiftState |= Codes.modifierBitmasks['ALT_GR_SIM'];
          keyShiftState &= ~Codes.modifierCodes['RALT'];
        }

        for(i=0; i < lockNames.length; i++) {
          if(lockStates & Codes.stateBitmasks[lockNames[i]]) {
            this.stateKeys[lockKeys[i]] = lockStates & Codes.modifierCodes[lockNames[i]];
          }
        }
      } else if(d) {
        keyShiftState |= v;

        for(i=0; i < lockNames.length; i++) {
          if(v & Codes.stateBitmasks[lockNames[i]]) {
            this.stateKeys[lockKeys[i]] = true;
          }
        }
      } else {
        keyShiftState &= ~v;

        for(i=0; i < lockNames.length; i++) {
          if(v & Codes.stateBitmasks[lockNames[i]]) {
            this.stateKeys[lockKeys[i]] = false;
          }
        }
      }

      // Find and display the selected OSK layer
      if(keyman['osk'].vkbd) {
        keyman['osk'].vkbd.showLayer(this.getLayerId(keyShiftState));
      }

      // osk._UpdateVKShiftStyle will be called automatically upon the next _Show.
      if(keyman.osk._Visible) {
        keyman.osk._Show();
      }

      return true;
    }

    getLayerId(modifier: number): string {
      return osk.Layouts.getLayerId(modifier);
    }

    /**
     * Select the OSK's next keyboard layer based upon layer switching keys as a default
     * The next layer will be determined from the key name unless otherwise specifed
     *
     *  @param  {string}                    keyName     key identifier
     *  @param  {number|string|undefined}   nextLayerIn optional next layer identifier
     *  @return {boolean}                               return true if keyboard layer changed
     */
    selectLayer(keyName: string, nextLayerIn?: number | string): boolean {
      var nextLayer = arguments.length < 2 ? null : nextLayerIn;
      let keyman = com.keyman.singleton;
      var isChiral = keyman.keyboardManager.isChiral();

      // Layer must be identified by name, not number (27/08/2015)
      if(typeof nextLayer == 'number') {
        nextLayer = this.getLayerId(nextLayer * 0x10);
      }

      // Identify next layer, if required by key
      if(!nextLayer) {
        switch(keyName) {
          case 'K_LSHIFT':
          case 'K_RSHIFT':
          case 'K_SHIFT':
            nextLayer = 'shift';
            break;
          case 'K_LCONTROL':
          case 'K_LCTRL':
            if(isChiral) {
              nextLayer = 'leftctrl';
              break;
            }
          case 'K_RCONTROL':
          case 'K_RCTRL':
            if(isChiral) {
              nextLayer = 'rightctrl';
              break;
            }
          case 'K_CTRL':
            nextLayer = 'ctrl';
            break;
          case 'K_LMENU':
          case 'K_LALT':
            if(isChiral) {
              nextLayer = 'leftalt';
              break;
            }
          case 'K_RMENU':
          case 'K_RALT':
            if(isChiral) {
              nextLayer = 'rightalt';
              break;
            }
          case 'K_ALT':
            nextLayer = 'alt';
            break;
          case 'K_ALTGR':
            if(isChiral) {
              nextLayer = 'leftctrl-rightalt';
            } else {
              nextLayer = 'ctrl-alt';
            }
            break;
          case 'K_CURRENCIES':
          case 'K_NUMERALS':
          case 'K_SHIFTED':
          case 'K_UPPER':
          case 'K_LOWER':
          case 'K_SYMBOLS':
            nextLayer = 'default';
            break;
        }
      }

      // If no key corresponding to a layer transition is pressed, maintain the current layer.
      if(!nextLayer) {
        return false;
      }

      // Change layer and refresh OSK
      this.updateLayer(nextLayer);
      com.keyman.singleton.osk._Show();

      return true;
    }

    /**
     * Sets the new layer id, allowing for toggling shift/ctrl/alt while preserving the remainder
     * of the modifiers represented by the current layer id (where applicable)
     *
     * @param       {string}      id      layer id (e.g. ctrlshift)
     */
    updateLayer(id: string) {
      let keyman = com.keyman.singleton;
      let processor = keyman.textProcessor;
      let vkbd = keyman['osk'].vkbd;

      if(!vkbd) {
        return;
      }

      let activeLayer = vkbd.layerId;
      var s = activeLayer;

      // Do not change layer unless needed (27/08/2015)
      if(id == activeLayer && keyman.util.device.formFactor != 'desktop') {
        return false;
      }

      var idx=id;
      var i;

      if(keyman.util.device.formFactor == 'desktop') {
        // Need to test if target layer is a standard layer (based on the plain 'default')
        var replacements= ['leftctrl', 'rightctrl', 'ctrl', 'leftalt', 'rightalt', 'alt', 'shift'];

        for(i=0; i < replacements.length; i++) {
          // Don't forget to remove the kebab-case hyphens!
          idx=idx.replace(replacements[i] + '-', '');
          idx=idx.replace(replacements[i],'');
        }

        // If we are presently on the default layer, drop the 'default' and go straight to the shifted mode.
        // If on a common symbolic layer, drop out of symbolic mode and go straight to the shifted mode.
        if(activeLayer == 'default' || activeLayer == 'numeric' || activeLayer == 'symbol' || activeLayer == 'currency' || idx != '') {
          s = id;
        }
        // Otherwise, we are based upon a layer that accepts modifier variations.
        // Modify the layer according to the current state and key pressed.
        //
        // TODO:  Consider:  should this ever be allowed for a base layer other than 'default'?  If not,
        // if(idx == '') with accompanying if-else structural shift would be a far better test here.
        else {
          // Save our current modifier state.
          var modifier=processor.getModifierState(s);

          // Strip down to the base modifiable layer.
          for(i=0; i < replacements.length; i++) {
            // Don't forget to remove the kebab-case hyphens!
            s=s.replace(replacements[i] + '-', '');
            s=s.replace(replacements[i],'');
          }

          // Toggle the modifier represented by our input argument.
          switch(id) {
            case 'shift':
              modifier ^= Codes.modifierCodes['SHIFT'];
              break;
            case 'leftctrl':
              modifier ^= Codes.modifierCodes['LCTRL'];
              break;
            case 'rightctrl':
              modifier ^= Codes.modifierCodes['RCTRL'];
              break;
            case 'ctrl':
              modifier ^= Codes.modifierCodes['CTRL'];
              break;
            case 'leftalt':
              modifier ^= Codes.modifierCodes['LALT'];
              break;
            case 'rightalt':
              modifier ^= Codes.modifierCodes['RALT'];
              break;
            case 'alt':
              modifier ^= Codes.modifierCodes['ALT'];
              break;
            default:
              s = id;
          }

          // Combine our base modifiable layer and attach the new modifier variation info to obtain our destination layer.
          if(s != 'default') {
            if(s == '') {
              s = this.getLayerId(modifier);
            } else {
              s = this.getLayerId(modifier) + '-' + s;
            }
          }
        }
        
        if(s == '') {
          s = 'default';
        }
      } else {
        // Mobile form-factor.  Either the layout is specified by a keyboard developer with direct layer name references
        // or all layers are accessed via subkey of a single layer-shifting key - no need for modifier-combining logic.
        s = id;
      }

      // Actually set the new layer id.
      if(vkbd) {
        if(!vkbd.showLayer(s)) {
          vkbd.showLayer('default');
        }
      }
    }
  }
}