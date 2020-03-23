// Establishes key-code definitions.
/// <reference path="codes.ts" />
// Defines our generalized "KeyEvent" class.
/// <reference path="keyEvent.ts" />

namespace com.keyman.text {
  export class LegacyKeyEvent {
    Ltarg: HTMLElement;
    Lcode: number;
    Lmodifiers: number;
    LisVirtualKey: number;
  }

  export class Processor {

    // Tracks the simulated value for supported state keys, allowing the OSK to mirror a physical keyboard for them.
    // Using the exact keyCode name from the Codes definitions will allow for certain optimizations elsewhere in the code.
    stateKeys = {
      "K_CAPS":false,
      "K_NUMLOCK":false,
      "K_SCROLL":false
    };

    // Tracks the most recent modifier state information in order to quickly detect changes
    // in keyboard state not otherwise captured by the hosting page in the browser.
    // Needed for AltGr simulation.
    modStateFlags: number = 0;
    // Denotes whether or not KMW needs to 'swallow' the next keypress.
    swallowKeypress: boolean = false;


    /**
     * Get the default key string. If keyName is U_xxxxxx, use that Unicode codepoint.
     * Otherwise, lookup the  virtual key code (physical keyboard mapping)
     *
     * @param   {object}  Lkc  The pre-analyzed key event object
     * @param   {number}  keyShiftState
     * @param   {boolean} usingOSK
     * @return  {string}
     */
    defaultKeyOutput(Lkc: KeyEvent, keyShiftState: number, usingOSK: boolean): string {
      let keyName = Lkc.kName;
      let n = Lkc.Lcode;
      let outputTarget = Lkc.Ltarg;

      let keyman = com.keyman.singleton;
      let domManager = keyman.domManager;
      let activeKeyboard = keyman.keyboardManager.activeKeyboard;

      let quiet = outputTarget instanceof Mock;

      var ch = '', checkCodes = false;
      // check if exact match to SHIFT's code.  Only the 'default' and 'shift' layers should have default key outputs.
      if(keyShiftState == 0) {
        checkCodes = true;
      } else if (keyShiftState == Codes.modifierCodes['SHIFT']) {
        checkCodes = true; 
        keyShiftState = 1; // It's used as an index.
      } else {
        if(!quiet) {
          console.warn("KMW only defines default key output for the 'default' and 'shift' layers!");
        }
      }

      // If this was triggered by the OSK -or- if it was triggered by a 'synthetic' OutputTarget (TouchAlias, Mock)
      // that lacks default key processing behavior.
      if(usingOSK || outputTarget.isSynthetic) {
        var code = Codes.keyCodes[keyName];
        if(!code) {
          code = n;
        }

        switch(code) {
          case Codes.keyCodes['K_BKSP']:  //Only desktop UI, not touch devices. TODO: add repeat while mouse down for desktop UI
            if(quiet) {
              // TODO:  Remove need for this clause via refactoring.  It's currently needed for predictive text Mocks.
              return '\b'; // the escape sequence for backspace.
            } else {
              keyman.interface.defaultBackspace(outputTarget);
            }
            return '';
          case Codes.keyCodes['K_TAB']:
            if(!quiet) {
              domManager.moveToNext(keyShiftState);
            }
            break;
          case Codes.keyCodes['K_TABBACK']:
            if(!quiet) {
              domManager.moveToNext(true);
            }
            break;
          case Codes.keyCodes['K_TABFWD']:
            if(!quiet) {
              domManager.moveToNext(false);
            }
            break;
          case Codes.keyCodes['K_ENTER']:
            outputTarget.handleNewlineAtCaret();

            return '\n';  // We still return this, as it ensures we generate a rule-match.
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

        // Only desktop UI, not touch devices. TODO: add repeat while mouse down for desktop UI
        //
        // Can easily occur from mnemonic keyboards, which create synthetic events without
        // the appropriate kName value.
        //
        // Not strictly if `Lkc.vkCode` is properly maintained, but it's good to have an
        // extra safety; this would have blocked the backspace bug as well.
      } else if(Lkc.Lcode == 8) {
        keyman.interface.defaultBackspace();
        return '';
      }

      // Translate numpad keystrokes into their non-numpad equivalents
      if(Lkc.Lcode >= Codes.keyCodes["K_NP0"]  &&  Lkc.Lcode <= Codes.keyCodes["K_NPSLASH"] && activeKeyboard && !activeKeyboard['KM']) {
        // Number pad, numlock on
        if(Lkc.Lcode < 106) {
          var Lch = Lkc.Lcode-48;
        } else {
          Lch = Lkc.Lcode-64;
        }
        ch = String._kmwFromCharCode(Lch); //I3319
        return ch;
      }

      // TODO:  Refactor the overloading of the 'n' parameter here into separate methods.

      // Test for fall back to U_xxxxxx key id
      // For this first test, we ignore the keyCode and use the keyName
      if(keyName && keyName.substr(0,2) == 'U_') {
        var codePoint = parseInt(keyName.substr(2,6), 16);
        if (((0x0 <= codePoint) && (codePoint <= 0x1F)) || ((0x80 <= codePoint) && (codePoint <= 0x9F))) {
          // Code points [U_0000 - U_001F] and [U_0080 - U_009F] refer to Unicode C0 and C1 control codes.
          // Check the codePoint number and do not allow output of these codes via U_xxxxxx shortcuts.
          if(!quiet) {
            console.log("Suppressing Unicode control code: U_00" + codePoint.toString(16));
          }
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
          if(!quiet) {
            console.error("Error detected with default mapping for key:  code = " + n + ", shift state = " + (keyShiftState == 1 ? 'shift' : 'default'));
          }
        }
      }
      return ch;
    }

    static getOutputTarget(Lelem?: HTMLElement): OutputTarget {
      let keyman = com.keyman.singleton;

      if(!Lelem && !keyman.isHeadless) {
        Lelem = keyman.domManager.getLastActiveElement();
        if(!Lelem) {
          // If we're trying to find an active target but one doesn't exist, just return null.
          return null;
        }
      }

      // If we were provided an element or found an active element but it's improperly attached, that should cause an error.
      if(Lelem._kmwAttachment && Lelem._kmwAttachment.interface) {
        return Lelem._kmwAttachment.interface;
      } else {
        throw new Error("OSK could not find element output target data!");
      }
    }

    _GetClickEventProperties(e: osk.ActiveKey, Lelem: HTMLElement): KeyEvent {
      let keyman = com.keyman.singleton;

      var activeKeyboard = keyman.keyboardManager.activeKeyboard;
      let formFactor = keyman.util.device.formFactor;

      // Get key name and keyboard shift state (needed only for default layouts and physical keyboard handling)
      // Note - virtual keys should be treated case-insensitive, so we force uppercasing here.
      var layer = e.layer || e.displayLayer || '', keyName=e.id.toUpperCase();

      // Start:  mirrors _GetKeyEventProperties

      // Override key shift state if specified for key in layout (corrected for popup keys KMEW-93)
      var keyShiftState = this.getModifierState(layer);

      // First check the virtual key, and process shift, control, alt or function keys
      var Lkc: KeyEvent = {
        Ltarg: Processor.getOutputTarget(Lelem),
        Lmodifiers: keyShiftState,
        Lstates: 0,
        Lcode: Codes.keyCodes[keyName],
        LisVirtualKey: true,
        vkCode: 0,
        kName: keyName,
        kLayer: layer,
        kbdLayer: e.displayLayer,
        kNextLayer: e.nextlayer
      };

      // If it's actually a state key modifier, trigger its effects immediately, as KeyboardEvents would do the same.
      switch(keyName) {
        case 'K_CAPS':
        case 'K_NUMLOCK':
        case 'K_SCROLL':
          this.stateKeys[keyName] = ! this.stateKeys[keyName];
      }

      // Performs common pre-analysis for both 'native' and 'embedded' OSK key & subkey input events.
      this.commonClickEventPreprocessing(Lkc);

      // End - mirrors _GetKeyEventProperties

      // Include *limited* support for mnemonic keyboards (Sept 2012)
      // If a touch layout has been defined for a mnemonic keyout, do not perform mnemonic mapping for rules on touch devices.
      if(activeKeyboard && activeKeyboard['KM'] && !(activeKeyboard['KVKL'] && formFactor != 'desktop')) {
        if(Lkc.Lcode != Codes.keyCodes['K_SPACE']) { // exception required, March 2013
          // Jan 2019 - interesting that 'K_SPACE' also affects the caps-state check...
          Lkc.vkCode = Lkc.Lcode;
          this.setMnemonicCode(Lkc, layer.indexOf('shift') != -1, this.stateKeys['K_CAPS']);
        }
      } else {
        Lkc.vkCode=Lkc.Lcode;
      }

      // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
      if(typeof activeKeyboard['KM'] == 'undefined') {
        Lkc.Lcode=keyman.keyMapManager._USKeyCodeToCharCode(Lkc);
        Lkc.LisVirtualKey=false;
      }

      return Lkc;
    }

    commonClickEventPreprocessing(Lkc: KeyEvent) {
      // Set the flags for the state keys.
      Lkc.Lstates |= this.stateKeys['K_CAPS']    ? Codes.modifierCodes['CAPS'] : Codes.modifierCodes['NO_CAPS'];
      Lkc.Lstates |= this.stateKeys['K_NUMLOCK'] ? Codes.modifierCodes['NUM_LOCK'] : Codes.modifierCodes['NO_NUM_LOCK'];
      Lkc.Lstates |= this.stateKeys['K_SCROLL']  ? Codes.modifierCodes['SCROLL_LOCK'] : Codes.modifierCodes['NO_SCROLL_LOCK'];

      // Set LisVirtualKey to false to ensure that nomatch rule does fire for U_xxxx keys
      if(Lkc.kName.substr(0,2) == 'U_') {
        Lkc.LisVirtualKey=false;
      }

      // Get code for non-physical keys (T_KOKAI, U_05AB etc)
      if(typeof Lkc.Lcode == 'undefined') {
        Lkc.Lcode = this.getVKDictionaryCode(Lkc.kName);// Updated for Build 347
        if(!Lkc.Lcode) {
          // Special case for U_xxxx keys. This vk code will never be used
          // in a keyboard, so we use this to ensure that keystroke processing
          // occurs for the key.
          Lkc.Lcode = 1; 
        }
      }

      // Handles modifier states when the OSK is emulating rightalt through the leftctrl-leftalt layer.
      if((Lkc.Lmodifiers & Codes.modifierBitmasks['ALT_GR_SIM']) == Codes.modifierBitmasks['ALT_GR_SIM'] && osk.Layouts.emulatesAltGr()) {
        Lkc.Lmodifiers &= ~Codes.modifierBitmasks['ALT_GR_SIM'];
        Lkc.Lmodifiers |= Codes.modifierCodes['RALT'];
      }
    }

    processKeystroke(keyEvent: KeyEvent, outputTarget: OutputTarget, fromOSK: boolean): RuleBehavior {
      let keyman = com.keyman.singleton;

      var activeKeyboard = keyman.keyboardManager.activeKeyboard;
      let kbdInterface = keyman.interface;
      let keyMapManager = keyman.keyMapManager;

      if(!keyman.isEmbedded && !fromOSK && keyman.util.device.browser == 'firefox') {
        // I1466 - Convert the - keycode on mnemonic as well as positional layouts
        // FireFox, Mozilla Suite
        if(keyMapManager.browserMap.FF['k'+keyEvent.Lcode]) {
          keyEvent.Lcode=keyMapManager.browserMap.FF['k'+keyEvent.Lcode];
        }
      }

      var matchBehavior: RuleBehavior;
      this.swallowKeypress = false;

      // Pass this key code and state to the keyboard program
      if(activeKeyboard && keyEvent.Lcode != 0) {
        matchBehavior = kbdInterface.processKeystroke(fromOSK ? keyman.util.device : keyman.util.physicalDevice, outputTarget, keyEvent);
      }

      if(!matchBehavior) {
        // Restore the virtual key code if a mnemonic keyboard is being used
        // If no vkCode value was stored, maintain the original Lcode value.
        keyEvent.Lcode=keyEvent.vkCode || keyEvent.Lcode;

        // Handle unmapped keys, including special keys
        // The following is physical layout dependent, so should be avoided if possible.  All keys should be mapped.
        kbdInterface.activeTargetOutput = outputTarget;
        let preInput = Mock.from(outputTarget);

        var ch = this.defaultKeyOutput(keyEvent, keyEvent.Lmodifiers, fromOSK);
        kbdInterface.activeTargetOutput = null;
        if(ch) {
          if(ch == '\b') { // Is only returned when disableDOM is true, which prevents automatic default backspace application.
            // defaultKeyOutput can't always find the outputTarget if we're working with alternates!
            kbdInterface.defaultBackspace(outputTarget);
          } else if(ch != '\n') { // \n is handled automatically now.
            kbdInterface.output(0, outputTarget, ch);
          }
          matchBehavior = new RuleBehavior();
          matchBehavior.transcription = outputTarget.buildTranscriptionFrom(preInput, keyEvent);
        } else if(keyEvent.Lcode == 8) { // Backspace
          matchBehavior = new RuleBehavior();
          matchBehavior.transcription = outputTarget.buildTranscriptionFrom(preInput, keyEvent);
        }
      }

      return matchBehavior;
    }

    /**
     * Simulate a keystroke according to the touched keyboard button element
     *
     * Handles default output and keyboard processing for both OSK and physical keystrokes.
     * 
     * @param       {Object}      e      The abstracted KeyEvent to use for keystroke processing
     */
    processKeyEvent(keyEvent: KeyEvent, e?: osk.KeyElement | boolean): boolean {
      let keyman = com.keyman.singleton;

      // Determine the current target for text output and create a "mock" backup
      // of its current, pre-input state.
      let outputTarget = keyEvent.Ltarg;

      let fromOSK = !!e; // If specified, it's from the OSK.

      // Enables embedded-path OSK sourcing detection.
      if(typeof e == 'boolean') {
        e = null as osk.KeyElement; // Cast is necessary for TS type-checking later in the method.
      }

      let formFactor = keyman.util.device.formFactor;
      let keyMapManager = keyman.keyMapManager;

      this.swallowKeypress = false;

      if(fromOSK && !keyman.isEmbedded) {
        keyman.domManager.initActiveElement(keyEvent.Ltarg.getElement());

        // Turn off key highlighting (or preview)
        keyman['osk'].vkbd.highlightKey(e,false);
      }

      // Exclude menu and OSK hide keys from normal click processing
      if(keyEvent.kName == 'K_LOPT' || keyEvent.kName == 'K_ROPT') {
        keyman['osk'].vkbd.optionKey(e, keyEvent.kName, true);
        return true;
      }

      // The default OSK layout for desktop devices does not include nextlayer info, relying on modifier detection here.
      // It's the OSK equivalent to doModifierPress on 'desktop' form factors.
      if((formFactor == 'desktop' || keyman.keyboardManager.layoutIsDesktopBased()) && fromOSK) {
        // If it's a desktop OSK style and this triggers a layer change,
        // a modifier key was clicked.  No output expected, so it's safe to instantly exit.
        if(this.selectLayer(keyEvent.kName, keyEvent.kNextLayer)) {
          return true;
        }
      }

      // Will handle keystroke-based non-layer change modifier & state keys, mapping them through the physical keyboard's version
      // of state management.
      if(!fromOSK && this.doModifierPress(keyEvent, !fromOSK)) {
        return true;
      }

      if(!keyman.isEmbedded && !fromOSK && keyman.util.device.browser == 'firefox') {
        // I1466 - Convert the - keycode on mnemonic as well as positional layouts
        // FireFox, Mozilla Suite
        if(keyMapManager.browserMap.FF['k'+keyEvent.Lcode]) {
          keyEvent.Lcode=keyMapManager.browserMap.FF['k'+keyEvent.Lcode];
        }
      } //else 
      //{
      // Safari, IE, Opera?
      //}

      // If suggestions exist AND space is pressed, accept the suggestion and do not process the keystroke.
      // If a suggestion was just accepted AND backspace is pressed, revert the change and do not process the backspace.
      // We check the first condition here, while the prediction UI handles the second through the try__() methods below.
      if(keyman.modelManager.enabled) {
        // The following code relies on JS's logical operator "short-circuit" properties to prevent unwanted triggering of the second condition.

        // Can the suggestion UI revert a recent suggestion?  If so, do that and swallow the backspace.
        if((keyEvent.kName == "K_BKSP" || keyEvent.Lcode == Codes.keyCodes["K_BKSP"]) && keyman.modelManager.tryRevertSuggestion()) {
          return;
          // Can the suggestion UI accept an existing suggestion?  If so, do that and swallow the space character.
        } else if((keyEvent.kName == "K_SPACE" || keyEvent.Lcode == Codes.keyCodes["K_SPACE"]) && keyman.modelManager.tryAcceptSuggestion('space')) {
          return;
        }
      }

      if(fromOSK && !keyman.isEmbedded) {
        keyman.uiManager.setActivatingUI(true);
        com.keyman.DOMEventHandlers.states._IgnoreNextSelChange = 100;
        keyman.domManager.focusLastActiveElement();
        com.keyman.DOMEventHandlers.states._IgnoreNextSelChange = 0;
      }
      // // ...end I3363 (Build 301)

      let preInputMock = Mock.from(outputTarget);

      let ruleBehavior = this.processKeystroke(keyEvent, outputTarget, fromOSK);

      // Swap layer as appropriate.
      if(keyEvent.kNextLayer) {
        this.selectLayer(keyEvent.kName, keyEvent.kNextLayer);
      }
      
      // Should we swallow any further processing of keystroke events for this keydown-keypress sequence?
      if(ruleBehavior != null) {
        let alternates: Alternate[];

        // Note - we don't yet do fat-fingering with longpress keys.
        if(keyEvent.keyDistribution && keyEvent.kbdLayer) {
          let activeLayout = keyman['osk'].vkbd.layout as osk.ActiveLayout;
          alternates = [];
  
          for(let pair of keyEvent.keyDistribution) {
            let mock = Mock.from(preInputMock);
            
            let altKey = activeLayout.getLayer(keyEvent.kbdLayer).getKey(pair.keyId);
            if(!altKey) {
              console.warn("Potential fat-finger key could not be found in layer!");
              continue;
            }

            let altEvent = this._GetClickEventProperties(altKey, keyEvent.Ltarg.getElement());
            altEvent.Ltarg = mock;
            let alternateBehavior = this.processKeystroke(altEvent, mock, fromOSK);
            if(alternateBehavior) {
              // TODO: if alternateBehavior.beep == true, set 'p' to 0.  It's a disallowed key sequence,
              //       so a user should never have intended to type it.  Should probably renormalize 
              //       the distribution afterward, though...
              
              let transform: Transform = alternateBehavior.transcription.transform;
              
              // Ensure that the alternate's token id matches that of the current keystroke, as we only
              // record the matched rule's context (since they match)
              transform.id = ruleBehavior.transcription.token;
              alternates.push({sample: transform, 'p': pair.p});
            }
          }
        }

        if(ruleBehavior.beep) {
          // TODO:  Must be relocated further 'out' to complete the full, planned web-core refactor.
          //        We're still referencing the DOM, even if only the manager object.  (It's an improvement, at least.)
          keyman.domManager.doBeep(outputTarget);
        }

        for(let storeID in ruleBehavior.setStore) {
          // TODO:  Must be relocated further 'out' to complete the full, planned web-core refactor.
          //        `Processor` shouldn't be directly setting anything on the OSK when the refactor is complete.
          
          // How would this be handled in an eventual headless mode?
          switch(Number.parseInt(storeID)) { // Because the number was converted into a String for 'dictionary' use.
            case KeyboardInterface.TSS_LAYER:
              keyman.osk.vkbd.showLayer(ruleBehavior.setStore[storeID]); //Build 350, osk reference now OK, so should work
              break;
            case KeyboardInterface.TSS_PLATFORM:
              console.error("Rule attempted to perform illegal operation - 'platform' may not be changed.");
              break;
            default:
              console.warn("Unknown store affected by keyboard rule: " + storeID);
          }
        }

        // If the transform isn't empty, we've changed text - which should produce a 'changed' event in the DOM.
        //
        // TODO:  This check should be done IN a dom module, not here in web-core space.  This place is closer 
        //        to that goal than it previously was, at least.
        let ruleTransform = ruleBehavior.transcription.transform;
        if(ruleTransform.insert != "" || ruleTransform.deleteLeft > 0 || ruleTransform.deleteRight > 0) {
          if(outputTarget.getElement() == DOMEventHandlers.states.activeElement) {
            DOMEventHandlers.states.changed = true;
          }
        }

        // -- All keystroke (and 'alternate') processing is now complete.  Time to finalize everything! --
        
        // Notify the ModelManager of new input - it's predictive text time!
        ruleBehavior.transcription.alternates = alternates;
        keyman.modelManager.predict(ruleBehavior.transcription);

        // KMEA and KMEI (embedded mode) use direct insertion of the character string
        if(keyman.isEmbedded) {
          // A special embedded callback used to setup direct callbacks to app-native code.
          keyman['oninserttext'](ruleTransform.deleteLeft, ruleTransform.insert, ruleTransform.deleteRight);
        }

        // Since this method now performs changes for 'default' keystrokes, synthetic 'change' event generation
        // belongs here, rather than only in interface.processKeystroke() as in versions pre-12.
        if(outputTarget.getElement()) {
          keyman['interface'].doInputEvent(outputTarget.getElement());
        }

        this.swallowKeypress = (e && keyEvent.Lcode != 8 ? keyEvent.Lcode != 0 : false);
        if(keyEvent.Lcode == 8) {
          this.swallowKeypress = false;
        }
        return false;
      } else {
        this.swallowKeypress = false;
      }

      /* I732 END - 13/03/2007 MCD: End Positional Layout support in OSK */
      
      if(fromOSK && !keyman.isEmbedded) {
        keyman.uiManager.setActivatingUI(false);	// I2498 - KeymanWeb OSK does not accept clicks in FF when using automatic UI
      }

      // Special case for embedded to pass K_TAB back to device to process
      if(keyman.isEmbedded && (keyEvent.Lcode == Codes.keyCodes["K_TAB"] ||
          keyEvent.Lcode == Codes.keyCodes["K_TABBACK"] || keyEvent.Lcode == Codes.keyCodes["K_TABFWD"])) {
        return false;
      }

      // TODO:  rework the return value to be `ruleBehavior` instead.  Functions that call this one are
      //        the ones that should worry about event handler returns, etc.  Not this one.
      //
      //        They should also be the ones to handle the TODOs seen earlier in this function -
      //        once THOSE are properly relocated.  (They're too DOM-heavy to remain in web-core.)

      // Only return true (for the eventual event handler's return value) if we didn't match a rule.
      return ruleBehavior == null;
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
    clickKey(e: osk.KeyElement, touch?: Touch, layerId?: string, keyDistribution?: KeyDistribution) {
      let keyman = com.keyman.singleton;
      var Lelem = keyman.domManager.getLastActiveElement();

      if(Lelem != null) {
        // Handle any DOM state management related to click inputs.
        let outputTarget = Processor.getOutputTarget(Lelem);
        keyman.domManager.initActiveElement(Lelem);
  
        // Turn off key highlighting (or preview)
        keyman['osk'].vkbd.highlightKey(e,false);
        
        // Clear any cached codepoint data; we can rebuild it if it's unchanged.
        outputTarget.invalidateSelection();
        // Deadkey matching continues to be troublesome.
        // Deleting matched deadkeys here seems to correct some of the issues.   (JD 6/6/14)
        outputTarget.deadkeys().deleteMatched();      // Delete any matched deadkeys before continuing
  
        let Lkc = this._GetClickEventProperties(e['key'].spec as osk.ActiveKey, Lelem);
        if(keyman.modelManager.enabled) {
          Lkc.source = touch;
          Lkc.keyDistribution = keyDistribution;
        }
        return this.processKeyEvent(Lkc, e);
      } else {
        return true;
      }
    }

    // FIXME:  makes some bad assumptions.
    setMnemonicCode(Lkc: KeyEvent, shifted: boolean, capsActive: boolean) {
      // K_SPACE is not handled by defaultKeyOutput for physical keystrokes unless using touch-aliased elements.
      // It's also a "exception required, March 2013" for clickKey, so at least they both have this requirement.
      if(Lkc.Lcode != Codes.keyCodes['K_SPACE']) {
        // So long as the key name isn't prefixed with 'U_', we'll get a default mapping based on the Lcode value.
        // We need to determine the mnemonic base character - for example, SHIFT + K_PERIOD needs to map to '>'.
        let mappingEvent: KeyEvent = new KeyEvent();
        for(var key in Lkc) {
          mappingEvent[key] = Lkc[key];
        }
        
        // To facilitate storing relevant commands, we should probably reverse-lookup
        // the actual keyname instead.
        mappingEvent.kName = 'K_xxxx';
        mappingEvent.Ltarg = new Mock(); // helps prevent breakage for mnemonics.
        var mappedChar: string = this.defaultKeyOutput(mappingEvent, (shifted ? 0x10 : 0), false);
        
        /* First, save a backup of the original code.  This one won't needlessly trigger keyboard
         * rules, but allows us to replicate/emulate commands after rule processing if needed.
         * (Like backspaces)
         */
        Lkc.vkCode = Lkc.Lcode;
        if(mappedChar) {
          // Will return 96 for 'a', which is a keycode corresponding to Codes.keyCodes('K_NP1') - a numpad key.
          // That stated, we're in mnemonic mode - this keyboard's rules are based on the char codes.
          Lkc.Lcode = mappedChar.charCodeAt(0);
        } else {
          // Don't let command-type keys (like K_DEL, which will output '.' otherwise!)
          // trigger keyboard rules.
          delete Lkc.Lcode;
        }
      }

      if(capsActive) {
        // TODO:  Needs fixing - does not properly mirror physical keystrokes, as Lcode range 96-111 corresponds
        // to numpad keys!  (Physical keyboard section has its own issues here.)
        if((Lkc.Lcode >= 65 && Lkc.Lcode <= 90) /* 'A' - 'Z' */ || (Lkc.Lcode >= 97 && Lkc.Lcode <= 122) /* 'a' - 'z' */) {
          Lkc.Lmodifiers ^= 0x10;  // Flip the 'shifted' bit, so it'll act as the opposite key.
          Lkc.Lcode ^= 0x20; // Flips the 'upper' vs 'lower' bit for the base 'a'-'z' ASCII alphabetics.
        }
      }
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
     * Description  Updates the current shift state within KMW, updating the OSK's visualization thereof.
     */
    _UpdateVKShift(e: KeyEvent, v: number, d: boolean|number): boolean {
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
            (this.modStateFlags & Codes.modifierBitmasks['ALT_GR_SIM']) == Codes.modifierBitmasks['ALT_GR_SIM']) {
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
          var modifier=this.getModifierState(s);

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

    /**
     * Function     _GetEventKeyCode
     * Scope        Private
     * @param       {Event}       e         Event object
     * Description  Finds the key code represented by the event.
     */
    _GetEventKeyCode(e: KeyboardEvent) {
      if (e.keyCode) {
        return e.keyCode;
      } else if (e.which) {
        return e.which;
      } else {
        return null;
      }
    }

    // Returns true if the key event is a modifier press, allowing keyPress to return selectively
    // in those cases.
    private doModifierPress(Levent: KeyEvent, isKeyDown: boolean): boolean {
      let keyman = com.keyman.singleton;
      let outputTarget = Levent.Ltarg;

      switch(Levent.Lcode) {
        case 8: 
          outputTarget.deadkeys().clear();
          break; // I3318 (always clear deadkeys after backspace) 
        case 16: //"K_SHIFT":16,"K_CONTROL":17,"K_ALT":18
        case 17: 
        case 18: 
        case 20: //"K_CAPS":20, "K_NUMLOCK":144,"K_SCROLL":145
        case 144:
        case 145:
          // For eventual integration - we bypass an OSK update for physical keystrokes when in touch mode.
          keyman['interface'].notifyKeyboard(Levent.Lcode, outputTarget, isKeyDown ? 1 : 0); 
          if(!keyman.util.device.touchable) {
            return this._UpdateVKShift(Levent, Levent.Lcode-15, 1); // I2187
          } else {
            return true;
          }
      }

      if(Levent.LmodifierChange) {
        keyman['interface'].notifyKeyboard(0, outputTarget, 1); 
        this._UpdateVKShift(Levent, 0, 1);
      }

      // No modifier keypresses detected.
      return false;
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
    _GetKeyEventProperties(e: KeyboardEvent, keyState?: boolean): KeyEvent {
      let keyman = com.keyman.singleton;
      var s = new KeyEvent();

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
      s.Ltarg = Processor.getOutputTarget(target);

      s.Lcode = this._GetEventKeyCode(e);
      if (s.Lcode == null) {
        return null;
      }

      // Stage 1 - track the true state of the keyboard's modifiers.
      var prevModState = this.modStateFlags, curModState = 0x0000;
      var ctrlEvent = false, altEvent = false;
      
      let keyCodes = Codes.keyCodes;
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

      let modifierCodes = Codes.modifierCodes;
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
      s.LmodifierChange = this.modStateFlags != curModState;
      this.modStateFlags = curModState;

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

      let modifierBitmasks = Codes.modifierBitmasks;
      // Stage 4 - map the modifier set to the appropriate keystroke's modifiers.
      if(keyman.keyboardManager.isChiral()) {
        s.Lmodifiers = curModState & modifierBitmasks.CHIRAL;

        // Note for future - embedding a kill switch here or in keymanweb.osk.emulatesAltGr would facilitate disabling
        // AltGr / Right-alt simulation.
        if(osk.Layouts.emulatesAltGr() && (s.Lmodifiers & modifierBitmasks['ALT_GR_SIM']) == modifierBitmasks['ALT_GR_SIM']) {
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
      var activeKeyboard = keyman.keyboardManager.activeKeyboard;

      if(activeKeyboard && activeKeyboard['KM']) {
        // The following will never set a code corresponding to a modifier key, so it's fine to do this,
        // which may change the value of Lcode, here.
        this.setMnemonicCode(s, e.getModifierState("Shift"), e.getModifierState("CapsLock"));
      }

      // The 0x6F used to be 0x60 - this adjustment now includes the chiral alt and ctrl modifiers in that check.
      var LisVirtualKeyCode = (typeof e.charCode != 'undefined' && e.charCode != null  &&  (e.charCode == 0 || (s.Lmodifiers & 0x6F) != 0));
      s.LisVirtualKey = LisVirtualKeyCode || e.type != 'keypress';

      let keyMapManager = keyman.keyMapManager;

      // Other minor physical-keyboard adjustments
      if(activeKeyboard && !activeKeyboard['KM']) {
        // Positional Layout

        /* 13/03/2007 MCD: Swedish: Start mapping of keystroke to US keyboard */
        var Lbase=keyMapManager.languageMap[com.keyman.osk.Layouts._BaseLayout];
        if(Lbase && Lbase['k'+s.Lcode]) {
          s.Lcode=Lbase['k'+s.Lcode];
        }
        /* 13/03/2007 MCD: Swedish: End mapping of keystroke to US keyboard */
        
        if(typeof(activeKeyboard['KM'])=='undefined'  &&  !(s.Lmodifiers & 0x60)) {
          // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
          s = {
            Lcode: keyMapManager._USKeyCodeToCharCode(s),
            Ltarg: s.Ltarg,
            Lmodifiers: 0,
            LisVirtualKey: false,
            vkCode: s.Lcode, // Helps to merge OSK and physical keystroke control paths.
            Lstates: s.Lstates,
            kName: ''
          };
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
    keyDown(e: KeyboardEvent): boolean {
      this.swallowKeypress = false;

      // Get event properties  
      var Levent = this._GetKeyEventProperties(e, true);
      if(Levent == null) {
        return true;
      }

      var LeventMatched = !this.processKeyEvent(Levent);

      if(LeventMatched) {
        if(e  &&  e.preventDefault) {
          e.preventDefault();
          e.stopPropagation();
        }
      }

      return !LeventMatched;
    }

    // KeyUp basically exists for two purposes:
    // 1)  To detect browser form submissions (handled in kmwdomevents.ts)
    // 2)  To detect modifier state changes.
    keyUp(e: KeyboardEvent): boolean {
      var Levent = this._GetKeyEventProperties(e, false);
      if(Levent == null) {
        return true;
      }

      return this.doModifierPress(Levent, false);
    }

    keyPress(e: KeyboardEvent): boolean {
      let keyman = com.keyman.singleton;

      var Levent = this._GetKeyEventProperties(e);
      if(Levent == null || Levent.LisVirtualKey) {
        return true;
      }

      // _Debug('KeyPress code='+Levent.Lcode+'; Ltarg='+Levent.Ltarg.tagName+'; LisVirtualKey='+Levent.LisVirtualKey+'; _KeyPressToSwallow='+keymanweb._KeyPressToSwallow+'; keyCode='+(e?e.keyCode:'nothing'));

      /* I732 START - 13/03/2007 MCD: Swedish: Start positional keyboard layout code: prevent keystroke */
      if(!keyman.keyboardManager.activeKeyboard['KM']) {
        if(!this.swallowKeypress) {
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
      let outputTarget = Levent.Ltarg;
      
      // Only reached if it's a mnemonic keyboard.
      if(this.swallowKeypress || keyman['interface'].processKeystroke(keyman.util.physicalDevice, Levent.Ltarg, Levent)) {
        this.swallowKeypress = false;
        if(e && e.preventDefault) {
          e.preventDefault();
          e.stopPropagation();
        }
        return false;
      }

      this.swallowKeypress = false;
      return true;
    }
  }
}