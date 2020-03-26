// Establishes key-code definitions.
/// <reference path="codes.ts" />
// Defines our generalized "KeyEvent" class.
/// <reference path="keyEvent.ts" />
// Defines the RuleBehavior keyboard-processing return object.
/// <reference path="ruleBehavior.ts" />
// Defines default key handling behaviors.
/// <reference path="defaultOutput.ts" />
// Defines the keyboard wrapper object.
/// <reference path="../keyboards/keyboard.ts" />
// Defines built-in keymapping.
/// <reference path="keyMapping.ts" />

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

    keyboardInterface: KeyboardInterface;

    baseLayout: string;
    private keyboard: keyboards.Keyboard;

    constructor(baseLayout?: string) {
      this.baseLayout = baseLayout || 'us'; // default BaseLayout
      this.keyboardInterface = new KeyboardInterface();
      this.installInterface();
    }

    private installInterface() {
      // TODO:  replace 'window' with a (currently-unwritten) utility call that retrieves 
      //        the global object (whether browser, Node, WebWorker).
      //
      //        We must ensure that the keyboard can find the API functions at the expected place.
      let globalThis = window;
      globalThis[KeyboardInterface.GLOBAL_NAME] = this.keyboardInterface;

      // Ensure that the active keyboard is set on the keyboard interface object.
      if(this.activeKeyboard) {
        this.keyboardInterface.activeKeyboard = this.activeKeyboard;
      }
    }

    public get activeKeyboard(): keyboards.Keyboard {
      return this.keyboard;
    }

    public set activeKeyboard(keyboard: keyboards.Keyboard) {
      this.keyboard = keyboard;

      // All old deadkeys and keyboard-specific cache should immediately be invalidated
      // on a keyboard change.
      this.keyboardInterface.resetContext();

      // Ensure that the setting propagates immediately to the keyboard interface object.
      // Matters for some unit tests.
      this.keyboardInterface.activeKeyboard = keyboard;
    }

    /**
     * Get the default RuleBehavior for the specified key, attempting to mimic standard browser defaults 
     * where and when appropriate.
     *
     * @param   {object}  Lkc  The pre-analyzed key event object
     * @param   {boolean} usingOSK
     * @return  {string}
     */
    defaultRuleBehavior(Lkc: KeyEvent, usingOSK: boolean): RuleBehavior {
      let outputTarget = Lkc.Ltarg;
      let preInput = Mock.from(outputTarget);
      let ruleBehavior = new RuleBehavior();

      let matched = false;
      var char = '';
      var special: EmulationKeystrokes;
      if(usingOSK || outputTarget.isSynthetic) {
        matched = true;  // All the conditions below result in matches until the final else, which restores the expected default
                         // if no match occurs.

        if(DefaultOutput.isCommand(Lkc)) {
          // Note this in the rule behavior, return successfully.  We'll consider applying it later.
          ruleBehavior.triggersDefaultCommand = true;

          // We'd rather let the browser handle these keys, but we're using emulated keystrokes, forcing KMW
          // to emulate default behavior here.
        } else if((special = DefaultOutput.forSpecialEmulation(Lkc)) != null) { 
          switch(special) {
            case EmulationKeystrokes.Backspace:
              this.keyboardInterface.defaultBackspace(outputTarget);
              break;
            case EmulationKeystrokes.Enter:
              outputTarget.handleNewlineAtCaret();
              break;
            case EmulationKeystrokes.Space:
              this.keyboardInterface.output(0, outputTarget, ' ');
              break;
            // case '\u007f': // K_DEL
              // // For (possible) future implementation.
              // // Would recommend (conceptually) equaling K_RIGHT + K_BKSP, the former of which would technically be a 'command'.
            default:
              // In case we extend the allowed set, but forget to implement its handling case above.
              ruleBehavior.errorLog = "Unexpected 'special emulation' character (\\u" + (special as String).kmwCharCodeAt(0).toString(16) + ") went unhandled!";
          } 
        } else {
          // Back to the standard default, pending normal matching.
          matched = false;
        }
      }

      let isMnemonic = this.activeKeyboard && this.activeKeyboard.isMnemonic;

      if(!matched) {
        if((char = DefaultOutput.forAny(Lkc, isMnemonic)) != null) {
          special = DefaultOutput.forSpecialEmulation(Lkc)
          if(special == EmulationKeystrokes.Backspace) {
            // A browser's default backspace may fail to delete both parts of an SMP character.
            this.keyboardInterface.defaultBackspace();
          } else if(special) {
            // We only do the "for special emulation" cases under the condition above... aside from backspace
            // Let the browser handle those.
            return null;
          } else {
            this.keyboardInterface.output(0, outputTarget, char);
          }
        } else {
          // No match, no default RuleBehavior.
          return null;
        }
      }

      // Shortcut things immediately if there were issues generating this rule behavior.
      if(ruleBehavior.errorLog) {
        return ruleBehavior;
      }

      let transcription = outputTarget.buildTranscriptionFrom(preInput, Lkc);
      ruleBehavior.transcription = transcription;

      return ruleBehavior;
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

    setSyntheticEventDefaults(Lkc: text.KeyEvent) {
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

      if(!keyman.isEmbedded && !fromOSK && keyman.util.device.browser == 'firefox') {
        // I1466 - Convert the - keycode on mnemonic as well as positional layouts
        // FireFox, Mozilla Suite
        if(KeyMapping.browserMap.FF['k'+keyEvent.Lcode]) {
          keyEvent.Lcode=KeyMapping.browserMap.FF['k'+keyEvent.Lcode];
        }
      }

      var matchBehavior: RuleBehavior;

      // Pass this key code and state to the keyboard program
      if(this.activeKeyboard && keyEvent.Lcode != 0) {
        /*
         * The `this.installInterface()` call is insurance against something I've seen in unit tests when things break a bit.
         *
         * Currently, when a KMW shutdown doesn't go through properly or completely, sometimes we end up with parallel
         * versions of KMW running, and an old, partially-shutdown one will "snipe" a command meant for the most-recent 
         * one's test. So, installing here ensures that the active Processor has its matching KeyboardInterface ready, 
         * even should that occur.
         */
        this.installInterface();
        matchBehavior = this.keyboardInterface.processKeystroke(fromOSK ? keyman.util.device : keyman.util.physicalDevice, outputTarget, keyEvent);
      }

      if(!matchBehavior) {
        // Restore the virtual key code if a mnemonic keyboard is being used
        // If no vkCode value was stored, maintain the original Lcode value.
        keyEvent.Lcode=keyEvent.vkCode || keyEvent.Lcode;

        // Handle unmapped keys, including special keys
        // The following is physical layout dependent, so should be avoided if possible.  All keys should be mapped.
        this.keyboardInterface.activeTargetOutput = outputTarget;

        // Match against the 'default keyboard' - rules to mimic the default string output when typing in a browser.
        // Many keyboards rely upon these 'implied rules'.
        matchBehavior = this.defaultRuleBehavior(keyEvent, fromOSK);

        this.keyboardInterface.activeTargetOutput = null;
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
      let formFactor = keyman.util.device.formFactor;

      // Determine the current target for text output and create a "mock" backup
      // of its current, pre-input state.
      let outputTarget = keyEvent.Ltarg;

      let fromOSK = !!e; // If specified, it's from the OSK.

      // Enables embedded-path OSK sourcing detection.
      if(typeof e == 'boolean') {
        e = null as osk.KeyElement; // Cast is necessary for TS type-checking later in the method.
      }

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
      if((formFactor == 'desktop' || this.activeKeyboard.usesDesktopLayoutOnDevice(keyman.util.device)) && fromOSK) {
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
        if(KeyMapping.browserMap.FF['k'+keyEvent.Lcode]) {
          keyEvent.Lcode = KeyMapping.browserMap.FF['k'+keyEvent.Lcode];
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
        com.keyman.dom.DOMEventHandlers.states._IgnoreNextSelChange = 100;
        keyman.domManager.focusLastActiveElement();
        com.keyman.dom.DOMEventHandlers.states._IgnoreNextSelChange = 0;
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

        // If we're performing a 'default command', it's not a standard 'typing' event - don't do fat-finger stuff.
        // Also, don't do fat-finger stuff if predictive text isn't enabled.
        if(keyman.modelManager.enabled && !ruleBehavior.triggersDefaultCommand) {
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

              let altEvent = osk.PreProcessor._GetClickEventProperties(altKey, keyEvent.Ltarg.getElement());
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
        }

        // Now that we've done all the keystroke processing needed, ensure any extra effects triggered
        // by the actual keystroke occur.
        ruleBehavior.finalize();

        // If the transform isn't empty, we've changed text - which should produce a 'changed' event in the DOM.
        //
        // TODO:  This check should be done IN a dom module, not here in web-core space.  This place is closer 
        //        to that goal than it previously was, at least.
        let ruleTransform = ruleBehavior.transcription.transform;
        if(ruleTransform.insert != "" || ruleTransform.deleteLeft > 0 || ruleTransform.deleteRight > 0) {
          if(outputTarget.getElement() == dom.DOMEventHandlers.states.activeElement) {
            dom.DOMEventHandlers.states.changed = true;
          }
        }

        // -- All keystroke (and 'alternate') processing is now complete.  Time to finalize everything! --
        
        // Notify the ModelManager of new input - it's predictive text time!
        ruleBehavior.transcription.alternates = alternates;
        // Yes, even for ruleBehavior.triggersDefaultCommand.  Those tend to change the context.
        keyman.modelManager.predict(ruleBehavior.transcription);

        // KMEA and KMEI (embedded mode) use direct insertion of the character string
        if(keyman.isEmbedded) {
          // A special embedded callback used to setup direct callbacks to app-native code.
          keyman['oninserttext'](ruleTransform.deleteLeft, ruleTransform.insert, ruleTransform.deleteRight);
          keyman.refreshElementContent(outputTarget.getElement());
        }

        // Text did not change (thus, no text "input") if we tabbed or merely moved the caret.
        if(!ruleBehavior.triggersDefaultCommand) {
          // For DOM-aware targets, this will trigger a DOM event page designers may listen for.
          outputTarget.doInputEvent();
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

    // FIXME:  makes some bad assumptions.
    static setMnemonicCode(Lkc: KeyEvent, shifted: boolean, capsActive: boolean) {
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
        mappingEvent.Lmodifiers = (shifted ? 0x10 : 0);  // mnemonic lookups only exist for default & shift layers.
        var mappedChar: string = DefaultOutput.forAny(mappingEvent, true);
        
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
      var activeKeyboard = this.activeKeyboard;
      if(!activeKeyboard.scriptObject['VKDictionary']) {
        var a=[];
        if(typeof activeKeyboard.scriptObject['KVKD'] == 'string') {
          // Build the VK dictionary
          // TODO: Move the dictionary build into the compiler -- so compiler generates code such as following.  
          // Makes the VKDictionary member unnecessary.
          //       this.KVKD={"K_ABC":256,"K_DEF":257,...};
          var s=activeKeyboard.scriptObject['KVKD'].split(' ');
          for(var i=0; i<s.length; i++) {
            a[s[i].toUpperCase()]=i+256; // We force upper-case since virtual keys should be case-insensitive.
          }
        }
        activeKeyboard.scriptObject['VKDictionary']=a;
      }

      var res=activeKeyboard.scriptObject['VKDictionary'][keyName.toUpperCase()];
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

      if(!this.activeKeyboard) {
        return true;
      }

      if(e) {
        // read shift states from Pevent
        keyShiftState = e.Lmodifiers;
        lockStates = e.Lstates;

        // Are we simulating AltGr?  If it's a simulation and not real, time to un-simulate for the OSK.
        if(this.activeKeyboard.isChiral && osk.Layouts.emulatesAltGr() && 
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
      var isChiral = this.activeKeyboard && this.activeKeyboard.isChiral;

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

    // Returns true if the key event is a modifier press, allowing keyPress to return selectively
    // in those cases.
    doModifierPress(Levent: KeyEvent, isKeyDown: boolean): boolean {
      let keyman = com.keyman.singleton;
      let outputTarget = Levent.Ltarg;

      if(!this.activeKeyboard) {
        return false;
      }

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
          this.activeKeyboard.notify(Levent.Lcode, outputTarget, isKeyDown ? 1 : 0); 
          if(!keyman.util.device.touchable) {
            return this._UpdateVKShift(Levent, Levent.Lcode-15, 1); // I2187
          } else {
            return true;
          }
      }

      if(Levent.LmodifierChange) {
        this.activeKeyboard.notify(0, outputTarget, 1); 
        this._UpdateVKShift(Levent, 0, 1);
      }

      // No modifier keypresses detected.
      return false;
    }
  }
}