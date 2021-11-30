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

// Also relies on @keymanapp/web-utils, which is included via tsconfig.json.

namespace com.keyman.text {
  export type BeepHandler = (outputTarget: OutputTarget) => void;
  export type LogMessageHandler = (str: string) => void;

  export interface VariableStoreSerializer {
    loadStore(keyboardID: string, storeName: string): VariableStore;
    saveStore(keyboardID: string, storeName: string, storeMap: VariableStore);
  }

  export interface ProcessorInitOptions {
    baseLayout?: string;
    variableStoreSerializer?: VariableStoreSerializer;
  }

  export class KeyboardProcessor {
    public static readonly DEFAULT_OPTIONS: ProcessorInitOptions = {
      baseLayout: 'us'
    }

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

    keyboardInterface: KeyboardInterface;

    baseLayout: string;

    // Callbacks for various feedback types
    beepHandler?: BeepHandler;
    warningLogger?: LogMessageHandler;
    errorLogger?: LogMessageHandler;

    constructor(options?: ProcessorInitOptions) {
      if(!options) {
        options = KeyboardProcessor.DEFAULT_OPTIONS;
      }

      this.baseLayout = options.baseLayout || KeyboardProcessor.DEFAULT_OPTIONS.baseLayout;
      this.keyboardInterface = new KeyboardInterface(options.variableStoreSerializer);
      this.installInterface();
    }

    private installInterface() {
      // We must ensure that the keyboard can find the API functions at the expected place.
      let globalThis = utils.getGlobalObject();
      globalThis[KeyboardInterface.GLOBAL_NAME] = this.keyboardInterface;

      // Ensure that the active keyboard is set on the keyboard interface object.
      if(this.activeKeyboard) {
        this.keyboardInterface.activeKeyboard = this.activeKeyboard;
      }
    }

    public get activeKeyboard(): keyboards.Keyboard {
      return this.keyboardInterface.activeKeyboard;
    }

    public set activeKeyboard(keyboard: keyboards.Keyboard) {
      this.keyboardInterface.activeKeyboard = keyboard;

      // All old deadkeys and keyboard-specific cache should immediately be invalidated
      // on a keyboard change.
      this.resetContext();
    }

    get layerStore(): MutableSystemStore {
      return this.keyboardInterface.systemStores[KeyboardInterface.TSS_LAYER] as MutableSystemStore;
    }

    public get layerChangedStore(): MutableSystemStore {
      return this.keyboardInterface.systemStores[KeyboardInterface.TSS_LAYERCHANGED] as MutableSystemStore;
    }

    public get layerId(): string {
      return this.layerStore.value;
    }

    // Note:  will trigger an 'event' callback designed to notify the OSK of layer changes.
    public set layerId(value: string) {
      this.layerStore.set(value);
    }

    /**
     * Get the default RuleBehavior for the specified key, attempting to mimic standard browser defaults
     * where and when appropriate.
     *
     * @param   {object}  Lkc           The pre-analyzed KeyEvent object
     * @param   {boolean} outputTarget  The OutputTarget receiving the KeyEvent
     * @return  {string}
     */
    defaultRuleBehavior(Lkc: KeyEvent, outputTarget: OutputTarget): RuleBehavior {
      let preInput = Mock.from(outputTarget);
      let ruleBehavior = new RuleBehavior();

      let matched = false;
      var char = '';
      var special: EmulationKeystrokes;
      if(Lkc.isSynthetic || outputTarget.isSynthetic) {
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
            this.keyboardInterface.defaultBackspace(outputTarget);
          } else if(special || DefaultOutput.isCommand(Lkc)) { // Filters out 'commands' like TAB.
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

    setSyntheticEventDefaults(Lkc: text.KeyEvent) {
      // Set the flags for the state keys.
      Lkc.Lstates |= this.stateKeys['K_CAPS']    ? Codes.modifierCodes['CAPS'] : Codes.modifierCodes['NO_CAPS'];
      Lkc.Lstates |= this.stateKeys['K_NUMLOCK'] ? Codes.modifierCodes['NUM_LOCK'] : Codes.modifierCodes['NO_NUM_LOCK'];
      Lkc.Lstates |= this.stateKeys['K_SCROLL']  ? Codes.modifierCodes['SCROLL_LOCK'] : Codes.modifierCodes['NO_SCROLL_LOCK'];

      // Set LisVirtualKey to false to ensure that nomatch rule does fire for U_xxxx keys
      if(Lkc.kName && Lkc.kName.substr(0,2) == 'U_') {
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
      if((Lkc.Lmodifiers & Codes.modifierBitmasks['ALT_GR_SIM']) == Codes.modifierBitmasks['ALT_GR_SIM'] && this.activeKeyboard.emulatesAltGr) {
        Lkc.Lmodifiers &= ~Codes.modifierBitmasks['ALT_GR_SIM'];
        Lkc.Lmodifiers |= Codes.modifierCodes['RALT'];
      }
    }

    constructNullKeyEvent(device: utils.DeviceSpec): KeyEvent {
      const keyEvent = KeyEvent.constructNullKeyEvent(device);
      this.setSyntheticEventDefaults(keyEvent);
      return keyEvent;
    }

    processNewContextEvent(device: utils.DeviceSpec, outputTarget: OutputTarget): RuleBehavior {
      return this.activeKeyboard ?
        this.keyboardInterface.processNewContextEvent(outputTarget, this.constructNullKeyEvent(device)) :
        null;
    }

    processPostKeystroke(device: utils.DeviceSpec, outputTarget: OutputTarget): RuleBehavior {
      return this.activeKeyboard ?
        this.keyboardInterface.processPostKeystroke(outputTarget, this.constructNullKeyEvent(device)) :
        null;
    }

    processKeystroke(keyEvent: KeyEvent, outputTarget: OutputTarget): RuleBehavior {
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
        matchBehavior = this.keyboardInterface.processKeystroke(outputTarget, keyEvent);
      }

      if(!matchBehavior || matchBehavior.triggerKeyDefault) {
        // Restore the virtual key code if a mnemonic keyboard is being used
        // If no vkCode value was stored, maintain the original Lcode value.
        keyEvent.Lcode=keyEvent.vkCode || keyEvent.Lcode;

        // Handle unmapped keys, including special keys
        // The following is physical layout dependent, so should be avoided if possible.  All keys should be mapped.
        this.keyboardInterface.activeTargetOutput = outputTarget;

        // Match against the 'default keyboard' - rules to mimic the default string output when typing in a browser.
        // Many keyboards rely upon these 'implied rules'.
        let defaultBehavior = this.defaultRuleBehavior(keyEvent, outputTarget);
        if(defaultBehavior) {
          if(!matchBehavior) {
            matchBehavior = defaultBehavior;
          } else {
            matchBehavior.mergeInDefaults(defaultBehavior);
          }
          matchBehavior.triggerKeyDefault = false; // We've triggered it successfully.
        } // If null, we must rely on something else (like the browser, in DOM-aware code) to fulfill the default.

        this.keyboardInterface.activeTargetOutput = null;
      }

      return matchBehavior;
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
          //
          // However, DO make sure modifier keys pass through safely.
          // (https://github.com/keymanapp/keyman/issues/3744)
          if(!KeyboardProcessor.isModifier(Lkc)) {
            delete Lkc.Lcode;
          }
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
    static getModifierState(layerId: string): number {
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
     * Get state key state from layer id
     *
     * @param       {string}      layerId       layer id (e.g. caps)
     * @return      {number}                    modifier key state (desktop keyboards)
     */
     static getStateFromLayer(layerId: string): number {
      var modifier=0;

      if(layerId.indexOf('caps') >= 0) {
        modifier |= Codes.modifierCodes['CAPS'];
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
        if(this.activeKeyboard.isChiral && (this.activeKeyboard.emulatesAltGr) &&
            (this.modStateFlags & Codes.modifierBitmasks['ALT_GR_SIM']) == Codes.modifierBitmasks['ALT_GR_SIM']) {
          keyShiftState |= Codes.modifierBitmasks['ALT_GR_SIM'];
          keyShiftState &= ~Codes.modifierCodes['RALT'];
        }

        for(i=0; i < lockNames.length; i++) {
          if(lockStates & Codes.stateBitmasks[lockNames[i]]) {
            this.stateKeys[lockKeys[i]] = !!(lockStates & Codes.modifierCodes[lockNames[i]]);
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

      this.updateStates();

      if(this.activeKeyboard.isMnemonic && this.stateKeys['K_CAPS']) {
        // Modifier keypresses doesn't trigger mnemonic manipulation of modifier state.
        // Only an output key does; active use of Caps will also flip the SHIFT flag.
        if(!e || !KeyboardProcessor.isModifier(e)) {
          // Mnemonic keystrokes manipulate the SHIFT property based on CAPS state.
          // We need to unflip them when tracking the OSK layer.
          keyShiftState ^= Codes.modifierCodes['SHIFT'];
        }
      }

      this.layerId = this.getLayerId(keyShiftState);
      return true;
    }

    private updateStates(): void {
      var lockNames  = ['CAPS', 'NUM_LOCK', 'SCROLL_LOCK'];
      var lockKeys   = ['K_CAPS', 'K_NUMLOCK', 'K_SCROLL'];

      for(let i=0; i < lockKeys.length; i++) {
        const key = lockKeys[i];
        const flag = this.stateKeys[key];
        const onBit = lockNames[i];
        const offBit = 'NO_' + lockNames[i];

        // Ensures that the current mod-state info properly matches the currently-simulated
        // state key states.
        if(flag) {
          this.modStateFlags |= Codes.modifierCodes[onBit];
          this.modStateFlags &= ~Codes.modifierCodes[offBit];
        } else {
          this.modStateFlags &= ~Codes.modifierCodes[onBit];
          this.modStateFlags |= Codes.modifierCodes[offBit];
        }
      }
    }

    getLayerId(modifier: number): string {
      return keyboards.Layouts.getLayerId(modifier);
    }

    /**
     * Select the OSK's next keyboard layer based upon layer switching keys as a default
     * The next layer will be determined from the key name unless otherwise specifed
     *
     *  @param  {string}                    keyName     key identifier
     *  @param  {number|string|undefined}   nextLayerIn optional next layer identifier
     *  @return {boolean}                               return true if keyboard layer changed
     */
    selectLayer(keyEvent: KeyEvent, fromNameOnly: boolean = false): boolean {
      let keyName = keyEvent.kName;
      var nextLayer = fromNameOnly ? null : keyEvent.kNextLayer;
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
      this.updateLayer(keyEvent, nextLayer);

      return true;
    }

    /**
     * Sets the new layer id, allowing for toggling shift/ctrl/alt while preserving the remainder
     * of the modifiers represented by the current layer id (where applicable)
     *
     * @param       {string}      id      layer id (e.g. ctrlshift)
     */
    updateLayer(keyEvent: KeyEvent, id: string) {
      let activeLayer = this.layerId;
      var s = activeLayer;

      // Do not change layer unless needed (27/08/2015)
      if(id == activeLayer && keyEvent.device.formFactor != utils.FormFactor.Desktop) {
        return false;
      }

      var idx=id;
      var i;

      if(keyEvent.device.formFactor == utils.FormFactor.Desktop) {
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
          var modifier=KeyboardProcessor.getModifierState(s);

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

      let layout = this.activeKeyboard.layout(keyEvent.device.formFactor);
      if(layout.getLayer(s)) {
        this.layerId = s;
      } else {
        this.layerId = 'default';
      }

      let baseModifierState = text.KeyboardProcessor.getModifierState(this.layerId);
      this.modStateFlags = baseModifierState | keyEvent.Lstates;
    }

    static isModifier(Levent: KeyEvent): boolean {
      switch(Levent.Lcode) {
        case 16: //"K_SHIFT":16,"K_CONTROL":17,"K_ALT":18
        case 17:
        case 18:
        case 20: //"K_CAPS":20, "K_NUMLOCK":144,"K_SCROLL":145
        case 144:
        case 145:
          return true;
        default:
          return false;
      }
    }

    // Returns true if the key event is a modifier press, allowing keyPress to return selectively
    // in those cases.
    doModifierPress(Levent: KeyEvent, outputTarget: OutputTarget, isKeyDown: boolean): boolean {
      if(!this.activeKeyboard) {
        return false;
      }

      if(Levent.Lcode == 8) {
        // I3318 (always clear deadkeys after backspace)
        outputTarget.deadkeys().clear();
      } else if(KeyboardProcessor.isModifier(Levent)) {
        // For eventual integration - we bypass an OSK update for physical keystrokes when in touch mode.
        this.activeKeyboard.notify(Levent.Lcode, outputTarget, isKeyDown ? 1 : 0);
        if(!Levent.device.touchable) {
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

    resetContext() {
      this.layerId = 'default';

      this.keyboardInterface.resetContextCache();
      this._UpdateVKShift(null, 15, 0);
    };

    setNumericLayer(device: utils.DeviceSpec) {
      if (this.activeKeyboard) {
        let layout = this.activeKeyboard.layout(device.formFactor);
        if(layout.getLayer('numeric')) {
          this.layerId = 'numeric';
        }
      }
    };
  }
}

(function () {
  let ns = com.keyman.text;

  // Let the Keyboard Processor be available both in the browser and in Node.
  if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
    module.exports = ns.KeyboardProcessor;
    //@ts-ignore
    ns.KeyboardProcessor.com = com; // Export the root namespace so that all KeyboardProcessor classes are accessible by unit tests.
  }
}());
