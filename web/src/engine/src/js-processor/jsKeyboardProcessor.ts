/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Implementation of the JavaScript keyboard processor
 */

// #region Big ol' list of imports

import { EventEmitter } from 'eventemitter3';
import { ModifierKeyConstants } from '@keymanapp/common-types';
import {
  Codes, type JSKeyboard, MinimalKeymanGlobal, KeyEvent, Layouts,
  DefaultRules, EmulationKeystrokes, type MutableSystemStore,
  TextStore, ProcessorAction, SystemStoreIDs, SyntheticTextStore,
  KeyboardProcessor,
  EventMap,
  BeepHandler,
} from "keyman/engine/keyboard";
import { JSKeyboardInterface }  from './jsKeyboardInterface.js';
import { DeviceSpec, globalObject, KMWString } from "keyman/common/web-utils";

// #endregion

export type LogMessageHandler = (str: string) => void;

export interface ProcessorInitOptions {
  baseLayout?: string;
  keyboardInterface?: JSKeyboardInterface; // for tests, replace keyboardInterface with a mock, TODO-web-core: refactor into a unit test pattern
  defaultOutputRules?: DefaultRules; // Takes the class def object, not an instance thereof.
}

export class JSKeyboardProcessor extends EventEmitter<EventMap> implements KeyboardProcessor {
  private static readonly DEFAULT_OPTIONS: ProcessorInitOptions = {
    baseLayout: 'us',
    defaultOutputRules: new DefaultRules()  // TODO-web-core: move this out of here and only in keymanEngine.ts, rename to DefaultOutputRules
  };

  // Tracks the simulated value for supported state keys, allowing the OSK to mirror a physical keyboard for them.
  // Using the exact keyCode name from the Codes definitions will allow for certain optimizations elsewhere in the code.
  public stateKeys = {
    "K_CAPS":false,
    "K_NUMLOCK":false,
    "K_SCROLL":false
  };

  // Tracks the most recent modifier state information in order to quickly detect changes
  // in keyboard state not otherwise captured by the hosting page in the browser.
  // Needed for AltGr simulation.
  public modStateFlags: number = 0;

  public keyboardInterface: JSKeyboardInterface;

  /**
   * Indicates the device (platform) to be used for non-keystroke events,
   * such as those sent to `begin postkeystroke` and `begin newcontext`
   * entry points.
   */
  public contextDevice: DeviceSpec;

  public baseLayout: string;

  public defaultRules: DefaultRules;

  // Callbacks for various feedback types
  public beepHandler?: BeepHandler;
  public warningLogger?: LogMessageHandler;
  public errorLogger?: LogMessageHandler;

  constructor(device: DeviceSpec, options?: ProcessorInitOptions) {
    super();

    if(!options) {
      options = JSKeyboardProcessor.DEFAULT_OPTIONS;
    }

    this.contextDevice = device;

    this.baseLayout = options.baseLayout || JSKeyboardProcessor.DEFAULT_OPTIONS.baseLayout;
    this.keyboardInterface = options.keyboardInterface || new JSKeyboardInterface(globalObject(), MinimalKeymanGlobal);
    this.defaultRules = options.defaultOutputRules || JSKeyboardProcessor.DEFAULT_OPTIONS.defaultOutputRules;
  }

  public get activeKeyboard(): JSKeyboard {
    return this.keyboardInterface.activeKeyboard;
  }

  public set activeKeyboard(keyboard: JSKeyboard) {
    this.keyboardInterface.activeKeyboard = keyboard;

    // All old deadkeys and keyboard-specific cache should immediately be invalidated
    // on a keyboard change.
    this.resetContext();
  }

  public get layerStore(): MutableSystemStore {
    return this.keyboardInterface.systemStores[SystemStoreIDs.TSS_LAYER] as MutableSystemStore;
  }

  public get newLayerStore(): MutableSystemStore {
    return this.keyboardInterface.systemStores[SystemStoreIDs.TSS_NEWLAYER] as MutableSystemStore;
  }

  public get oldLayerStore(): MutableSystemStore {
    return this.keyboardInterface.systemStores[SystemStoreIDs.TSS_OLDLAYER] as MutableSystemStore;
  }

  public get layerId(): string {
    return this.layerStore.value;
  }

  // Note:  will trigger an 'event' callback designed to notify the OSK of layer changes.
  public set layerId(value: string) {
    this.layerStore.set(value);
  }

  /**
   * Get the default ProcessorAction for the specified key, attempting to mimic standard browser defaults
   * where and when appropriate.
   *
   * @param   {KeyEvent}  Lkc           The pre-analyzed KeyEvent object
   * @param   {TextStore} textStore     The textStore receiving the KeyEvent
   * @param   {boolean}   readonly      True if the textStore is read-only
   * @return  {string}
   */
  private defaultRuleBehavior(Lkc: KeyEvent, textStore: TextStore, readonly: boolean): ProcessorAction {
    const preInput = SyntheticTextStore.from(textStore, readonly);
    const ruleBehavior = new ProcessorAction();

    let matched = false;
    let char = '';
    let special: EmulationKeystrokes;
    if(Lkc.isSynthetic || textStore.isSynthetic) {
      matched = true;  // All the conditions below result in matches until the final else, which restores the expected default
                        // if no match occurs.

      if(this.defaultRules.isCommand(Lkc)) {
        // Note this in the rule behavior, return successfully.  We'll consider applying it later.
        ruleBehavior.triggersDefaultCommand = true;

        // We'd rather let the browser handle these keys, but we're using emulated keystrokes, forcing KMW
        // to emulate default behavior here.
      } else if((special = this.defaultRules.forSpecialEmulation(Lkc)) != null) {
        switch(special) {
          case EmulationKeystrokes.Backspace:
            this.keyboardInterface.defaultBackspace(textStore);
            break;
          case EmulationKeystrokes.Enter:
            textStore.handleNewlineAtCaret();
            break;
          // case '\u007f': // K_DEL
            // // For (possible) future implementation.
            // // Would recommend (conceptually) equaling K_RIGHT + K_BKSP, the former of which would technically be a 'command'.
          default:
            // In case we extend the allowed set, but forget to implement its handling case above.
            ruleBehavior.errorLog = "Unexpected 'special emulation' character (\\u" + KMWString.charCodeAt(special as string, 0).toString(16) + ") went unhandled!";
        }
      } else {
        // Back to the standard default, pending normal matching.
        matched = false;
      }
    }

    const isMnemonic = this.activeKeyboard && this.activeKeyboard.isMnemonic;

    if(!matched) {
      if((char = this.defaultRules.forAny(Lkc, isMnemonic, ruleBehavior)) != null) {
        special = this.defaultRules.forSpecialEmulation(Lkc)
        if(special == EmulationKeystrokes.Backspace) {
          // A browser's default backspace may fail to delete both parts of an SMP character.
          this.keyboardInterface.defaultBackspace(textStore);
        } else if(special || this.defaultRules.isCommand(Lkc)) { // Filters out 'commands' like TAB.
          // We only do the "for special emulation" cases under the condition above... aside from backspace
          // Let the browser handle those.
          return null;
        } else {
          this.keyboardInterface.output(0, textStore, char);
        }
      } else {
        // No match, no default ProcessorAction.
        return null;
      }
    }

    // Shortcut things immediately if there were issues generating this rule behavior.
    if(ruleBehavior.errorLog) {
      return ruleBehavior;
    }

    const transcription = textStore.buildTranscriptionFrom(preInput, Lkc, readonly);
    ruleBehavior.transcription = transcription;

    return ruleBehavior;
  }

  private processNewContextEvent(device: DeviceSpec, textStore: TextStore): ProcessorAction {
    return this.activeKeyboard ?
      this.keyboardInterface.processNewContextEvent(textStore, this.activeKeyboard.constructNullKeyEvent(device, this.stateKeys)) :
      null;
  }

  public processPostKeystroke(device: DeviceSpec, textStore: TextStore): ProcessorAction {
    return this.activeKeyboard ?
      this.keyboardInterface.processPostKeystroke(textStore, this.activeKeyboard.constructNullKeyEvent(device, this.stateKeys)) :
      null;
  }

  public processKeystroke(keyEvent: KeyEvent, textStore: TextStore): ProcessorAction {
    let matchBehavior: ProcessorAction;

    // Before keyboard rules apply, check if the left-context is empty.
    const nothingDeletable = KMWString.length(textStore.getTextBeforeCaret()) == 0 && textStore.isSelectionEmpty();

    // Pass this key code and state to the keyboard program
    if(this.activeKeyboard && keyEvent.Lcode != 0) {
      matchBehavior = this.keyboardInterface.processKeystroke(textStore, keyEvent);
    }

    // Final conditional component - if someone actually makes a keyboard rule that blocks output
    // of K_BKSP with an empty left-context or does other really weird things... it's on them.
    //
    // We don't expect such rules to appear, but trying to override them would likely result in odd
    // behavior in cases where such rules actually would appear.  (Though, _that_ should be caught
    // in the keyboard-review process and heavily discouraged, so... yeah.)
    if(nothingDeletable && keyEvent.Lcode == Codes.keyCodes.K_BKSP && matchBehavior.triggerKeyDefault) {
      matchBehavior = this.defaultRuleBehavior(keyEvent, textStore, false);
      matchBehavior.triggerKeyDefault = true;
      // Force a single `deleteLeft`.
      // @ts-ignore // force value override, because deleteLeft is marked readonly.
      matchBehavior.transcription.transform.deleteLeft = 1;
    } else if(!matchBehavior || matchBehavior.triggerKeyDefault) {
      // Restore the virtual key code if a mnemonic keyboard is being used
      // If no vkCode value was stored, maintain the original Lcode value.
      keyEvent.Lcode=keyEvent.vkCode || keyEvent.Lcode;

      // Handle unmapped keys, including special keys
      // The following is physical layout dependent, so should be avoided if possible.  All keys should be mapped.
      this.keyboardInterface.activeTextStore = textStore;

      // Match against the 'default keyboard' - rules to mimic the default string output when typing in a browser.
      // Many keyboards rely upon these 'implied rules'.
      const defaultBehavior = this.defaultRuleBehavior(keyEvent, textStore, false);
      if(defaultBehavior) {
        if(!matchBehavior) {
          matchBehavior = defaultBehavior;
        } else {
          this.mergeInOtherProcessorAction(matchBehavior, defaultBehavior);
        }
        matchBehavior.triggerKeyDefault = false; // We've triggered it successfully.
      } // If null, we must rely on something else (like the browser, in DOM-aware code) to fulfill the default.

      this.keyboardInterface.activeTextStore = null;
    }

    return matchBehavior;
  }

  /**
   * Function     _UpdateVKShift
   * Scope        Private
   * @param       {Object}            e     OSK event
   * @return      {boolean}                 Always true
   * Description  Updates the current shift state within KMW, updating the OSK's visualization thereof.
   */
  private _UpdateVKShift(e: KeyEvent): boolean {
    let keyShiftState=0;

    const lockNames  = ['CAPS', 'NUM_LOCK', 'SCROLL_LOCK'] as const;
    const lockKeys = ['K_CAPS', 'K_NUMLOCK', 'K_SCROLL'] as const;
    const lockModifiers = [ModifierKeyConstants.CAPITALFLAG, ModifierKeyConstants.NUMLOCKFLAG, ModifierKeyConstants.SCROLLFLAG] as const;


    if(!this.activeKeyboard) {
      return true;
    }

    if(e) {
      // read shift states from Pevent
      keyShiftState = e.Lmodifiers;

      // Are we simulating AltGr?  If it's a simulation and not real, time to un-simulate for the OSK.
      if(this.activeKeyboard.isChiral && (this.activeKeyboard.emulatesAltGr) &&
          (this.modStateFlags & Codes.modifierBitmasks['ALT_GR_SIM']) == Codes.modifierBitmasks['ALT_GR_SIM']) {
        keyShiftState |= Codes.modifierBitmasks['ALT_GR_SIM'];
        keyShiftState &= ~ModifierKeyConstants.RALTFLAG;
      }

      // Set stateKeys where corresponding value is passed in e.Lstates
      let stateMutation = false;
      for(let i=0; i < lockNames.length; i++) {
        if(e.Lstates & Codes.stateBitmasks[lockNames[i]]) {
          this.stateKeys[lockKeys[i]] = !!(e.Lstates & lockModifiers[i]);
          stateMutation = true;
        }
      }

      if(stateMutation) {
        this.emit('statekeychange', this.stateKeys);
      }
    }

    this.updateStates();

    if(this.activeKeyboard.isMnemonic && this.stateKeys['K_CAPS']) {
      // Modifier keypresses doesn't trigger mnemonic manipulation of modifier state.
      // Only an output key does; active use of Caps will also flip the SHIFT flag.
      if(!e || !e.isModifier) {
        // Mnemonic keystrokes manipulate the SHIFT property based on CAPS state.
        // We need to unflip them when tracking the OSK layer.
        keyShiftState ^= ModifierKeyConstants.K_SHIFTFLAG;
      }
    }

    this.layerId = this.getLayerId(keyShiftState);
    return true;
  }

  private updateStates(): void {
    const lockKeys = ['K_CAPS', 'K_NUMLOCK', 'K_SCROLL'] as const;
    const lockModifiers = [ModifierKeyConstants.CAPITALFLAG, ModifierKeyConstants.NUMLOCKFLAG, ModifierKeyConstants.SCROLLFLAG] as const;
    const noLockModifers = [ModifierKeyConstants.NOTCAPITALFLAG, ModifierKeyConstants.NOTNUMLOCKFLAG, ModifierKeyConstants.NOTSCROLLFLAG] as const;



    for(let i=0; i < lockKeys.length; i++) {
      const key = lockKeys[i];
      const flag = this.stateKeys[key];

      // Ensures that the current mod-state info properly matches the currently-simulated
      // state key states.
      if(flag) {
        this.modStateFlags |= lockModifiers[i];
        this.modStateFlags &= ~noLockModifers[i];
      } else {
        this.modStateFlags &= ~lockModifiers[i];
        this.modStateFlags |= noLockModifers[i];
      }
    }
  }

  private getLayerId(modifier: number): string {
    return Layouts.getLayerId(modifier);
  }

  /**
   * Select the OSK's next keyboard layer based upon layer switching keys as a default
   * The next layer will be determined from the key name unless otherwise specifed
   *
   *  @param  {string}                    keyName     key identifier
   *  @return {boolean}                               return true if keyboard layer changed
   */
  public selectLayer(keyEvent: KeyEvent): boolean {
    const keyName = keyEvent.kName;
    let nextLayer = keyEvent.kNextLayer;
    const isChiral = this.activeKeyboard && this.activeKeyboard.isChiral;

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
  private updateLayer(keyEvent: KeyEvent, id: string) {
    const activeLayer = this.layerId;
    let s = activeLayer;

    // Do not change layer unless needed (27/08/2015)
    if(id == activeLayer && keyEvent.device.formFactor != DeviceSpec.FormFactor.Desktop) {
      return;
    }

    let idx=id;
    let i;

    if(keyEvent.device.formFactor == DeviceSpec.FormFactor.Desktop) {
      // Need to test if target layer is a standard layer (based on the plain 'default')
      const replacements= ['leftctrl', 'rightctrl', 'ctrl', 'leftalt', 'rightalt', 'alt', 'shift'];

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
        let modifier=Codes.getModifierState(s);

        // Strip down to the base modifiable layer.
        for(i=0; i < replacements.length; i++) {
          // Don't forget to remove the kebab-case hyphens!
          s=s.replace(replacements[i] + '-', '');
          s=s.replace(replacements[i],'');
        }

        // Toggle the modifier represented by our input argument.
        switch(id) {
          case 'shift':
            modifier ^= ModifierKeyConstants.K_SHIFTFLAG;
            break;
          case 'leftctrl':
            modifier ^= ModifierKeyConstants.LCTRLFLAG;
            break;
          case 'rightctrl':
            modifier ^= ModifierKeyConstants.RCTRLFLAG;
            break;
          case 'ctrl':
            modifier ^= ModifierKeyConstants.K_CTRLFLAG;
            break;
          case 'leftalt':
            modifier ^= ModifierKeyConstants.LALTFLAG;
            break;
          case 'rightalt':
            modifier ^= ModifierKeyConstants.RALTFLAG;
            break;
          case 'alt':
            modifier ^= ModifierKeyConstants.K_ALTFLAG;
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

    const layout = this.activeKeyboard.layout(keyEvent.device.formFactor);
    if(layout.getLayer(s)) {
      this.layerId = s;
    } else {
      this.layerId = 'default';
    }

    const baseModifierState = Codes.getModifierState(this.layerId);
    this.modStateFlags = baseModifierState | keyEvent.Lstates;
  }

  // Returns true if the key event is a modifier press, allowing keyPress to return selectively
  // in those cases.
  public doModifierPress(Levent: KeyEvent, textStore: TextStore, isKeyDown: boolean): boolean {
    if(!this.activeKeyboard) {
      return false;
    }

    if(Levent.isModifier) {
      this.activeKeyboard.notify(Levent.Lcode, textStore, isKeyDown ? 1 : 0);
      // For eventual integration - we bypass an OSK update for physical keystrokes when in touch mode.
      if(!Levent.device.touchable) {
        return this._UpdateVKShift(Levent); // I2187
      } else {
        return true;
      }
    }

    if(Levent.LmodifierChange) {
      this.activeKeyboard.notify(0, textStore, 1);
      if(!Levent.device.touchable) {
        this._UpdateVKShift(Levent);
      }
    }

    // No modifier keypresses detected.
    return false;
  }

  /**
   * Tell the currently active keyboard that a new context has been selected,
   * e.g. by focus change, selection change, keyboard change, etc.
   *
   * @param    {Object}   textStore  The TextStore that has focus
   * @returns  {Object}                 A ProcessorAction object describing the cumulative effects of
   *                                    all matched keyboard rules
   */
  private performNewContextEvent(textStore: TextStore): ProcessorAction {
    const ruleBehavior = this.processNewContextEvent(this.contextDevice, textStore);

    if (ruleBehavior) {
      this.finalizeProcessorAction(ruleBehavior, textStore);
    }
    return ruleBehavior;
  }

  public resetContext(textStore?: TextStore) {
    this.layerId = 'default';

    // Make sure all deadkeys for the context get cleared properly.
    textStore?.resetContext();
    this.keyboardInterface.resetContextCache();

    // May be null if it's a keyboard swap.
    // Performed before _UpdateVKShift since the op may modify the displayed layer
    // Also updates the layer for predictions.
    if(textStore) {
      this.performNewContextEvent(textStore);
    }

    if(!this.contextDevice.touchable) {
      this._UpdateVKShift(null);
    }
  };

  public setNumericLayer(device: DeviceSpec) {
    if (this.activeKeyboard) {
      const layout = this.activeKeyboard.layout(device.formFactor);
      if(layout.getLayer('numeric')) {
        this.layerId = 'numeric';
      }
    }
  };

  public finalizeProcessorAction(data: ProcessorAction, textStore: TextStore): void {
    if (!data.transcription) {
      throw "Cannot finalize a ProcessorAction with no transcription.";
    }

    if (this.beepHandler && data.beep) {
      this.beepHandler(textStore);
    }

    for (const storeID in data.setStore) {
      const sysStore = this.keyboardInterface.systemStores[storeID];
      if (sysStore) {
        try {
          sysStore.set(data.setStore[storeID]);
        } catch (error) {
          if (this.errorLogger) {
            this.errorLogger("Rule attempted to perform illegal operation - 'platform' may not be changed.");
          }
        }
      } else if (this.warningLogger) {
        this.warningLogger("Unknown store affected by keyboard rule: " + storeID);
      }
    }

    this.keyboardInterface.applyVariableStores(data.variableStores);

    if (this.keyboardInterface.variableStoreSerializer) {
      for (const storeID in data.saveStore) {
        this.keyboardInterface.variableStoreSerializer.saveStore(this.activeKeyboard.id, storeID, data.saveStore[storeID]);
      }
    }

    if (data.triggersDefaultCommand) {
      const keyEvent = data.transcription.keystroke;
      this.defaultRules.applyCommand(keyEvent, textStore);
    }

    if (this.warningLogger && data.warningLog) {
      this.warningLogger(data.warningLog);
    } else if (this.errorLogger && data.errorLog) {
      this.errorLogger(data.errorLog);
    }
  }

  /**
   * Merges default-related behaviors from another ProcessorAction into this one.  Assumes that the current instance
   * "came first" chronologically.  Both RuleBehaviors must be sourced from the same keystroke.
   *
   * Intended use:  merging rule-based behavior with default key behavior during scenarios like those described
   * at https://github.com/keymanapp/keyman/pull/4350#issuecomment-768753852.
   *
   * This function does not attempt a "complete" merge for two fully-constructed RuleBehaviors!  Things
   * WILL break for unintended uses.
   * @param other
   */
  private mergeInOtherProcessorAction(first: ProcessorAction, other: ProcessorAction): void {
    const keystroke = first.transcription.keystroke;
    const keyFromOther = other.transcription.keystroke;
    if (keystroke.Lcode != keyFromOther.Lcode || keystroke.Lmodifiers != keyFromOther.Lmodifiers) {
      throw "ProcessorAction default-merge not supported unless keystrokes are identical!";
    }

    first.triggersDefaultCommand = first.triggersDefaultCommand || other.triggersDefaultCommand;

    const mergingTextStore = SyntheticTextStore.from(first.transcription.preInput, false);
    mergingTextStore.apply(first.transcription.transform);
    mergingTextStore.apply(other.transcription.transform);

    first.transcription = mergingTextStore.buildTranscriptionFrom(first.transcription.preInput, keystroke, false, first.transcription.alternates);
  }

}
