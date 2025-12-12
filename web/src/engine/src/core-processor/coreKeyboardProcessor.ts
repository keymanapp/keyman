/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { EventEmitter } from 'eventemitter3';
import { KM_Core, KM_CORE_STATUS, KM_CORE_CT, km_core_context, km_core_context_items } from 'keyman/engine/core-adapter';
import {
  BeepHandler,
  DeviceSpec, EventMap, Keyboard, KeyboardMinimalInterface, KeyboardProcessor,
  KeyEvent, KMXKeyboard, SyntheticTextStore, MutableSystemStore, TextStore, ProcessorAction,
  StateKeyMap,
  Deadkey,
  Codes
} from "keyman/engine/keyboard";
import { KM_CORE_EVENT_FLAG } from '../core-adapter/KM_Core.js';
import { ModifierKeyConstants } from '@keymanapp/common-types';

export class CoreKeyboardInterface implements KeyboardMinimalInterface {
  public activeKeyboard: Keyboard;
}

/**
 * Implements the core keyboard processing engine that interacts with the
 * shared Keyman Core component which handles .kmx keyboards.
 */
export class CoreKeyboardProcessor extends EventEmitter<EventMap> implements KeyboardProcessor {
  private _newLayerStore: MutableSystemStore = new MutableSystemStore(0, 'default');
  private _oldLayerStore: MutableSystemStore = new MutableSystemStore(0, 'default');
  private _layerStore: MutableSystemStore = new MutableSystemStore(0, 'default');
  private _keyboardInterface: CoreKeyboardInterface = new CoreKeyboardInterface();

  /**
   * Initialize the core processor with the provided base path.
   * Sets up the necessary environment for processing keyboard events.
   *
   * @param {string}  basePath    The path for the core processor resources, i.e. where the
   *                              km-core.js file is located.
   * @returns {Promise<void>} A promise that resolves when initialization is complete.
   */
  public async init(basePath: string): Promise<void> {
    await KM_Core.createCoreProcessor(basePath);
  }

  /**
   * Tracks the simulated value for supported state keys, allowing the OSK to
   * mirror a physical keyboard for them. Uses the exact keyCode name from the
   * Codes definitions to enable certain optimizations elsewhere in the code.
   *
   * @type {StateKeyMap}
   */
  public stateKeys: StateKeyMap = {
    "K_CAPS": false,
    "K_NUMLOCK": false,
    "K_SCROLL": false
  }

  /**
   * Indicates the device (platform) to be used for non-keystroke events.
   * Used for events such as those sent to `begin postkeystroke` and
   * `begin newcontext` entry points.
   *
   * @type {DeviceSpec}
   */
  public contextDevice: DeviceSpec;

  /**
   * Optional handler for beep events triggered by the processor.
   * Allows custom handling of beep feedback, such as for alerts or errors.
   *
   * @type {BeepHandler}
   */
  public beepHandler?: BeepHandler;

  /**
   * Bitfield representing the most recent modifier state (Alt, Ctrl, Shift, etc.)
   * as observed or simulated by the processor. Used to quickly detect changes in
   * modifier state not otherwise captured by the hosting page (important for AltGr).
   *
   * @type {number}
   */
  public modStateFlags: number = 0;

  /**
   * Stores the identifier for the base keyboard layout in use.
   * Used to determine the default layout for key mapping and processing.
   *
   * @type {string}
   */
  public baseLayout: string;

  /**
   * The currently active keyboard.
   *
   * @type {Keyboard}
   */
  public get activeKeyboard(): Keyboard {
    return this.keyboardInterface.activeKeyboard;
  }
  public set activeKeyboard(keyboard: Keyboard) {
    this.keyboardInterface.activeKeyboard = keyboard;
  }

  /**
   * The keyboard interface used by the processor.
   * Provides access to the keyboard interface implementation.
   *
   * @type {KeyboardMinimalInterface}
   */
  get keyboardInterface(): KeyboardMinimalInterface {
    return this._keyboardInterface;
  }

  /**
   * The store representing the currently active keyboard layer.
   *
   * @type {MutableSystemStore}
   */
  public get layerStore(): MutableSystemStore {
    // TODO-web-core: link to .kmx layer store (#15284)
    return this._layerStore;
  }

  /**
   * A writable store used when transitioning to a new layer
   *
   * @type {MutableSystemStore}
   */
  public get newLayerStore(): MutableSystemStore {
    // TODO-web-core: link to .kmx new-layer store (#15284)
    return this._newLayerStore;
  }

  /**
   * A store representing the previously active layer
   *
   * @type {MutableSystemStore}
   */
  public get oldLayerStore(): MutableSystemStore {
    // TODO-web-core: link to .kmx old-layer store (#15284)
    return this._oldLayerStore;
  }

  /**
   * Identifier of the currently active layer.
   *
   * @type {string}
   */
  public get layerId(): string {
    return this._layerStore.value;
  }
  public set layerId(value: string) {
    this._layerStore.set(value);
  }

  private getLayerId(modifier: number): string {
    // TODO-web-core: implement
    // return Layouts.getLayerId(modifier);
    return 'default'; // TODO-web-core: put into LayerNames enum
  }

  /**
   * Retrieve context including deadkeys from TextStore and apply to Core's context
   *
   * @param context     Context from Keyman Core
   * @param textStore   Web TextStore
   */
  private applyContextFromTextStore(context: km_core_context, textStore: TextStore): void {
    // Unlike the desktop Engines, we still track markers (deadkeys) in Engine
    // for Web at this time. This is for two reasons:
    // 1. We still have the legacy JSKeyboard code paths which manage deadkey
    //    state
    // 2. SyntheticTextStores which are used for rewinding and replaying key
    //    events in predictive text and multitap need to also replay deadkeys
    //
    // TODO: Once we make CoreKeyboardProcessor the primary keyboard processor
    // and fully deprecate JSKeyboardProcessor, we should consider moving the
    // ownership of context back into opaque Core objects within
    // SyntheticTextStore, so ownership of context and marker state can be
    // managed entirely within Core, KeymanWeb does not need to have knowledge
    // of markers, and then we better align with the desktop Engines.

    const caretPosition = textStore.getCaret();
    const text = textStore.getText().substring(0, caretPosition);
    const deadkeys = textStore.deadkeys().dks.sort((a, b) => a.p != b.p ? a.p - b.p : a.o - b.o);
    const contextItems = new KM_Core.instance.km_core_context_items();

    const deadkeyIterator = deadkeys.values();
    let deadkey = deadkeyIterator.next();
    let textIndex = 0;
    while (!deadkey.done || textIndex < text.length) {
      // flush out invalid deadkeys
      while (!deadkey.done && (deadkey.value.p < textIndex || deadkey.value.p > text.length)) {
        // this should never happen -- it would mean that a deadkey position was < 0, in the
        // middle of a surrogate pair, or after the caret.
        console.warn(`invalid deadkey '${deadkey.value.d}' position ${deadkey.value.p}`);
        deadkey = deadkeyIterator.next();
      }

      // insert 0 or more deadkeys at current index
      while (!deadkey.done && deadkey.value.p == textIndex) {
        const contextItem = new KM_Core.instance.km_core_context_item();
        contextItem.marker = deadkey.value.d;
        contextItems.push_back(contextItem);
        deadkey = deadkeyIterator.next();
      }

      // insert next character
      if (textIndex < text.length) {
        const contextItem = new KM_Core.instance.km_core_context_item();
        contextItem.character = text.codePointAt(textIndex);
        contextItems.push_back(contextItem);
        textIndex++;
        if (contextItem.character > 0xFFFF) {
          // we have a surrogate pair, skip other half of surrogate, codePointAt()
          // already handled that for us
          textIndex++;
        }
      }
    }

    // Add end element
    contextItems.push_back(KM_Core.instance.create_end_context());

    KM_Core.instance.context_set(context, contextItems);
  }

  /**
   * Saves marker entries from Core's context into a TextStore's deadkey list.
   *
   * @param context     Context from Keyman Core
   * @param textStore   Web TextStore
   */
  private saveMarkersToTextStore(context: km_core_context, textStore: TextStore): void {
    const { status, object } = KM_Core.instance.context_get(context);
    if (status != KM_CORE_STATUS.OK) {
      console.error('KeymanWeb: km_core_context_get failed with status: ' + status);
      return;
    }
    textStore.deadkeys().clear();
    let textIndex = 0;
    const contextItems: km_core_context_items = object;
    for (let i = 0; i < contextItems.size(); i++) {
      const contextItem = contextItems.get(i);
      if (contextItem.type !== KM_CORE_CT.MARKER) {
        textIndex++;
        if (contextItem.character > 0xFFFF) {
          // character will be a surrogate pair in the text store
          textIndex++;
        }
        continue;
      }
      textStore.deadkeys().add(new Deadkey(textIndex, contextItem.marker));
    }
  }

  /**
   * Processes a keystroke event and updates the text store accordingly.
   * Handles the main logic for interpreting and applying keyboard input.
   *
   * @param {KeyEvent}   keyEvent    The key event to process.
   * @param {TextStore}  textStore   The current text store context.
   *
   * @returns {ProcessorAction} The resulting processor action.
   */
  public processKeystroke(keyEvent: KeyEvent, textStore: TextStore): ProcessorAction {

    const preInput = SyntheticTextStore.from(textStore, true);
    const activeKeyboard = this.activeKeyboard as KMXKeyboard;
    const coreContext = KM_Core.instance.state_context(activeKeyboard.state);

    this.applyContextFromTextStore(coreContext, textStore);

    const status = KM_Core.instance.process_event(activeKeyboard.state, keyEvent.Lcode, keyEvent.Lmodifiers, keyEvent.source?.type === 'keydown', KM_CORE_EVENT_FLAG.DEFAULT);
    // TODO-web-core: properly set flags (#15283)
    if (status != KM_CORE_STATUS.OK) {
      console.error('KeymanWeb: km_core_process_event failed with status: ' + status);
      return null;
    }
    const processorAction = new ProcessorAction();
    const core_actions = KM_Core.instance.state_get_actions(activeKeyboard.state);

    textStore.deleteCharsBeforeCaret(core_actions.code_points_to_delete);
    textStore.insertTextBeforeCaret(core_actions.output);
    this.saveMarkersToTextStore(coreContext, textStore);

    processorAction.beep = core_actions.do_alert;
    processorAction.triggerKeyDefault = core_actions.emit_keystroke;

    // TODO-web-core: Implement options (#13426)
    // process_persist_action(engine, actions->persist_options);
    // TODO-web-core: do we have to do anything with the new_caps_lock_state? (#15285)
    // process_capslock_action(actions->new_caps_lock_state);

    processorAction.transcription = textStore.buildTranscriptionFrom(preInput, keyEvent, false);

    return processorAction;
  }

  /**
   * Processes post-keystroke actions for the given device and text store.
   * Handles any actions that should occur after a keystroke is processed.
   *
   * @param {DeviceSpec}  device     The device specification.
   * @param {TextStore}   textStore  The current text store context.
   *
   * @returns {ProcessorAction} The resulting processor action, or null if not applicable.
   */
  public processPostKeystroke(device: DeviceSpec, textStore: TextStore): ProcessorAction {
    // TODO-web-core: Implement this method (#15286)
    return null;
  }

  // TODO-web-core: this could be shared with JsKeyboardProcessor
  /**
   * Determines if the given key event is a modifier key press.
   * Returns true if the event corresponds to a modifier key, otherwise false.
   *
   * @param {KeyEvent}   keyEvent    The key event to evaluate.
   * @param {TextStore}  textStore   The current text store context.
   * @param {boolean}    isKeyDown   Indicates if the key event is a key down event.
   *
   * @returns {boolean} True if the event is a modifier key press, false otherwise.
   */
  public doModifierPress(keyEvent: KeyEvent, textStore: TextStore, isKeyDown: boolean): boolean {
    if(!this.activeKeyboard) {
      return false;
    }

    if(keyEvent.isModifier) {
      this.activeKeyboard.notify(keyEvent.Lcode, textStore, isKeyDown);
      // For eventual integration - we bypass an OSK update for physical keystrokes when in touch mode.
      if(!keyEvent.device.touchable) {
        return this._UpdateVKShift(keyEvent); // I2187
      } else {
        return true;
      }
    }

    if(keyEvent.LmodifierChange) {
      this.activeKeyboard.notify(0, textStore, true);
      if(!keyEvent.device.touchable) {
        this._UpdateVKShift(keyEvent);
      }
    }

    // No modifier keypresses detected.
    return false;
  }


  // TODO-web-core: this could be shared with JsKeyboardProcessor
  /**
   * Updates the virtual keyboard shift state based on the provided key event.
   * Handles modifier key simulation, state key updates, and layer selection for the OSK.
   *
   * @param {KeyEvent} e - The key event used to update the shift state.
   *
   * @returns {boolean} True if the update was processed, otherwise true if no active keyboard.
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
      // read shift states from event
      keyShiftState = e.Lmodifiers;

      // Are we simulating AltGr?  If it's a simulation and not real, time to un-simulate for the OSK.
      if(this.activeKeyboard.isChiral && this.activeKeyboard.emulatesAltGr &&
          (this.modStateFlags & Codes.modifierBitmasks['ALT_GR_SIM']) == Codes.modifierBitmasks['ALT_GR_SIM']) {
        keyShiftState |= Codes.modifierBitmasks['ALT_GR_SIM'];
        keyShiftState &= ~ModifierKeyConstants.RALTFLAG;
      }

      // Set stateKeys where corresponding value is passed in e.Lstates
      let stateMutation = false;
      for(let i=0; i < lockNames.length; i++) {
        if((e.Lstates & Codes.stateBitmasks[lockNames[i]]) != 0) {
          this.stateKeys[lockKeys[i]] = ((e.Lstates & lockModifiers[i]) != 0);
          stateMutation = true;
        }
      }

      if(stateMutation) {
        this.emit('statekeychange', this.stateKeys);
      }
    }

    this.updateStates();

    if (this.activeKeyboard.isMnemonic && this.stateKeys['K_CAPS'] && (!e || !e.isModifier)) {
      // Modifier keypresses don't trigger mnemonic manipulation of modifier state.
      // Only an output key does; active use of Caps will also flip the SHIFT flag.
      // Mnemonic keystrokes manipulate the SHIFT property based on CAPS state.
      // We need to unflip them when tracking the OSK layer.
      keyShiftState ^= ModifierKeyConstants.K_SHIFTFLAG;
    }

    this.layerId = this.getLayerId(keyShiftState);
    return true;
  }

  // TODO-web-core: this could be shared with JsKeyboardProcessor
  private updateStates(): void {
    const lockKeys = ['K_CAPS', 'K_NUMLOCK', 'K_SCROLL'] as const;
    const lockModifiers = [ModifierKeyConstants.CAPITALFLAG, ModifierKeyConstants.NUMLOCKFLAG, ModifierKeyConstants.SCROLLFLAG] as const;
    const noLockModifers = [ModifierKeyConstants.NOTCAPITALFLAG, ModifierKeyConstants.NOTNUMLOCKFLAG, ModifierKeyConstants.NOTSCROLLFLAG] as const;

    for (let i = 0; i < lockKeys.length; i++) {
      const key = lockKeys[i];
      const flag = this.stateKeys[key];

      // Ensures that the current mod-state info properly matches the currently-simulated
      // state key states.
      if (flag) {
        this.modStateFlags |= lockModifiers[i];
        this.modStateFlags &= ~noLockModifers[i];
      } else {
        this.modStateFlags &= ~lockModifiers[i];
        this.modStateFlags |= noLockModifers[i];
      }
    }
  }

  /**
   * Resets the keyboard context, optionally using the provided text store.
   * Clears or reinitializes the context for subsequent keyboard processing.
   *
   * @param {TextStore} [textStore] - The optional text store to use for resetting context.
   */
  public resetContext(textStore?: TextStore): void {}

  /**
   * Finalizes the processor action and applies any final changes to the text store.
   * Ensures that all necessary updates are completed after processing a key event.
   *
   * @param {ProcessorAction}   data        The processor action to finalize.
   * @param {TextStore}         textStore   The text store to update.
   */
  public finalizeProcessorAction(data: ProcessorAction, textStore: TextStore): void { }

  /**
   * Selects the next keyboard layer based on the provided key event.
   * Determines and applies the appropriate layer switch for the OSK.
   *
   * @param {KeyEvent}  keyEvent   The key event used to determine the next layer.
   *
   * @returns {boolean} True if the keyboard layer changed, false otherwise.
   */
  public selectLayer(keyEvent: KeyEvent): boolean {
    // TODO-web-core: Implement this method (#15284)
    return false;
  }

  /**
   * Sets the numeric layer for the given device.
   * Switches the keyboard to a numeric input layer if supported.
   *
   * @param {DeviceSpec} device - The device for which to set the numeric layer.
   */
  public setNumericLayer(device: DeviceSpec): void {}


  /** @internal */
  public unitTestEndPoints = {
    saveMarkersToTextStore: this.saveMarkersToTextStore.bind(this),
    applyContextFromTextStore: this.applyContextFromTextStore.bind(this),
  };
}
