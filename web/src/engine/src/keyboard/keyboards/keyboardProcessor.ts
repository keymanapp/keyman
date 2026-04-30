/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { EventEmitter } from 'eventemitter3';
import { ModifierKeyConstants } from '@keymanapp/common-types';
import { DeviceSpec } from 'keyman/common/web-utils';
import { Codes } from '../codes.js';
import { KeyEvent } from '../keyEvent.js';
import { type MutableSystemStore } from "../systemStore.js";
import { Keyboard } from './keyboard.js';
import { KeyboardMinimalInterface } from './keyboardMinimalInterface.js';
import { ProcessorAction } from './processorAction.js';
import { StateKeyMap } from './stateKeyMap.js';
import { TextStore } from '../textStore.js';

export interface EventMap {
  statekeychange: (stateKeys: StateKeyMap) => void;
}

export type BeepHandler = (textStore: TextStore) => void;

const lockNames = ['CAPS', 'NUM_LOCK', 'SCROLL_LOCK'] as const;
const lockKeys = ['K_CAPS', 'K_NUMLOCK', 'K_SCROLL'] as const;
const lockModifiers = [ModifierKeyConstants.CAPITALFLAG, ModifierKeyConstants.NUMLOCKFLAG, ModifierKeyConstants.SCROLLFLAG] as const;
const noLockModifers = [ModifierKeyConstants.NOTCAPITALFLAG, ModifierKeyConstants.NOTNUMLOCKFLAG, ModifierKeyConstants.NOTSCROLLFLAG] as const;

export type KeyboardProcessor = AbstractKeyboardProcessor<Keyboard, KeyboardMinimalInterface>;

/**
 * Abstract class for the keyboard processing engine used by the web runtime to
 * translate low-level key events and device/context signals into high-level
 * keyboard actions and text-store updates.
 *
 * This class extends an EventEmitter of EventMap and centralizes responsibilities
 * such as:
 * - tracking simulated state keys so the on‑screen keyboard (OSK) can mirror hardware state,
 * - selecting and switching keyboard layers for the OSK,
 * - handling modifier simulation (including AltGr),
 * - routing non-keystroke device/context events,
 * - coordinating mutable layer state stores,
 * - performing keystroke and post‑keystroke processing and finalizing ProcessorAction
 *   results against a TextStore.
 */
export abstract class AbstractKeyboardProcessor<TKeyboard extends Keyboard, TKeyboardInterface extends KeyboardMinimalInterface> extends EventEmitter<EventMap> {
  public constructor(device: DeviceSpec, baseLayout: string, protected _keyboardInterface: TKeyboardInterface | null) {
    super();
    this.contextDevice = device;
    this.baseLayout = baseLayout;
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
  };

  /**
   * Indicates the device (platform) to be used for non-keystroke events.
   * Used for events such as those sent to `begin postkeystroke` and
   * `begin newcontext` entry points.
   *
   * @type {DeviceSpec}
   */
  public contextDevice: DeviceSpec;

  /**
   * Optional handler used to produce an audible beep or other feedback when a rule
   * or keyboard action requests it.
   *
   * @type {BeepHandler | undefined}
   */
  public beepHandler?: BeepHandler;

  /**
   * Stores the identifier for the base physical layout in use (for
   * example "us"). Used to determine the default layout for key mapping
   * and processing.
   *
   * @type {string}
   */
  public baseLayout: string;

  /**
   * Bitfield representing the most recent modifier state (Alt, Ctrl, Shift, etc.)
   * as observed or simulated by the processor. Used to quickly detect changes in
   * modifier state not otherwise captured by the hosting page (important for AltGr).
   *
   * @type {number}
   */
  public modStateFlags: number = 0;

  /**
   * The currently active Keyboard instance. Implementations provide a getter and
   * setter to change the active keyboard at runtime. Setting a new keyboard should
   * update any associated stores and clear or reinitialize processor state as needed.
   *
   * @type {TKeyboard}
   */
  public get activeKeyboard(): TKeyboard {
    return this.keyboardInterface.activeKeyboard as TKeyboard;
  }

  public set activeKeyboard(keyboard: TKeyboard) {
    this.keyboardInterface.activeKeyboard = keyboard;

    // All old deadkeys and keyboard-specific cache should immediately be invalidated
    // on a keyboard change.
    this.resetContext();
  }

  /**
   * The keyboard interface used by the processor.
   * Provides access to the keyboard interface implementation.
   *
   * @type {TKeyboardInterface}
   */
  public get keyboardInterface(): TKeyboardInterface {
    if (!this._keyboardInterface) {
      throw new Error('Keyboard interface not initialized');
    }
    return this._keyboardInterface;
  }

  /**
   * The store representing the currently active keyboard layer.
   *
   * @type {MutableSystemStore}
   */
  public abstract get layerStore(): MutableSystemStore;

  /**
   * A writable store used when transitioning to a new layer; allows
   * accumulation of changes before committing them to layerStore.
   *
   * @type {MutableSystemStore}
   */
  public abstract get newLayerStore(): MutableSystemStore;

  /**
   * A store representing the previously active layer; useful for reverting or
   * comparing layer states when switching layers.
   *
   * @type {MutableSystemStore}
   */
  public abstract get oldLayerStore(): MutableSystemStore;

  /**
   * Identifier of the currently active layer. Implementations provide getter and
   * setter access. Setting this value should trigger the appropriate layer store
   * updates and notify any listeners of the change.
   *
   * @type {string}
   */
  public abstract get layerId(): string;
  public abstract set layerId(value: string);

  /**
   * Process a keystroke, i.e. the `begin Unicode` group.
   * Evaluates keyboard rules, updates internal state, and returns a ProcessorAction
   * describing the changes to apply to the TextStore.
   *
   * @param {KeyEvent}   keyEvent   The key event to process.
   * @param {TextStore}  textStore  The text store representing current context.
   *
   * @returns {ProcessorAction} The resulting processor action.
   */
  public abstract processKeystroke(keyEvent: KeyEvent, textStore: TextStore): ProcessorAction;

  /**
   * Processes the `begin PostKeystroke` group.
   * This is used to evaluate rules that run after a keystroke has been
   * processed and can produce actions that affect the TextStore, layer state,
   * or other side effects.
   *
   * @param {DeviceSpec} device     The device context.
   * @param {TextStore}  textStore  The text store representing current context.
   *
   * @returns {ProcessorAction} The resulting processor action.
   */
  public abstract processPostKeystroke(device: DeviceSpec, textStore: TextStore): ProcessorAction;

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
    if (!this.activeKeyboard) {
      return false;
    }

    if (keyEvent.isModifier) {
      this.activeKeyboard.notify(keyEvent.Lcode, textStore, isKeyDown ? 1 : 0);
      // For eventual integration - we bypass an OSK update for physical keystrokes when in touch mode.
      if (!keyEvent.device.touchable) {
        return this.updateShiftState(keyEvent); // I2187
      } else {
        return true;
      }
    }

    if (keyEvent.LmodifierChange) {
      this.activeKeyboard.notify(0, textStore, 1);
      if (!keyEvent.device.touchable) {
        this.updateShiftState(keyEvent);
      }
    }

    // No modifier keypresses detected.
    return false;
  }

  /**
   * Updates the virtual keyboard shift state based on the provided key event.
   * Handles modifier key simulation, state key updates, and layer selection for the OSK.
   *
   * @param {KeyEvent | null} e - The key event used to update the shift state.
   *
   * @returns {boolean} True if the update was processed, otherwise true if no active keyboard.
   */
  protected updateShiftState(e: KeyEvent | null): boolean {
    let keyShiftState = 0;

    if (!this.activeKeyboard) {
      return true;
    }

    if (e) {
      // read shift states from event
      keyShiftState = e.Lmodifiers;

      // Are we simulating AltGr?  If it's a simulation and not real, time to un-simulate for the OSK.
      if (this.activeKeyboard.isChiral && this.activeKeyboard.emulatesAltGr &&
        (this.modStateFlags & Codes.modifierBitmasks['ALT_GR_SIM']) == Codes.modifierBitmasks['ALT_GR_SIM']) {
        keyShiftState |= Codes.modifierBitmasks['ALT_GR_SIM'];
        keyShiftState &= ~ModifierKeyConstants.RALTFLAG;
      }

      // Set stateKeys where corresponding value is passed in e.Lstates
      let stateMutation = false;
      for (let i = 0; i < lockNames.length; i++) {
        if ((e.Lstates & Codes.stateBitmasks[lockNames[i]]) != 0) {
          this.stateKeys[lockKeys[i]] = ((e.Lstates & lockModifiers[i]) != 0);
          stateMutation = true;
        }
      }

      if (stateMutation) {
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

  private updateStates(): void {
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

  protected abstract getLayerId(modifier: number): string;

  /**
   * Resets the processor's context to a clean state.
   * May clear stores, simulated state keys, and modifier flags. If a TextStore is provided,
   * it will also be reset.
   *
   * @param {TextStore} [textStore] - The optional text store to use for resetting context.
   */
  public abstract resetContext(textStore?: TextStore): void;

  /**
   * Finalizes the processor action and applies any final changes to the text store.
   * Ensures that all necessary updates are completed after processing a key event.
   *
   * @param {ProcessorAction}   data        The processor action to finalize.
   * @param {TextStore}         textStore   The text store to update.
   */
  public abstract finalizeProcessorAction(data: ProcessorAction, textStore: TextStore): void;

  /**
   * Selects the OSK's next keyboard layer based upon layer switching keys.
   * By default, the next layer is determined from the key name unless otherwise
   * specified. Returns true if the active keyboard layer changed.
   *
   * @param {KeyEvent} keyEvent - Key identifier or event used to determine layer change.
   *
   * @returns {boolean} True if the keyboard layer changed.
   */
  public abstract selectLayer(keyEvent: KeyEvent): boolean;

  /**
   *
   * Select the numeric layer if the provided device contains one.
   *
   * @param {DeviceSpec} device - The device for which the numeric layer should be set.
   */
  public abstract setNumericLayer(device: DeviceSpec): void;

}
