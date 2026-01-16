/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { EventEmitter } from 'eventemitter3';
import { DeviceSpec } from 'keyman/common/web-utils';
import { KeyEvent } from '../keyEvent.js';
import { type MutableSystemStore } from "../systemStore.js";
import { Keyboard } from './keyboardLoaderBase.js';
import { KeyboardMinimalInterface } from './keyboardMinimalInterface.js';
import { ProcessorAction } from './processorAction.js';
import { StateKeyMap } from './stateKeyMap.js';
import { TextStore } from '../textStore.js';

export interface EventMap {
  statekeychange: (stateKeys: StateKeyMap) => void;
}

export type BeepHandler = (textStore: TextStore) => void;

/**
 * Interface for the keyboard processing engine used by the web runtime to
 * translate low-level key events and device/context signals into high-level
 * keyboard actions and text-store updates.
 *
 * This interface extends an EventEmitter of EventMap and centralizes responsibilities
 * such as:
 * - tracking simulated state keys so the on‑screen keyboard (OSK) can mirror hardware state,
 * - selecting and switching keyboard layers for the OSK,
 * - handling modifier simulation (including AltGr),
 * - routing non-keystroke device/context events,
 * - coordinating mutable layer state stores,
 * - performing keystroke and post‑keystroke processing and finalizing ProcessorAction
 *   results against a TextStore.
 */
export interface KeyboardProcessor extends EventEmitter<EventMap> {
  /**
   * Tracks the simulated values for supported state keys (e.g. CapsLock, NumLock)
   * so that the OSK can reflect a physical keyboard's state. Keys are referenced
   * using the exact keyCode names from the Codes definitions to allow for optimized
   * handling elsewhere.
   *
   * @type {StateKeyMap}
   */
  stateKeys: StateKeyMap;

  /**
   * Indicates the device/platform to be used for non-keystroke events (for example,
   * events dispatched to "begin postkeystroke" and "begin newcontext" entry points).
   * This lets the processor adapt behavior or rule evaluation to the active device.
   *
   * @type {DeviceSpec}
   */
  contextDevice: DeviceSpec;

  /**
   * Optional handler used to produce an audible beep or other feedback when a rule
   * or keyboard action requests it.
   *
   * @type {BeepHandler | undefined}
   */
  beepHandler?: BeepHandler;

  /**
   * Stores the identifier for the base physical layout in use (for
   * example "us"). Used to determine the default layout for key mapping
   * and processing.
   *
   * @type {string}
   */
  baseLayout: string;

  /**
   * Bitfield representing the most recent modifier state (Alt, Ctrl, Shift, etc.)
   * as observed or simulated by the processor. Used to quickly detect changes in
   * modifier state not otherwise captured by the hosting page (important for AltGr).
   *
   * @type {number}
   */
  modStateFlags: number;

  /**
   * The currently active Keyboard instance. Implementations provide a getter and
   * setter to change the active keyboard at runtime. Setting a new keyboard should
   * update any associated stores and clear or reinitialize processor state as needed.
   *
   * @type {Keyboard}
   */
  get activeKeyboard(): Keyboard;

  set activeKeyboard(keyboard: Keyboard);


  /**
   * Read-only minimal interface for interacting with the current keyboard.
   *
   * @type {KeyboardMinimalInterface}
   */
  get keyboardInterface(): KeyboardMinimalInterface

  /**
   * The store representing the currently active keyboard layer.
   *
   * @type {MutableSystemStore}
   */
  get layerStore(): MutableSystemStore;

  /**
   * A writable store used when transitioning to a new layer; allows
   * accumulation of changes before committing them to layerStore.
   *
   * @type {MutableSystemStore}
   */
  get newLayerStore(): MutableSystemStore;

  /**
   * A store representing the previously active layer; useful for reverting or
   * comparing layer states when switching layers.
   *
   * @type {MutableSystemStore}
   */
  get oldLayerStore(): MutableSystemStore;

  /**
   * Identifier of the currently active layer. Implementations provide getter and
   * setter access. Setting this value should trigger the appropriate layer store
   * updates and notify any listeners of the change.
   *
   * @type {string}
   */
  get layerId(): string;
  set layerId(value: string);

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
  processKeystroke(keyEvent: KeyEvent, textStore: TextStore): ProcessorAction;

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
  processPostKeystroke(device: DeviceSpec, textStore: TextStore): ProcessorAction;

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
  doModifierPress(keyEvent: KeyEvent, textStore: TextStore, isKeyDown: boolean): boolean;

  /**
   * Resets the processor's context to a clean state.
   * May clear stores, simulated state keys, and modifier flags. If a TextStore is provided,
   * it will also be reset.
   *
   * @param {TextStore} [textStore] - The optional text store to use for resetting context.
   */
  resetContext(textStore?: TextStore): void;

  /**
   * Finalizes the processor action and applies any final changes to the text store.
   * Ensures that all necessary updates are completed after processing a key event.
   *
   * @param {ProcessorAction}   data        The processor action to finalize.
   * @param {TextStore}         textStore   The text store to update.
   */
  finalizeProcessorAction(data: ProcessorAction, textStore: TextStore): void;

  /**
   * Selects the OSK's next keyboard layer based upon layer switching keys.
   * By default, the next layer is determined from the key name unless otherwise
   * specified. Returns true if the active keyboard layer changed.
   *
   * @param {KeyEvent} keyEvent - Key identifier or event used to determine layer change.
   *
   * @returns {boolean} True if the keyboard layer changed.
   */
  selectLayer(keyEvent: KeyEvent): boolean;

  /**
   *
   * Select the numeric layer if the provided device contains one.
   *
   * @param {DeviceSpec} device - The device for which the numeric layer should be set.
   */
  setNumericLayer(device: DeviceSpec): void;

}
