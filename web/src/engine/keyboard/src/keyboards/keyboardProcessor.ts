/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { EventEmitter } from 'eventemitter3';
import { DeviceSpec } from '@keymanapp/web-utils';
import { KeyEvent } from '../keyEvent.js';
import { OutputTargetInterface } from '../outputTargetInterface.js';
import { type MutableSystemStore } from "../systemStore.js";
import { Keyboard } from './keyboardLoaderBase.js';
import { KeyboardMinimalInterface } from './keyboardMinimalInterface.js';
import { ProcessorAction } from './processorAction.js';
import { StateKeyMap } from './stateKeyMap.js';

export interface EventMap {
  statekeychange: (stateKeys: StateKeyMap) => void;
}

export type BeepHandler = (outputTarget: OutputTargetInterface) => void;

export interface KeyboardProcessor extends EventEmitter<EventMap> {
  // public static readonly DEFAULT_OPTIONS: ProcessorInitOptions = {
  //   baseLayout: 'us',
  //   defaultOutputRules: new DefaultRules()
  // };

  // Tracks the simulated value for supported state keys, allowing the OSK to mirror a physical keyboard for them.
  // Using the exact keyCode name from the Codes definitions will allow for certain optimizations elsewhere in the code.
  stateKeys: StateKeyMap;

  /**
  * Indicates the device (platform) to be used for non-keystroke events,
  * such as those sent to `begin postkeystroke` and `begin newcontext`
  * entry points.
  */
  contextDevice: DeviceSpec;

  beepHandler?: BeepHandler;
  baseLayout: string;

  // Tracks the most recent modifier state information in order to quickly detect changes
  // in keyboard state not otherwise captured by the hosting page in the browser.
  // Needed for AltGr simulation.
  modStateFlags: number;

  get activeKeyboard(): Keyboard;

  set activeKeyboard(keyboard: Keyboard);


  get keyboardInterface(): KeyboardMinimalInterface

  get layerStore(): MutableSystemStore;

  get newLayerStore(): MutableSystemStore;

  get oldLayerStore(): MutableSystemStore;

  get layerId(): string;
  set layerId(value: string);

  processPostKeystroke(device: DeviceSpec, outputTarget: OutputTargetInterface): ProcessorAction;

  processKeystroke(keyEvent: KeyEvent, outputTarget: OutputTargetInterface): ProcessorAction;

  /**
   * Select the OSK's next keyboard layer based upon layer switching keys as a default
   * The next layer will be determined from the key name unless otherwise specifed
   *
   *  @param  {string}                    keyName     key identifier
   *  @return {boolean}                               return true if keyboard layer changed
   */
  selectLayer(keyEvent: KeyEvent): boolean;

  // Returns true if the key event is a modifier press, allowing keyPress to return selectively
  // in those cases.
  doModifierPress(Levent: KeyEvent, outputTarget: OutputTargetInterface, isKeyDown: boolean): boolean;

  resetContext(target?: OutputTargetInterface): void;

  setNumericLayer(device: DeviceSpec): void;

  finalizeProcessorAction(data: ProcessorAction, outputTarget: OutputTargetInterface): void;
}
