/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { EventEmitter } from 'eventemitter3';
import { KM_Core } from 'keyman/engine/core-adapter';
import {
  BeepHandler,
  DeviceSpec, EventMap, Keyboard, KeyboardMinimalInterface, KeyboardProcessor,
  KeyEvent, MutableSystemStore, OutputTargetInterface, ProcessorAction,
  StateKeyMap
} from "keyman/engine/keyboard";

export class CoreKeyboardProcessor extends EventEmitter<EventMap> implements KeyboardProcessor {
  private activeKeyboard_: Keyboard = null;
  private layerId_: string = null;

  public async init(basePath: string): Promise<void> {
    await KM_Core.createCoreProcessor(basePath);
  }

  // Tracks the simulated value for supported state keys, allowing the OSK to mirror a physical keyboard for them.
  // Using the exact keyCode name from the Codes definitions will allow for certain optimizations elsewhere in the code.
  public stateKeys: StateKeyMap = {
    "K_CAPS": false,
    "K_NUMLOCK": false,
    "K_SCROLL": false
  }

  /**
  * Indicates the device (platform) to be used for non-keystroke events,
  * such as those sent to `begin postkeystroke` and `begin newcontext`
  * entry points.
  */
  public contextDevice: DeviceSpec;

  public beepHandler?: BeepHandler;

  // Tracks the most recent modifier state information in order to quickly detect changes
  // in keyboard state not otherwise captured by the hosting page in the browser.
  // Needed for AltGr simulation.
  public modStateFlags: number = 0;
  public baseLayout: string;

  public get activeKeyboard(): Keyboard {
    return this.activeKeyboard_;
  }

  public set activeKeyboard(keyboard: Keyboard) {
    this.activeKeyboard_ = keyboard
  }

   get keyboardInterface(): KeyboardMinimalInterface {
    // TODO-web-core: Implement this method
    return null;
  }

  public get layerStore(): MutableSystemStore {
    // TODO-web-core: Implement this method
    return null;
  }

  public get newLayerStore(): MutableSystemStore {
    // TODO-web-core: Implement this method
    return null;
  }

  public get oldLayerStore(): MutableSystemStore {
    // TODO-web-core: Implement this method
    return null;
  }

  public get layerId(): string {
    return this.layerId_;
  }
  public set layerId(value: string) {
    this.layerId_ = value;
  }

  public processPostKeystroke(device: DeviceSpec, outputTarget: OutputTargetInterface): ProcessorAction {
    // TODO-web-core: Implement this method
    return null;
  }

  public processKeystroke(keyEvent: KeyEvent, outputTarget: OutputTargetInterface): ProcessorAction {
    // TODO-web-core: Implement this method
    return null;
  }

  /**
   * Select the OSK's next keyboard layer based upon layer switching keys as a default
   * The next layer will be determined from the key name unless otherwise specifed
   *
   *  @param  {string}                    keyName     key identifier
   *  @return {boolean}                               return true if keyboard layer changed
   */
  public selectLayer(keyEvent: KeyEvent): boolean {
    // TODO-web-core: Implement this method
    return false;
  }

  // Returns true if the key event is a modifier press, allowing keyPress to return selectively
  // in those cases.
  public doModifierPress(Levent: KeyEvent, outputTarget: OutputTargetInterface, isKeyDown: boolean): boolean {
    // TODO-web-core: Implement this method
    return false;
  }

  public resetContext(target?: OutputTargetInterface): void {}

  public setNumericLayer(device: DeviceSpec): void {}

  public finalizeProcessorAction(data: ProcessorAction, outputTarget: OutputTargetInterface): void {}
}
