/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import {
  DeviceSpec, Keyboard, KeyboardMinimalInterface, KeyboardProcessor,
  KeyEvent, MutableSystemStore, OutputTargetInterface, ProcessorAction
} from "keyman/engine/keyboard";
import { KM_Core } from 'keyman/engine/core-adapter';

export class CoreKeyboardProcessor implements KeyboardProcessor {
  public async init(basePath: string): Promise<void> {
    await KM_Core.createCoreProcessor(basePath);
  }

  public get activeKeyboard(): Keyboard {
    // TODO-web-core: Implement this method
    return null;
  }

  public set activeKeyboard(keyboard: Keyboard) {}

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
    // TODO-web-core: Implement this method
    return '';
  }
  public set layerId(value: string) {}

  // processPostKeystroke(device: DeviceSpec, outputTarget: OutputTargetInterface): ProcessorAction;

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
