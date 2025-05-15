/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { KeyEvent } from '../keyEvent.js';
import { OutputTargetInterface } from '../outputTargetInterface.js';
import { type MutableSystemStore } from "../systemStores.js";
import { Keyboard } from './keyboardLoaderBase.js';
import { RuleBehavior } from './ruleBehavior.js';
import { DeviceSpec } from '@keymanapp/web-utils';

export interface KeyboardProcessor {
  // public static readonly DEFAULT_OPTIONS: ProcessorInitOptions = {
  //   baseLayout: 'us',
  //   defaultOutputRules: new DefaultRules()
  // };

  get activeKeyboard(): Keyboard;

  set activeKeyboard(keyboard: Keyboard);

  get layerStore(): MutableSystemStore;

  get newLayerStore(): MutableSystemStore;

  get oldLayerStore(): MutableSystemStore;

  get layerId(): string;
  set layerId(value: string);

  // processPostKeystroke(device: DeviceSpec, outputTarget: OutputTargetInterface): RuleBehavior;

  // processKeystroke(keyEvent: KeyEvent, outputTarget: OutputTargetInterface): RuleBehavior;

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

  finalize(data: RuleBehavior, outputTarget: OutputTargetInterface): void;
  mergeInDefaults(first: RuleBehavior, other: RuleBehavior): void;
}
