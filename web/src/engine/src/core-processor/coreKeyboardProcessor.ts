/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { EventEmitter } from 'eventemitter3';
import { KM_Core, KM_CORE_STATUS } from 'keyman/engine/core-adapter';
import {
  BeepHandler,
  DeviceSpec, EventMap, Keyboard, KeyboardMinimalInterface, KeyboardProcessor,
  KeyEvent, KMXKeyboard, SyntheticTextStore, MutableSystemStore, TextStore, ProcessorAction,
  StateKeyMap
} from "keyman/engine/keyboard";

export class CoreKeyboardInterface implements KeyboardMinimalInterface {
  public activeKeyboard: Keyboard;

  constructor() {

  }
}

export class CoreKeyboardProcessor extends EventEmitter<EventMap> implements KeyboardProcessor {
  private _newLayerStore: MutableSystemStore = new MutableSystemStore(0, 'default');
  private _oldLayerStore: MutableSystemStore = new MutableSystemStore(0, 'default');
  private _layerStore: MutableSystemStore = new MutableSystemStore(0, 'default');
  private _keyboardInterface: CoreKeyboardInterface = new CoreKeyboardInterface();

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
    return this.keyboardInterface.activeKeyboard;
  }

  public set activeKeyboard(keyboard: Keyboard) {
    this.keyboardInterface.activeKeyboard = keyboard;
  }

  get keyboardInterface(): KeyboardMinimalInterface {
    return this._keyboardInterface;
  }

  public get layerStore(): MutableSystemStore {
    // TODO-web-core: link to .kmx layer store
    return this._layerStore;
  }

  public get newLayerStore(): MutableSystemStore {
    // TODO-web-core: link to .kmx new-layer store
    return this._newLayerStore;
  }

  public get oldLayerStore(): MutableSystemStore {
    // TODO-web-core: link to .kmx old-layer store
    return this._oldLayerStore;
  }

  public get layerId(): string {
    return this._layerStore.value;
  }
  public set layerId(value: string) {
    this._layerStore.set(value);
  }

  public processPostKeystroke(device: DeviceSpec, textStore: TextStore): ProcessorAction {
    // TODO-web-core: Implement this method
    return null;
  }

  public processKeystroke(keyEvent: KeyEvent, textStore: TextStore): ProcessorAction {
    const preInput = SyntheticTextStore.from(textStore, true);
    const activeKeyboard = this.activeKeyboard as KMXKeyboard;
    const status = KM_Core.instance.process_event(activeKeyboard.state, keyEvent.Lcode, keyEvent.Lmodifiers, 1, 0); // TODO-web-core: properly set keyDown and flags
    if (status != KM_CORE_STATUS.OK) {
      console.error('KeymanWeb: km_core_process_event failed with status: ' + status);
      return null;
    }
    const processorAction = new ProcessorAction();
    const core_actions = KM_Core.instance.state_get_actions(activeKeyboard.state);

    textStore.deleteCharsBeforeCaret(core_actions.code_points_to_delete);
    textStore.insertTextBeforeCaret(core_actions.output);
    processorAction.beep = core_actions.do_alert;
    processorAction.triggerKeyDefault = core_actions.emit_keystroke;

    // TODO-web-core: Implement options
    // process_persist_action(engine, actions->persist_options);
    // TODO-web-core: do we have to do anything with the new_caps_lock_state?
    // process_capslock_action(actions->new_caps_lock_state);

    processorAction.transcription = textStore.buildTranscriptionFrom(preInput, keyEvent, false);

    return processorAction;
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
  public doModifierPress(Levent: KeyEvent, textStore: TextStore, isKeyDown: boolean): boolean {
    // TODO-web-core: Implement this method
    return false;
  }

  public resetContext(textStore?: TextStore): void {}

  public setNumericLayer(device: DeviceSpec): void {}

  public finalizeProcessorAction(data: ProcessorAction, textStore: TextStore): void {}
}
