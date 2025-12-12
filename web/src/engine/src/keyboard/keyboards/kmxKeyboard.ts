import { DeviceSpec } from 'keyman/common/web-utils';
import {
  KM_Core, km_core_keyboard, KM_CORE_KMX_ENV,
  KM_CORE_OPTION_SCOPE, km_core_state, KM_CORE_STATUS
} from 'keyman/engine/core-adapter';
import { ActiveKey, ActiveSubKey } from './activeLayout.js';
import { StateKeyMap } from './stateKeyMap.js';
import { KeyEvent } from '../keyEvent.js';
import { TextStore } from '../textStore.js';

/**
 * Acts as a wrapper class for KMX(+) Keyman keyboards
 */
export class KMXKeyboard {
  private _state: km_core_state;

  public constructor(private _keyboard: km_core_keyboard) {
    const environment_opts =
      [
        {
          scope: KM_CORE_OPTION_SCOPE.OPT_ENVIRONMENT,
          key: KM_CORE_KMX_ENV.PLATFORM,
          // TODO-web-core: Turn touch off for non-touch targets; read proper platform string from web (#15289)
          value: "web iphone ipad androidphone androidtablet mobile touch hardware android phone"
        }, {
          scope: KM_CORE_OPTION_SCOPE.OPT_ENVIRONMENT,
          key: KM_CORE_KMX_ENV.BASELAYOUT,
          value: "kbdus.dll" // TODO: Base layout assumed to be US in v19
        }, {
          scope: KM_CORE_OPTION_SCOPE.OPT_ENVIRONMENT,
          key: KM_CORE_KMX_ENV.BASELAYOUTALT,
          value: "us", // TODO: Base layout assumed to be US in v19
        },{
          scope: KM_CORE_OPTION_SCOPE.OPT_ENVIRONMENT,
          key: KM_CORE_KMX_ENV.SIMULATEALTGR,
          value: "0" // TODO: We won't support simulating AltGr option in v19
        }
      ]
    const result = KM_Core.instance.state_create(_keyboard, environment_opts);
    if (result.status == KM_CORE_STATUS.OK) {
      this._state = result.object;
    }
  }

  public shutdown() {
    if (this._state) {
      this._state.delete();
      this._state = null;
    }
    if (this._keyboard) {
      KM_Core.instance.keyboard_dispose(this._keyboard);
      this._keyboard = null;
    }
  }

  public constructKeyEvent(key: ActiveKey | ActiveSubKey, device: DeviceSpec, stateKeys: StateKeyMap): KeyEvent {
    // TODO-web-core: Implement this method (#15290)
    return null;
  }

  public get isMnemonic(): boolean {
    return false;
  }

  public get version(): string {
    const attrs = KM_Core.instance.keyboard_get_attrs(this._keyboard);
    const version = attrs.object.version_string;
    attrs.delete();
    return version;
  }

  public get id(): string {
    const attrs = KM_Core.instance.keyboard_get_attrs(this._keyboard);
    const id = attrs.object.id;
    attrs.delete();
    return id;
  }

  public get keyboard(): km_core_keyboard {
    return this._keyboard;
  }

  public get state(): km_core_state {
    return this._state;
  }

  public get isChiral(): boolean {
    // TODO-web-core: Implement this method (#15290)
    return false;
  }

  /**
   * Signifies whether or not a layout or OSK should include AltGr / Right-alt emulation for this  keyboard.
   * @return  {boolean}
   */
  public get emulatesAltGr(): boolean {
    // TODO-web-core: Implement this method (#15290)
    return false;
  }

  /**
   * Notifies keyboard of keystroke or other event
   *
   * @param       {number}    eventCode     key code (16-18: Shift, Control or Alt),
   *                                        or 0 for focus
   * @param       {TextStore} textStore     textStore
   * @param       {number}    boolean       true for KeyDown or FocusReceived,
   *                                        false for KeyUp or FocusLost
   */
  public notify(eventCode: number, textStore: TextStore, data: boolean): void { // I2187
    // TODO-web-core: do we need to support this? (#15290)
  }

}
