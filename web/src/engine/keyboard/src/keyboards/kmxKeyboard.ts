import { km_core_keyboard } from 'keyman/engine/core-processor';

/**
 * Acts as a wrapper class for KMX(+) Keyman keyboards
 */
export class KMXKeyboard {

  constructor(id: string, keyboard: km_core_keyboard) {
    this._id = id;
    this._keyboard = keyboard;
  }

  private _id: string;
  private _keyboard: km_core_keyboard;

  get isMnemonic(): boolean {
    return false;
  }

  get version(): string {
    // TODO-web-core: get version from `km_core_keyboard_get_attrs`
    return '';
  }

  get id(): string {
    return this._id;
  }

  get keyboard(): km_core_keyboard {
    return this._keyboard;
  }
}
