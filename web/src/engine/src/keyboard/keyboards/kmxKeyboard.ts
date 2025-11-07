import { KM_Core, km_core_keyboard } from 'keyman/engine/core-processor';

/**
 * Acts as a wrapper class for KMX(+) Keyman keyboards
 */
export class KMXKeyboard {

  constructor(private _keyboard: km_core_keyboard) {
  }

  shutdown() {
    if (this._keyboard) {
      KM_Core.instance.keyboard_dispose(this._keyboard);
      this._keyboard = null;
    }
  }

  get isMnemonic(): boolean {
    return false;
  }

  get version(): string {
    const attrs = KM_Core.instance.keyboard_get_attrs(this._keyboard);
    const version = attrs.object.version_string;
    attrs.delete();
    return version;
  }

  get id(): string {
    const attrs = KM_Core.instance.keyboard_get_attrs(this._keyboard);
    const id = attrs.object.id;
    attrs.delete();
    return id;
  }

  get keyboard(): km_core_keyboard {
    return this._keyboard;
  }
}
