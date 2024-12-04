import { km_core_keyboard } from 'keyman/engine/core-processor';

/**
 * Acts as a wrapper class for KMX(+) Keyman keyboards
 */
export class KMXKeyboard {

  constructor(id: string, keyboard: km_core_keyboard) {
    this.id = id;
    this.keyboard = keyboard;
  }

  id: string;
  keyboard: km_core_keyboard;

  get isMnemonic(): boolean {
    return false;
  }

  get version(): string {
    // TODO-web-core: get version from `km_core_keyboard_get_attrs`
    return '';
  }
}
