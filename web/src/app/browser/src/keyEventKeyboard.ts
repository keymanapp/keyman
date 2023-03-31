import { DeviceSpec, KeyEvent, ManagedPromise } from '@keymanapp/keyboard-processor';

import { HardKeyboard } from 'keyman/engine/main';

export default class KeyEventKeyboard extends HardKeyboard {
  readonly baseDevice: DeviceSpec;

  constructor(baseDevice: DeviceSpec) {
    super();
    this.baseDevice = baseDevice;
  }

  // TODO:  actually implement
}