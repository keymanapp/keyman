// TODO:  export from ../index.ts as well!

import { KeyboardHarness, MinimalKeymanGlobal } from './keyboardHarness.js';

import Keyboard from './keyboard.js';
import KeyboardLoaderBase from './keyboardLoaderBase.js';

import vm from 'vm';
import fs from 'fs';
import { globalObject } from '@keymanapp/web-utils';

export default class NodeKeyboardLoader extends KeyboardLoaderBase {
  constructor()
  constructor(harness: KeyboardHarness);
  constructor(harness?: KeyboardHarness) {
    if(!harness) {
      super(new KeyboardHarness(vm.createContext(), MinimalKeymanGlobal));
    } else {
      // If we're going to sandbox, make sure the sandbox is sufficient!
      if(globalObject() != harness._jsGlobal) {
        vm.createContext(harness._jsGlobal);
        // Ensure any and all string extensions are available within the sandbox context!
        harness._jsGlobal.String = globalObject().String;
      }

      super(harness);
    }
  }

  protected loadKeyboardInternal(uri: string): Promise<Keyboard> {
    try {
      const script = new vm.Script(fs.readFileSync(uri).toString());
      script.runInContext(this.harness._jsGlobal);
    } catch (err) {
      return Promise.reject(err);
    }

    return Promise.resolve(this.harness.activeKeyboard);
  }
}