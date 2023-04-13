import { Keyboard, KeyboardHarness, KeyboardLoaderBase, KeyboardLoadErrorBuilder, MinimalKeymanGlobal } from '@keymanapp/keyboard-processor';

import vm from 'vm';
import fs from 'fs';
import { globalObject } from '@keymanapp/web-utils';

export class NodeKeyboardLoader extends KeyboardLoaderBase {
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

  protected loadKeyboardInternal(uri: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<Keyboard> {
    // `fs` does not like 'file:///'; it IS "File System" oriented, after all, and wants a path, not a URI.
    if(uri.indexOf('file:///') == 0) {
      uri = uri.substring('file:///'.length);
    }

    let script;
    try {
      script = new vm.Script(fs.readFileSync(uri).toString());
    } catch (err) {
      return Promise.reject(errorBuilder.missingError(err));
    }
    try {
      script.runInContext(this.harness._jsGlobal);
    } catch (err) {
      return Promise.reject(errorBuilder.scriptError(err));
    }

    const keyboard = this.harness.loadedKeyboard;
    this.harness.loadedKeyboard = null;
    return Promise.resolve(keyboard);
  }
}