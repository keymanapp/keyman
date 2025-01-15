import vm from 'node:vm';
import { readFile } from 'node:fs/promises';

import { globalObject } from '@keymanapp/web-utils';

import { JSKeyboard } from '../jsKeyboard.js';
import { KeyboardHarness, MinimalKeymanGlobal } from '../keyboardHarness.js';
import { KeyboardLoaderBase } from '../keyboardLoaderBase.js';
import { KeyboardLoadErrorBuilder } from '../keyboardLoadError.js';

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

  protected async loadKeyboardBlob(uri: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<Uint8Array> {
    // `fs` does not like 'file:///'; it IS "File System" oriented, after all, and wants a path, not a URI.
    if (uri.indexOf('file:///') == 0) {
      uri = uri.substring('file:///'.length);
    }

    let buffer: Buffer;
    try {
      buffer = await readFile(uri);
    } catch (err) {
      throw errorBuilder.keyboardDownloadError(err);
    }
    return Uint8Array.from(buffer);
  }

  protected async loadKeyboardFromScript(scriptSrc: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<JSKeyboard> {
    let script;
    try {
      script = new vm.Script(scriptSrc);
    } catch (err) {
      throw errorBuilder.invalidKeyboard(err);
    }
    try {
      script.runInContext(this.harness._jsGlobal);
    } catch (err) {
      throw errorBuilder.scriptError(err);
    }

    const keyboard = this.harness.loadedKeyboard;
    this.harness.loadedKeyboard = null;
    return Promise.resolve(keyboard);
  }
}