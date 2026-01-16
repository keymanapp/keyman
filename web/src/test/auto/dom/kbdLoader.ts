import {
  DOMKeyboardLoader
} from 'keyman/engine/keyboard';

import {
  JSKeyboard,
  KeyboardProperties,
  MinimalKeymanGlobal
} from 'keyman/engine/keyboard';

import { JSKeyboardInterface } from 'keyman/engine/js-processor';
import { KeyboardInfoPair } from 'keyman/engine/main';

const loader = new DOMKeyboardLoader(new JSKeyboardInterface(window, MinimalKeymanGlobal));

export function loadKeyboardFromPath(path: string) {
  return loader.loadKeyboardFromPath(path);
}

export type KeyboardMap = {
  [key: string]: KeyboardInfoPair & { keyboard: JSKeyboard }
};

export function loadKeyboardsFromStubs(apiStubs: any, baseDir: string) {
  baseDir = baseDir || './';
  const keyboards: KeyboardMap = {};
  let priorPromise: Promise<void | JSKeyboard> = Promise.resolve();
  for(const stub of apiStubs) {
    // We are keeping this strictly sequential because we don't have sandboxed
    // loading yet; lack of sandboxing means that all loading keyboards compete
    // for the same harness endpoint when loading.
    //
    // tl;dr: because otherwise, race condition.
    priorPromise = priorPromise.then(() => {
      // Adds closure to capture 'id' for async completion use.
      const overwriteLoader = (id: number, path: string) => {
        const loader = loadKeyboardFromPath(path);
        return loader.then(kbd => {
          if (kbd instanceof JSKeyboard) {
            keyboards[stub.id].keyboard = kbd;
          } else {
            // TODO-web-core: implement for KMX keyboards if needed
            keyboards[stub.id].keyboard = null;
          }
        });
      }
      keyboards[stub.id] = {
        // @ts-ignore - we temporarily allow it to be a Promise, until the overall Promise resolves.
        keyboard: overwriteLoader(stub.id, baseDir + stub.filename),
        // @ts-ignore
        metadata: new KeyboardProperties(stub)
      };

      // Because tests closer to top-level KMW will expect a filename entry.
      // @ts-ignore // .filename is readonly
      keyboards[stub.id].metadata.filename = baseDir + stub.filename;

      return keyboards[stub.id].keyboard;
    });
  }

  return priorPromise.then(() => keyboards);
}