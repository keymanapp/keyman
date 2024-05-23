import {
  DOMKeyboardLoader
} from '@keymanapp/keyboard-processor/dom-keyboard-loader';

import {
  Keyboard,
  KeyboardInterface,
  KeyboardProperties,
  MinimalKeymanGlobal
} from '@keymanapp/keyboard-processor';

import { KeyboardStub } from 'keyman/engine/package-cache';

const loader = new DOMKeyboardLoader(new KeyboardInterface(window, MinimalKeymanGlobal));

export function loadKeyboardFromPath(path) {
  return loader.loadKeyboardFromPath(path);
}

export function loadKeyboardsFromStubs(apiStubs, baseDir) {
  baseDir = baseDir || './';
  let keyboards: {
    [key: string]: {
      keyboard: Keyboard,
      metadata: KeyboardStub
    }
  } = {};
  let priorPromise: Promise<void | Keyboard> = Promise.resolve();
  for(let stub of apiStubs) {
    // We are keeping this strictly sequential because we don't have sandboxed
    // loading yet; lack of sandboxing means that all loading keyboards compete
    // for the same harness endpoint when loading.
    //
    // tl;dr: because otherwise, race condition.
    priorPromise = priorPromise.then(() => {
      // Adds closure to capture 'id' for async completion use.
      let overwriteLoader = (id, path) => {
        let loader = loadKeyboardFromPath(path);
        return loader.then(kbd => {
          keyboards[stub.id].keyboard = kbd;
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