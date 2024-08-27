import { Keyboard, KeyboardProperties, Codes } from '../../../../../build/engine/osk/lib/index.mjs';

// // The following block would be sufficient to replace the `loadKeyboardFromPath` func below...
// // were it not for web/src/engine/keyboard/ being outside of the standard `localhost` config
// // used for manual Web testing.

// import {
//   DOMKeyboardLoader
// } from '../../../keyboard/build/lib/dom-keyboard-loader.mjs';
// import {
//   KeyboardInterface,
//   MinimalKeymanGlobal
// } from '../../../keyboard/build/lib/index.mjs';

// // This script may or may not be temporary; the KMW "KeyboardManager" class may be spun off in a manner
// // that could replace this.

// const loader = new DOMKeyboardLoader(new KeyboardInterface(window, MinimalKeymanGlobal));

// export function loadKeyboardFromPath(path) {
//   return loader.loadKeyboardFromPath(path);
// }

// This script may or may not be temporary; the KMW "KeyboardManager" class may be spun off in a manner
// that could replace this.

export function loadKeyboardFromPath(path) {
  const window_KeymanWeb = window['KeymanWeb'];
  const window_keyman = window['keyman'];

  const promise = new Promise((resolve, reject) => {
    const loader = window['KeymanWeb'] = {
      KR(kbdObj) {
        this.keyboard = kbdObj;
      },
      keyboard: null,
    };

    // Needed for some debug-compiled keyboards.  Also, the `keyman` top-level var itself is
    // needed for any KMW 10+ keyboards.
    window['keyman'] = {
      osk: {
        modifierCodes: Codes.modifierCodes,
        keyCodes: Codes.keyCodes
      }
    };

    const script = document.createElement('script');
    document.head.appendChild(script);
    script.onerror = reject;
    script.onload = () => {
      if(loader.keyboard) {
        resolve(loader.keyboard);
      } else {
        reject();
      }

      // https://stackoverflow.com/a/37393041 - totally safe.
      script.remove();
    }

    // Now that EVERYTHING ELSE is ready, establish the link to the keyboard's script.
    script.src = path;
  });

  promise.finally(() => {
    window['KeymanWeb'] = window_KeymanWeb;
    window['keyman'] = window_keyman;

    if(window['KeymanWeb'] === undefined) {
      delete window['KeymanWeb'];
    }

    if(window['keyman'] === undefined) {
      delete window['keyman'];
    }
  });

  return promise;
}

export function loadKeyboardsFromStubs(apiStubs) {
  let keyboards = {};
  let priorPromise = Promise.resolve();
  for(let stub of apiStubs) {
    priorPromise = priorPromise.then(() => {
      // Adds closure to capture 'id' for async completion use.
      let overwriteLoader = (id, path) => {
        let loader = loadKeyboardFromPath(path);
        return loader.then(kbd => {
          keyboards[stub.id].keyboard = new Keyboard(kbd)
        });
      }
      keyboards[stub.id] = {
        keyboard: overwriteLoader(stub.id, stub.filename),
        metadata: new KeyboardProperties(stub)
      };
      return keyboards[stub.id].keyboard;
    });
  }

  return priorPromise.then(() => keyboards);
}