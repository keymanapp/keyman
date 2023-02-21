// Enables DOM types, but just for this one module.

///<reference lib="dom" />

import { Keyboard, KeyboardHarness, KeyboardLoaderBase, MinimalKeymanGlobal } from '@keymanapp/keyboard-processor';

import { ManagedPromise } from '@keymanapp/web-utils';

export class DOMKeyboardLoader extends KeyboardLoaderBase {
  public readonly element: HTMLIFrameElement;

  constructor()
  constructor(harness: KeyboardHarness);
  constructor(harness?: KeyboardHarness) {
    if(harness && harness._jsGlobal != window) {
      // Copy the String typing over; preserve string extensions!
      harness._jsGlobal['String'] = window['String'];
    }

    if(!harness) {
      super(new KeyboardHarness(window, MinimalKeymanGlobal));
    } else {
      super(harness);
    }
  }

  protected loadKeyboardInternal(uri: string): Promise<Keyboard> {
    const promise = new ManagedPromise<Keyboard>();

    try {
      const document = this.harness._jsGlobal.document;
      const script = document.createElement('script');
      document.head.appendChild(script);
      script.onerror = promise.reject;
      script.onload = () => {
        if(this.harness.activeKeyboard) {
          promise.resolve(this.harness.activeKeyboard);
        } else {
          promise.reject();
        }
      }

      promise.finally(() => {
        // https://stackoverflow.com/a/37393041 - totally safe.
        script.remove();
      });

      // Now that EVERYTHING ELSE is ready, establish the link to the keyboard's script.
      script.src = uri;
    } catch (err) {
      return Promise.reject(err);
    }

    return promise.corePromise;
  }
}