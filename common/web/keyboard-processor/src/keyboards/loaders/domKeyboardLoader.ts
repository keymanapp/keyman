// Enables DOM types, but just for this one module.

///<reference lib="dom" />

import { Keyboard, KeyboardHarness, KeyboardLoaderBase, KeyboardLoadErrorBuilder, MinimalKeymanGlobal } from '@keymanapp/keyboard-processor';

import { ManagedPromise } from '@keymanapp/web-utils';

export class DOMKeyboardLoader extends KeyboardLoaderBase {
  public readonly element: HTMLIFrameElement;
  private readonly performCacheBusting: boolean;

  constructor()
  constructor(harness: KeyboardHarness);
  constructor(harness: KeyboardHarness, cacheBust?: boolean)
  constructor(harness?: KeyboardHarness, cacheBust?: boolean) {
    if(harness && harness._jsGlobal != window) {
      // Copy the String typing over; preserve string extensions!
      harness._jsGlobal['String'] = window['String'];
    }

    if(!harness) {
      super(new KeyboardHarness(window, MinimalKeymanGlobal));
    } else {
      super(harness);
    }

    this.performCacheBusting = cacheBust || false;
  }

  protected loadKeyboardInternal(
    uri: string,
    errorBuilder: KeyboardLoadErrorBuilder,
    id?: string
  ): Promise<Keyboard> {
    const promise = new ManagedPromise<Keyboard>();

    if(this.performCacheBusting) {
      uri = this.cacheBust(uri);
    }

    try {
      const document = this.harness._jsGlobal.document;
      const script = document.createElement('script');
      if(id) {
        script.id = id;
      }
      document.head.appendChild(script);
      script.onerror = (err: any) => {
        promise.reject(errorBuilder.missingError(err));
      }
      script.onload = () => {
        if(this.harness.loadedKeyboard) {
          const keyboard = this.harness.loadedKeyboard;
          this.harness.loadedKeyboard = null;
          promise.resolve(keyboard);
        } else {
          promise.reject(errorBuilder.scriptError());
        }
      }

      // On the oldest mobile devices we support, Promise.finally may not actually exist.
      // Fortunately... it's not that hard of an issue to work around.
      // Note:  es6-shim doesn't polyfill Promise.finally!
      promise.then(() => {
        // It is safe to remove the script once it has been run (https://stackoverflow.com/a/37393041)
        script.remove();
      }).catch(() => {
        script.remove();
      });

      // Now that EVERYTHING ELSE is ready, establish the link to the keyboard's script.
      script.src = uri;
    } catch (err) {
      return Promise.reject(err);
    }

    return promise.corePromise;
  }

  private cacheBust(uri: string) {
    // Our WebView version directly sets the keyboard path, and it may replace the file
    // after KMW has loaded.  We need cache-busting to prevent the new version from
    // being ignored.
    return uri + "?v=" + (new Date()).getTime(); /*cache buster*/
  }
}