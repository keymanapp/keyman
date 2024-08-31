// Enables DOM types, but just for this one module.

///<reference lib="dom" />

import { default as Keyboard } from '../keyboard.js';
import { KeyboardHarness, MinimalKeymanGlobal } from '../keyboardHarness.js';
import { default as KeyboardLoaderBase } from '../keyboardLoaderBase.js';
import { KeyboardLoadErrorBuilder, KeyboardMissingError } from '../keyboardLoadError.js';

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

  protected async loadKeyboardBlob(uri: string): Promise<Blob> {
    if (this.performCacheBusting) {
      uri = this.cacheBust(uri);
    }

    const response = await fetch(uri);
    if (!response.ok) {
      throw new KeyboardMissingError(`Cannot find the keyboard at ${uri}.`, new Error(`HTTP ${response.status} ${response.statusText}`));
    }
    return response.blob();
  }

  protected async loadKeyboardFromScript(script: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<Keyboard> {
    this.evalScriptInContext(script, this.harness._jsGlobal);
    const keyboard = this.harness.loadedKeyboard;
    this.harness.loadedKeyboard = null;
    return keyboard;
  }

  private cacheBust(uri: string) {
    // Our WebView version directly sets the keyboard path, and it may replace the file
    // after KMW has loaded.  We need cache-busting to prevent the new version from
    // being ignored.
    return uri + "?v=" + (new Date()).getTime(); /*cache buster*/
  }

  private evalScriptInContext(script: string, context: any) {
    const f = function (s: string) {
      // use indirect eval (eval?.() notation doesn't work because of esbuild bundling)
      const evalFunc = eval;
      return evalFunc(s);
    }
    f.call(context, script);
  }

}