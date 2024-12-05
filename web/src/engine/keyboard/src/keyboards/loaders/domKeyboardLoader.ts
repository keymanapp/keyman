// Enables DOM types, but just for this one module.

///<reference lib="dom" />

import { JSKeyboard } from '../keyboard.js';
import { KeyboardHarness, MinimalKeymanGlobal } from '../keyboardHarness.js';
import { KeyboardLoaderBase } from '../keyboardLoaderBase.js';
import { KeyboardLoadErrorBuilder } from '../keyboardLoadError.js';

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

  protected async loadKeyboardBlob(uri: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<Uint8Array> {
    if (this.performCacheBusting) {
      uri = this.cacheBust(uri);
    }

    let response: Response;
    try {
      response = await fetch(uri);
    } catch (e) {
      throw errorBuilder.keyboardDownloadError(e);
    }

    if (!response.ok) {
      throw errorBuilder.keyboardDownloadError(new Error(`HTTP ${response.status} ${response.statusText}`));
    }

    let buffer: ArrayBuffer;
    try {
      buffer = await response.arrayBuffer();
    } catch (e) {
      throw errorBuilder.invalidKeyboard(e);
    }
    return new Uint8Array(buffer);
  }

  protected async loadKeyboardFromScript(script: string, errorBuilder: KeyboardLoadErrorBuilder): Promise<JSKeyboard> {
    try {
      this.evalScriptInContext(script, this.harness._jsGlobal);
    } catch (e) {
      throw errorBuilder.scriptError(e);
    }
    const keyboard = this.harness.loadedKeyboard;
    if (!keyboard) {
      throw errorBuilder.scriptError();
    }

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