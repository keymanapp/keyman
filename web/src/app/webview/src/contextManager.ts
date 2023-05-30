import { type Keyboard, Mock, OutputTarget } from '@keymanapp/keyboard-processor';
import { KeyboardStub } from 'keyman/engine/package-cache';
import { ContextManagerBase, ContextManagerConfiguration } from 'keyman/engine/main';
import { WebviewConfiguration } from './configuration.js';

export type OnInsertTextFunc = (deleteLeft: number, text: string, deleteRight: number) => void;

class ContextHost extends Mock {
  readonly oninserttext?: OnInsertTextFunc;

  constructor(oninserttext: OnInsertTextFunc) {
    super();
    this.oninserttext = oninserttext;
  }

  apply(transform: Transform): void {
    super.apply(transform);

    // Signal the necessary text changes to the embedding app, if it exists.
    if(this.oninserttext) {
      this.oninserttext(transform.deleteLeft, transform.insert, transform.deleteRight);
    }
  }

  // In app/webview, apps are expected to immediately update the selection range AFTER
  // changing the context's text.
  setText(text: string): void {
    this.text = text;
    this.setSelection(this.text._kmwLength());
  }
}

export default class ContextManager extends ContextManagerBase<WebviewConfiguration> {
  // Change of context?  Just replace the Mock.  Context will be ENTIRELY controlled
  // by whatever is hosting the WebView.  (Some aspects of this context replacement have
  // yet to be modularized at this time, though.)
  private _rawContext: ContextHost;

  private _activeKeyboard: {keyboard: Keyboard, metadata: KeyboardStub};

  constructor(engineConfig: WebviewConfiguration) {
    super(engineConfig);
  }

  initialize(): void {
    this._rawContext = new ContextHost(this.engineConfig.oninserttext);
    this.predictionContext.setCurrentTarget(this.activeTarget);
    this.resetContext();
  }

  get activeTarget(): Mock {
    return this._rawContext;
  }

  get activeKeyboard() {
    return this._activeKeyboard;
  }

  activateKeyboardForTarget(kbd: {keyboard: Keyboard, metadata: KeyboardStub}, target: OutputTarget) {
    // `target` is irrelevant for `app/webview`, as it'll only ever use 'global' keyboard settings.

    // Clone the object to prevent accidental by-reference changes.
    this._activeKeyboard = {...kbd};
  }

  /**
   * Reflects the active 'target' upon which any `set activeKeyboard` operation will take place.
   * For app/webview... there's only one target, thus only a "global default" matters.
   */
  protected currentKeyboardSrcTarget(): Mock {
    return null;
  }

  protected getFallbackStubKey(): KeyboardStub {
    // Fallback behavior - we're embedded in a touch-device's webview, so we need to keep a keyboard visible.
    return this.keyboardCache.defaultStub;
  }

  public async activateKeyboard(keyboardId: string, languageCode?: string, saveCookie?: boolean): Promise<boolean> {
    // If the default keyboard is requested, load that.  May vary based on form-factor, which is
    // part of what .getFallbackStubKey() handles.
    if(!keyboardId) {
      keyboardId = this.getFallbackStubKey().id;
      languageCode = this.getFallbackStubKey().langId;
    }

    try {
      return await super.activateKeyboard(keyboardId, languageCode, saveCookie);
    } catch(err) {
      // Fallback behavior - we're embedded in a touch-device's webview, so we need to keep a keyboard visible.
      const fallbackCodes = this.getFallbackStubKey();
      if(fallbackCodes.id != keyboardId) {
        await this.activateKeyboard(fallbackCodes.id, fallbackCodes.langId, true).catch(() => {});
      } // else "We already failed, so give up."

      throw err; // since the consuming method / API-caller may want to do its own error-handling.
    }
  }

  protected prepareKeyboardForActivation(
    keyboardId: string,
    languageCode?: string
  ): {keyboard: Promise<Keyboard>, metadata: KeyboardStub} {
    const originalKeyboard = this.activeKeyboard;
    const activatingKeyboard = super.prepareKeyboardForActivation(keyboardId, languageCode);

    // Erasing precomputed layout information attached to the keyboard object probably isn't
    // necessary at this point - osk.refreshLayout() exists and automatically handles any
    // needed recalculations itself.
    //
    // That said, it's best to keep it around for now and verify later.
    if(originalKeyboard?.metadata?.id == activatingKeyboard?.metadata?.id) {
      activatingKeyboard.keyboard = activatingKeyboard.keyboard.then((kbd) => {
        kbd.refreshLayouts()
        return kbd;
      });
    }

    return activatingKeyboard;
  }
}