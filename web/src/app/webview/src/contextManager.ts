import { JSKeyboard, Keyboard, TextStore, Transcription, TextTransform, SyntheticTextStore, findCommonSubstringEndIndex } from 'keyman/engine/keyboard';
import { KeyboardStub } from 'keyman/engine/keyboard-storage';
import { ContextManagerBase } from 'keyman/engine/main';
import { WebviewConfiguration } from './configuration.js';
import { LexicalModelTypes } from '@keymanapp/common-types';
import { KMWString, isEmptyTransform } from 'keyman/common/web-utils';

export type OnInsertTextFunc = (deleteLeft: number, text: string, deleteRight: number) => void;

/**
 * WebView-specific synthetic TextStore implementation that can
 * communicate and synchronize with the host app despite not being
 * backed by any sort of Web element.
 */
export class HostTextStore extends SyntheticTextStore {
  readonly oninserttext?: OnInsertTextFunc;
  private savedState: SyntheticTextStore;

  constructor(oninserttext: OnInsertTextFunc) {
    super();
    this.oninserttext = oninserttext;
    this.saveState();
  }

  apply(transform: LexicalModelTypes.Transform): void {
    super.apply(transform);
    this.updateHost();
  }

  updateHost(transcription?: Transcription): void {
    const savedState = this.savedState;

    if(this.savedState) {
      let transform: TextTransform = null;

      if(transcription) {
        const { preInput } = transcription;
        // If our saved state matches the `preInput` from the incoming transcription, just reuse its transform.
        // Will generally not match during multitap operations, though.
        //
        // Helps ensure backspaces pass through even if we don't currently have text available in context for them.
        if(preInput.text == savedState.text && preInput.selStart == savedState.selStart && preInput.selEnd == savedState.selEnd) {
          transform = transcription.transform;
        }
      }

      transform ||= this.buildTransformFrom(this.savedState);

      // Signal the necessary text changes to the embedding app, if it exists.
      if(this.oninserttext) {
        if(!isEmptyTransform(transform) || transform.erasedSelection) {
          this.oninserttext(transform.deleteLeft, transform.insert, transform.deleteRight);
        }
      }
    }

    // Save the current context state for use in future diffs.
    this.saveState();
  }

  saveState() {
    this.savedState = SyntheticTextStore.from(this);
  }

  restoreTo(original: TextStore): void {
    this.savedState = SyntheticTextStore.from(this);
    super.restoreTo(original);
  }

  updateContext(text: string, selStart: number, selEnd: number): boolean {
    let shouldResetContext = false;
    const tempTextStore = new SyntheticTextStore(text, selStart ?? KMWString.length(text), selEnd ?? KMWString.length(text));
    const newLeft = tempTextStore.getTextBeforeCaret();
    const oldLeft = this.getTextBeforeCaret();

    if(text != this.text) {
      const unexpectedBeforeCharCount = findCommonSubstringEndIndex(newLeft, oldLeft, true) + 1;
      shouldResetContext = !!unexpectedBeforeCharCount;
    }

    if(shouldResetContext) {
      this.text = text;
      this.selStart = selStart;
      this.selEnd = selEnd;
    } else {
      // Transform selection coordinates to their location within the longform context window.
      const delta = KMWString.length(oldLeft) - KMWString.length(newLeft);
      this.selStart = selStart - delta;
      this.selEnd = selEnd - delta;
    }

    if(selStart === undefined || selEnd === undefined) {
      // Regardless of keyboard, we should check the SMP-aware length of the string.
      // Our host app will not know whether or not the keyboard uses SMP chars,
      // and we want a consistent interface for context synchronization between
      // host app + app/webview KMW.
      this.setSelection(KMWString.length(this.text));
    }

    this.saveState();

    return shouldResetContext;
  }

  // In app/webview, apps are expected to immediately update the selection range AFTER
  // changing the context's text.
  setText(text: string): void {
    this.text = text;
    // Regardless of keyboard, we should check the SMP-aware length of the string.
    // Our host app will not know whether or not the keyboard uses SMP chars,
    // and we want a consistent interface for context synchronization between
    // host app + app/webview KMW.
    this.setSelection(KMWString.length(this.text));
    this.savedState = SyntheticTextStore.from(this);
  }
}

export class ContextManager extends ContextManagerBase<WebviewConfiguration> {
  // Change of context?  Just replace the SyntheticTextStore.  Context will be ENTIRELY controlled
  // by whatever is hosting the WebView.  (Some aspects of this context replacement have
  // yet to be modularized at this time, though.)
  private _hostTextStore: HostTextStore;

  private _activeKeyboard: {keyboard: JSKeyboard, metadata: KeyboardStub};

  constructor(engineConfig: WebviewConfiguration) {
    super(engineConfig);
  }

  initialize(): void {
    this._hostTextStore = new HostTextStore(this.engineConfig.oninserttext);
    this.predictionContext.setCurrentTextStore(this.activeTextStore);
    this.resetContext();
  }

  get activeTextStore(): SyntheticTextStore {
    return this._hostTextStore;
  }

  get activeKeyboard() {
    return this._activeKeyboard;
  }

  activateKeyboardForTextStore(kbd: { keyboard: JSKeyboard, metadata: KeyboardStub }, textStore: TextStore) {
    // `textStore` is irrelevant for `app/webview`, as it'll only ever use 'global' keyboard settings.

    // Clone the object to prevent accidental by-reference changes.
    this._activeKeyboard = {...kbd};
  }

  /**
   * Reflects the active 'textStore' upon which any `set activeKeyboard` operation will take place.
   * For app/webview... there's only one textStore, thus only a "global default" matters.
   */
  protected currentKeyboardSrcTextStore(): SyntheticTextStore {
    return null;
  }

  protected getFallbackStubKey(): KeyboardStub {
    // Fallback behavior - we're embedded in a touch-device's webview, so we need to keep a keyboard visible.
    return this.keyboardCache.defaultStub;
  }

  public async activateKeyboard(keyboardId: string, languageCode?: string, saveCookie?: boolean): Promise<boolean> {
    /*
      this.keyboardCache isn't set until partway through KMW initialization.
      Also, keyboard stubs aren't processed until initialization is complete,
      so it's best to wait for initialization anyway.
    */
    if(!this.engineConfig.deferForInitialization.isFulfilled) {
      await this.engineConfig.deferForInitialization.corePromise;

      // Our mobile app-engines all register the stub before attempting to set the keyboard,
      // so the Promise ordering here should work in our favor.
    }

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
        // TODO-embed-osk-in-kmx: Do we need to refresh layouts for KMX keyboards also?
        if (kbd instanceof JSKeyboard) {
          kbd.refreshLayouts();
        }
        return kbd;
      });
    }

    return activatingKeyboard;
  }

  public resetContext(): void {
    super.resetContext();
    this._hostTextStore.saveState();
  }
}