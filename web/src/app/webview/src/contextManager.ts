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
}

export default class ContextManager extends ContextManagerBase {
  // Change of context?  Just replace the Mock.  Context will be ENTIRELY controlled
  // by whatever is hosting the WebView.  (Some aspects of this context replacement have
  // yet to be modularized at this time, though.)
  private _rawContext: ContextHost;
  private config: WebviewConfiguration;

  private _activeKeyboard: {keyboard: Keyboard, metadata: KeyboardStub};

  constructor(engineConfig: WebviewConfiguration) {
    super();

    this.config = engineConfig;
  }

  initialize(): void {
    this._rawContext = new ContextHost(this.config.oninserttext);
    this.resetContext();
  }

  get activeTarget(): Mock {
    return this._rawContext;
  }

  get activeKeyboard() {
    return this._activeKeyboard;
  }

  setKeyboardActiveForTarget(kbd: {keyboard: Keyboard, metadata: KeyboardStub}, target: OutputTarget) {
    // `target` is irrelevant for `app/webview`, as it'll only ever use 'global' keyboard settings.

    // Clone the object to prevent accidental by-reference changes.
    this._activeKeyboard = {...kbd};
  }

  /**
   * Reflects the active 'target' upon which any `set activeKeyboard` operation will take place.
   * For app/webview... there's only one target, thus only a "global default" matters.
   */
  protected get keyboardTarget(): Mock {
    return null;
  }


  public async activateKeyboard(keyboardId: string, languageCode?: string, saveCookie?: boolean): Promise<boolean> {
    try {
      return await super.activateKeyboard(keyboardId, languageCode, saveCookie);
    } catch(err) {
      // Fallback behavior - we're embedded in a touch-device's webview, so we need to keep a keyboard visible.
      const defaultStub = this.keyboardCache.defaultStub;
      await this.activateKeyboard(defaultStub.id, defaultStub.langId, true).catch(() => {});

      throw err; // since the consumer may want to do its own error-handling.
    }
  }

  protected prepareKeyboardForActivation(
    keyboardId: string,
    languageCode?: string
  ): {keyboard: Promise<Keyboard>, metadata: KeyboardStub} {
    const originalKeyboard = this.activeKeyboard;
    const activatingKeyboard = super.prepareKeyboardForActivation(keyboardId, languageCode);

    // Probably isn't necessary at this point - osk.refreshLayout() exists - but
    // it's best to keep it around for now and verify later.
    if(originalKeyboard.metadata.id == activatingKeyboard.metadata.id) {
      activatingKeyboard.keyboard = activatingKeyboard.keyboard.then((kbd) => {
        kbd.refreshLayouts()
        return kbd;
      });
    }

    return activatingKeyboard;
  }
}