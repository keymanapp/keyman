import { type Keyboard, Mock } from '@keymanapp/keyboard-processor';
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

  set activeKeyboard(kbd: {keyboard: Keyboard, metadata: KeyboardStub}) {
    const priorEntry = this._activeKeyboard;

    // Clone the stub before exposing it...
    if(!this.confirmKeyboardChange(new KeyboardStub(kbd.metadata))) {
      return;
    }

    // Clone the object to prevent accidental by-reference changes.
    this._activeKeyboard = {...kbd};

    if(priorEntry.keyboard != kbd.keyboard || priorEntry.metadata != kbd.metadata) {
      this.emit('keyboardchange', kbd);
      this.resetContext();
    }
  }

  async setActiveKeyboardAsync(kbd: Promise<Keyboard>, metadata: KeyboardStub): Promise<boolean> {
    if(!this.confirmKeyboardChange) {
      return false;
    }

    // There is only the one target, so 'default global keyboard' use is fine.
    if(!await this.deferredKeyboardActivationValid(kbd, metadata, null)) {
      return false;
    } else {
      const activatingKeyboard = {
        keyboard: await kbd,
        metadata: metadata
      };

      this.activeKeyboard = activatingKeyboard;

      // The change may silently fail due to `set activeKeyboard`'s `confirmKeyboardChange` call.
      return this.activeKeyboard.keyboard == activatingKeyboard.keyboard
          && this.activeKeyboard.metadata == activatingKeyboard.metadata;
    }
  }
}