import { type Keyboard, Mock } from '@keymanapp/keyboard-processor';
import { type KeyboardStub } from 'keyman/engine/keyboard-cache';
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

  // ... selected text is actually looking kinda tricky here.
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
    this._activeKeyboard = kbd;

    if(priorEntry.keyboard != kbd.keyboard || priorEntry.metadata != kbd.metadata) {
      this.emit('keyboardchange', kbd);

      // Differs from the standard 'resetContext' by not using new-context rules; this IS
      // a keyboard change, not a true context change.
      this.resetKeyState();
      this.predictionContext.resetContext();
    }
  }
}