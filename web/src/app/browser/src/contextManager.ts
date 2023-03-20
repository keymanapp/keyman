import { type Keyboard, Mock, OutputTarget } from '@keymanapp/keyboard-processor';
import { type KeyboardStub } from 'keyman/engine/package-cache';
import {
  ContextManager as ContextManagerBase,
  type KeyboardInterface
} from 'keyman/engine/main';
import { BrowserConfiguration } from './configuration.js';

export default class ContextManager extends ContextManagerBase {
  private _activeKeyboard: {keyboard: Keyboard, metadata: KeyboardStub};
  private config: BrowserConfiguration;

  initialize(): void {
    // TBD:  keyman.domManager.init (the page-integration parts)
    // CTRL+F: `// Exit initialization here if we're using an embedded code path.`
    // EVERYTHING after that block will likely go here - DOMManager's role always
    // was context-management and the facilitation thereof.
    throw new Error('Method not implemented.');
  }

  get activeTarget(): OutputTarget {
    // TBD:  basically DOMManager's .activeElement.
    throw new Error('Method not implemented.');
  }

  get activeKeyboard() {
    return this._activeKeyboard;
  }

  set activeKeyboard(kbd: {keyboard: Keyboard, metadata: KeyboardStub}) {
    throw new Error('Method not implemented.');
    // depends on the target
    // if not set with an "independent keyboard", changes the global.
    // if set with an "independent keyboard", changes only the active target's keyboard.
  }

  insertText(kbdInterface: KeyboardInterface, Ptext: string, PdeadKey: number) {
    // Find the correct output target to manipulate.
    const outputTarget = this.activeTarget;

    if(outputTarget != null) {
      // Intent:  this class will be responsible for maintaining the active context... so
      // `this` itself will be responsible for _IgnoreNextSelChange and focusLastActiveElement.
      // Still trying to work out the uiManager bit, since the OSK does need to interact with that.
      // Keep the rest of the comment below post-modularization, though:
      //
      // While not yet fully connected, ContextManager and its subclasses will be responsible for maintaining
      // active elements & their focus... something that should only really matter for the website-integrating
      // subclass.

      // Required for the `sil_euro_latin` keyboard's desktop OSK/table to function properly.
      keyman.uiManager.setActivatingUI(true);
      dom.DOMEventHandlers.states._IgnoreNextSelChange = 100;
      keyman.domManager.focusLastActiveElement();
      dom.DOMEventHandlers.states._IgnoreNextSelChange = 0;

      return super.insertText(kbdInterface, Ptext, PdeadKey);
    }
    return false;
  }
}