import { type Keyboard, Mock, OutputTarget } from '@keymanapp/keyboard-processor';
import { type KeyboardStub } from 'keyman/engine/package-cache';
import { CookieSerializer } from 'keyman/engine/dom-utils';
import {
  ContextManagerBase,
  type KeyboardInterface
} from 'keyman/engine/main';
import { BrowserConfiguration } from './configuration.js';
import { FocusAssistant } from './context/focusAssistant.js';

interface KeyboardCookie {
  current: string;
}

export default class ContextManager extends ContextManagerBase<BrowserConfiguration> {
  private _activeKeyboard: {keyboard: Keyboard, metadata: KeyboardStub};
  private config: BrowserConfiguration;
  private cookieManager = new CookieSerializer<KeyboardCookie>('KeymanWeb_Keyboard');
  readonly focusAssistant = new FocusAssistant();

  initialize(): void {
    this.on('keyboardasyncload', (stub, completion) => {
      // TODO:  app/browser - display the loader UI if configured?
      // util.wait('Installing keyboard<br/>' + kbdName);

      completion.then(() => {
        // Cancel the loader UI.
      });
    });

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

  // on set activeTarget, make sure to also change it for this.predictionContext!

  get activeKeyboard() {
    return this._activeKeyboard;
  }

  setKeyboardActiveForTarget(kbd: {keyboard: Keyboard, metadata: KeyboardStub}, target: OutputTarget) {
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
      // Still trying to work out the uiManager / focusAssistant bit, since the OSK does need to interact with that.
      // Keep the rest of the comment below post-modularization, though:
      //
      // While not yet fully connected, ContextManager and its subclasses will be responsible for maintaining
      // active elements & their focus... something that should only really matter for the website-integrating
      // subclass.

      // Required for the `sil_euro_latin` keyboard's desktop OSK/table to function properly.
      focusAssistant.setActivatingUI(true);
      dom.DOMEventHandlers.states._IgnoreNextSelChange = 100;
      keyman.domManager.focusLastActiveElement();
      dom.DOMEventHandlers.states._IgnoreNextSelChange = 0;

      return super.insertText(kbdInterface, Ptext, PdeadKey);
    }
    return false;
  }

  /**
   * Reflects the active 'target' upon which any `set activeKeyboard` operation will take place.
   * When `null`, such operations will affect the global default; otherwise, such operations
   * affect only the specified `target`.
   */
  protected get keyboardTarget(): OutputTarget {
    // TODO: Remove `&& false` once the inlined section below is implemented.
    if(this.activeTarget /* has 'independent keyboard mode activated' */ && false) {
      return this.activeTarget;
    } else {
      return null;
    }
  }


  public async activateKeyboard(keyboardId: string, languageCode?: string, saveCookie?: boolean): Promise<boolean> {
    saveCookie ||= false;
    const originalKeyboardTarget = this.keyboardTarget;

    try {
      let result = await super.activateKeyboard(keyboardId, languageCode, saveCookie);

      if(saveCookie && !originalKeyboardTarget) { // if the active target uses global keyboard settings
        this.cookieManager.save({current: `${keyboardId}:${languageCode}`});
      }

      // Only do these if the active keyboard-target still matches the original keyboard-target;
      // otherwise, maintain what's correct for the currently active one.
      if(originalKeyboardTarget == this.keyboardTarget) {
        // TODO: app/browser - _SetTargDir (within its ContextManager)
        // util.addStyleSheet(domManager.setAttachmentFontStyle(kbdStub.KF));
        // focusAssistant.justActivated = true; // TODO:  Resolve without need for the cast.
      }

      return result;
    } catch(err) {
      // non-embedded:  if keyboard activation failed, deactivate the keyboard.

      // Make sure we don't infinite-recursion should the deactivate somehow fail.
      if(this.config.hostDevice.touchable) {
        // Fallback behavior - if on a touch device, we need to keep a keyboard visible.
        const defaultStub = this.keyboardCache.defaultStub;
        if(defaultStub.id != keyboardId || defaultStub.langId != languageCode) {
          await this.activateKeyboard(defaultStub.id, defaultStub.langId, true).catch(() => {});
        } // else "We already failed, so give up."
      } else {
        // Fallback behavior - if on a desktop device, the user still has a physical keyboard.
        // Just clear out the active keyboard & OSK.
        await this.activateKeyboard('', '', false).catch(() => {});
      }

      if((this.config as BrowserConfiguration).shouldAlert) {
        // TODO:  util.alert error report
      }

      throw err; // since the site-dev consumer may want to do their own error-handling.
    }
  }
}