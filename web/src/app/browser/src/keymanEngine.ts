import { KeymanEngine as KeymanEngineBase } from 'keyman/engine/main';
import { ProcessorInitOptions } from "@keymanapp/keyboard-processor";
import { type KeyboardStub } from "keyman/engine/package-cache";

import { BrowserConfiguration } from './configuration.js';
import ContextManager from './contextManager.js';
import DefaultBrowserRules from './defaultBrowserRules.js';
import KeyEventKeyboard from './keyEventKeyboard.js';

export class KeymanEngine extends KeymanEngineBase<ContextManager, KeyEventKeyboard> {
  constructor(worker: Worker, config: BrowserConfiguration) {
    super(worker, config, new ContextManager());
  }

  protected processorConfiguration(): ProcessorInitOptions {
    return {
      keyboardInterface: this.interface,
      defaultOutputRules: new DefaultBrowserRules(this.contextManager)
    };
  };

  protected async activateKeyboard(keyboardId: string, languageCode?: string, saveCookie?: boolean): Promise<void> {
    saveCookie ||= false;

    try {
      await super.activateKeyboard(keyboardId, languageCode, saveCookie);

      if(saveCookie /* && this.contextManager.activeTarget does not have independent-keyboard mode active */) {
        // TODO: persist the newly-activating keyboard's IDs for use as default upon page reload
      }

      // TODO: app/browser - _SetTargDir (within its ContextManager)
    } catch(err) {
      // non-embedded:  if keyboard activation failed, deactivate the keyboard.

      // Make sure we don't infinite-recursion should the deactivate somehow fail.
      if(this.config.hostDevice.touchable) {
        // Fallback behavior - if on a touch device, we need to keep a keyboard visible.
        const defaultStub = this.keyboardRequisitioner.cache.defaultStub;
        await this.activateKeyboard(defaultStub.id, defaultStub.langId, true).catch(() => {});
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

  protected onKeyboardAsyncLoadStart(requestedStub: KeyboardStub) {
    // TODO:  app/browser - display the loader UI if configured?
  }
}
