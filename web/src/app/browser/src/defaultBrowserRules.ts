import { ModifierKeyConstants } from '@keymanapp/common-types';
import {
  Codes,
  DefaultRules,
  type KeyEvent
} from 'keyman/engine/keyboard';
import { type OutputTarget } from 'keyman/engine/js-processor';

import ContextManager from './contextManager.js';

export default class DefaultBrowserRules extends DefaultRules {
  private contextManager: ContextManager;

  constructor(contextManager: ContextManager) {
    super();
    this.contextManager = contextManager;
  }

  isCommand(Lkc: KeyEvent): boolean {
    let code = this.codeForEvent(Lkc);

    switch(code) {
      case Codes.keyCodes['K_TAB']:
      case Codes.keyCodes['K_TABBACK']:
      case Codes.keyCodes['K_TABFWD']:
        return true;
      default:
        return super.isCommand(Lkc);
    }
  }

  /**
   * applyCommand - used when a RuleBehavior represents a non-text "command" within the Engine.
   */
  applyCommand(Lkc: KeyEvent, outputTarget: OutputTarget): void {
    let code = this.codeForEvent(Lkc);

    const moveToNext = (back: boolean) => {
      const contextManager = this.contextManager;
      const activeElement = contextManager.activeTarget?.getElement();
      const nextElement = contextManager.page.findNeighboringInput(activeElement, back);
      nextElement?.focus();
    }

    switch(code) {
      // This method will be handled between `ContextManager` and PageContextAttachment:
      // pageContextAttachment.findNeighboringInput(contextManager.activeTarget.getElement(), <same flag>)
      case Codes.keyCodes['K_TAB']:
        moveToNext((Lkc.Lmodifiers & ModifierKeyConstants.K_SHIFTFLAG) != 0);
        break;
      case Codes.keyCodes['K_TABBACK']:
        moveToNext(true);
        break;
      case Codes.keyCodes['K_TABFWD']:
        moveToNext(false);
        break;
    }

    super.applyCommand(Lkc, outputTarget);
  }
}