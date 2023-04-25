import {
  Codes,
  DefaultRules,
  type KeyEvent,
  type OutputTarget
} from '@keymanapp/keyboard-processor';

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

    const contextManager = this.contextManager;

    let elem: HTMLElement;
    switch(code) {
      // This method will be handled between `ContextManager` and PageContextAttachment:
      // pageContextAttachment.findNeighboringInput(contextManager.activeTarget.getElement(), <same flag>)
      case Codes.keyCodes['K_TAB']:
        const bBack = (Lkc.Lmodifiers & Codes.modifierCodes['SHIFT']) != 0;
        elem = contextManager.page.findNeighboringInput(contextManager.activeTarget.getElement(), bBack);
        elem.focus();
        break;
      case Codes.keyCodes['K_TABBACK']:
        elem = contextManager.page.findNeighboringInput(contextManager.activeTarget.getElement(), true);
        elem.focus();
        break;
      case Codes.keyCodes['K_TABFWD']:
        elem = contextManager.page.findNeighboringInput(contextManager.activeTarget.getElement(), false);
        elem.focus();
        break;
    }

    super.applyCommand(Lkc, outputTarget);
  }
}