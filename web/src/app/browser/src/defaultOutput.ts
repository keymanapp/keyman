import {
  Codes,
  DefaultOutput as DefaultOutputBase,
  type KeyEvent,
  type OutputTarget
} from '@keymanapp/keyboard-processor';

import ContextManager from './contextManager.js';

export default class DefaultOutput extends DefaultOutputBase {
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

    switch(code) {
      // This method will be transplanted to the specific `ContextManager` module stored above.
      case Codes.keyCodes['K_TAB']:
        domManager.moveToNext((Lkc.Lmodifiers & Codes.modifierCodes['SHIFT']) != 0);
        break;
      case Codes.keyCodes['K_TABBACK']:
        domManager.moveToNext(true);
        break;
      case Codes.keyCodes['K_TABFWD']:
        domManager.moveToNext(false);
        break;
    }

    super.applyCommand(Lkc, outputTarget);
  }
}