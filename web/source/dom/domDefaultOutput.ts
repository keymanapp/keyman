namespace com.keyman.dom {
  // Basically, these are shorthand imports, only viewable within this file.
  let DefaultOutput = text.DefaultOutput;
  let Codes = text.Codes;
  type KeyEvent = text.KeyEvent;

  // Now for some classic JS method "extension".
  let coreIsCommand = DefaultOutput.isCommand;
  let coreApplyCommand = DefaultOutput.applyCommand;

  DefaultOutput.isCommand = function(Lkc: KeyEvent): boolean {
    let code = DefaultOutput.codeForEvent(Lkc);

    switch(code) {
      case Codes.keyCodes['K_TAB']:
      case Codes.keyCodes['K_TABBACK']:
      case Codes.keyCodes['K_TABFWD']:
        return true;
      default:
        return coreIsCommand(Lkc);
    }
  }

  /**
   * applyCommand - used when a RuleBehavior represents a non-text "command" within the Engine.
   */
  DefaultOutput.applyCommand = function(Lkc: KeyEvent): void {
    let code = DefaultOutput.codeForEvent(Lkc);
    let domManager = com.keyman.singleton.domManager;

    switch(code) {
      case Codes.keyCodes['K_TAB']:
        domManager.moveToNext((Lkc.Lmodifiers & text.Codes.modifierCodes['SHIFT']) != 0);
        break;
      case Codes.keyCodes['K_TABBACK']:
        domManager.moveToNext(true);
        break;
      case Codes.keyCodes['K_TABFWD']:
        domManager.moveToNext(false);
        break;
    }

    coreApplyCommand(Lkc);
  }
}