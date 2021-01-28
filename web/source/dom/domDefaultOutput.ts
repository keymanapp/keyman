namespace com.keyman.dom {
  // Basically, these are shorthand imports, only viewable within this file.
  let DefaultOutput = text.DefaultOutput;
  let Codes = text.Codes;
  type KeyEvent = text.KeyEvent;
  type RuleBehavior = text.RuleBehavior;

  // Now for some classic JS method "extension".
  let coreIsCommand = DefaultOutput.isCommand;
  let coreApplyCommand = DefaultOutput.applyCommand;
  let coreForBaseKeys = DefaultOutput.forBaseKeys;

  DefaultOutput.isCommand = function(Lkc: KeyEvent): boolean {
    let code = DefaultOutput.codeForEvent(Lkc);
    let keyman = com.keyman.singleton;

    switch(code) {
      case Codes.keyCodes['K_TAB']:
      case Codes.keyCodes['K_TABBACK']:
      case Codes.keyCodes['K_TABFWD']:
        return !keyman.isEmbedded;
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

  DefaultOutput.forBaseKeys = function(Lkc: KeyEvent, ruleBehavior?: RuleBehavior): string {
    let n = Lkc.Lcode;
    let keyman = com.keyman.singleton;

    if(n == Codes.keyCodes['K_TAB'] || n == Codes.keyCodes['K_TABBACK'] || n == Codes.keyCodes['K_TABFWD']) {
          // Filter out unembedded desktop scenarios; we need browser-default behavior to go through then.
      if (Lkc.device.formFactor != utils.FormFactor.Desktop || keyman.isEmbedded) {
        // If TAB may be treated as a 'command key', it'll have been filtered out before this point.
        return '\t';
      } else {
        // For the filtered scenario, we need to explicitly NOT handle it.
        return null;
      }
    } else {
      return coreForBaseKeys(Lkc, ruleBehavior);
    }
  }
}