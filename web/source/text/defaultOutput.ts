// Establishes key-code definitions.
/// <reference path="codes.ts" />
// Defines our generalized "KeyEvent" class.
/// <reference path="keyEvent.ts" />

namespace com.keyman.text {
  /**
   * Defines a collection of static library functions that define KeymanWeb's default (implied) keyboard rule behaviors.
   */
  export class DefaultOutput {
    private constructor() {
    }

    /**
     * This function is designed for future migration into a separate, explicitly-DOM-aware module.
     * It would then be assigned to this class via classic JS method extension practices as an 'override'
     * of `commandEmulation`, reusing the base version of that name seen in this class, which is
     * designed for web-core.
     */
    public static domCommandEmulation(outputTarget: OutputTarget, code: number, keyShiftState: number) {
      let quiet = outputTarget instanceof Mock;

      let domManager = com.keyman.singleton.domManager;

      switch(code) {
        case Codes.keyCodes['K_TAB']:
          if(!quiet) {
            domManager.moveToNext(keyShiftState);
          }
          break;
        case Codes.keyCodes['K_TABBACK']:
          if(!quiet) {
            domManager.moveToNext(true);
          }
          break;
        case Codes.keyCodes['K_TABFWD']:
          if(!quiet) {
            domManager.moveToNext(false);
          }
          break;
      }
    }

    public static commandEmulation(Lkc: KeyEvent, keyShiftState: number) {
      let keyName = Lkc.kName;
      let n = Lkc.Lcode;
      let outputTarget = Lkc.Ltarg;

      let keyman = com.keyman.singleton;

      let quiet = outputTarget instanceof Mock;

      var code = Codes.keyCodes[keyName];
      if(!code) {
        code = n;
      }

      DefaultOutput.domCommandEmulation(outputTarget, code, keyShiftState);

      switch(code) {
        case Codes.keyCodes['K_BKSP']:  //Only desktop UI, not touch devices. TODO: add repeat while mouse down for desktop UI
          if(quiet) {
            // TODO:  Remove need for this clause via refactoring.  It's currently needed for predictive text Mocks.
            return '\b'; // the escape sequence for backspace.
          } else {
            keyman.interface.defaultBackspace(outputTarget);
          }
          return '';
        case Codes.keyCodes['K_ENTER']:
          outputTarget.handleNewlineAtCaret();

          return '\n';  // We still return this, as it ensures we generate a rule-match.
        case Codes.keyCodes['K_SPACE']:
          return ' ';
        //
        // // Problem:  clusters, and doing them right.
        // // The commented-out code below should be a decent starting point, but clusters make it complex.
        // // Mostly based on pre-12.0 code, but the general idea should be relatively clear.
        //
        // case Codes.keyCodes['K_LEFT']:
        //   if(touchAlias) {
        //     var caretPos = keymanweb.getTextCaret(Lelem);
        //     keymanweb.setTextCaret(Lelem, caretPos - 1 >= 0 ? caretPos - 1 : 0);
        //   }
        //   break;
        // case Codes.keyCodes['K_RIGHT']:
        //   if(touchAlias) {
        //     var caretPos = keymanweb.getTextCaret(Lelem);
        //     keymanweb.setTextCaret(Lelem, caretPos + 1);
        //   }
        //   if(code == VisualKeyboard.keyCodes['K_RIGHT']) {
        //     break;
        //   }
        // // Should we include this?  It could be tricky to do correctly...
        // case Codes.keyCodes['K_DEL']:
        //   // Move caret right one unit, then backspace.
        //   if(touchAlias) {
        //     var caretPos = keymanweb.getTextCaret(Lelem);
        //     keymanweb.setTextCaret(Lelem, caretPos + 1);
        //     if(caretPos == keymanweb.getTextCaret(Lelem)) {
        //       // Failed to move right - there's nothing to delete.
        //       break;
        //     }
        //     kbdInterface.defaultBackspace();
        //   }
      }
    }
  }
}