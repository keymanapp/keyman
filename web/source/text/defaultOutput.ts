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

    public static forNumpadKeys(Lkc: KeyEvent) {
      let activeKeyboard = com.keyman.singleton.keyboardManager.activeKeyboard;

      // Translate numpad keystrokes into their non-numpad equivalents
      if(Lkc.Lcode >= Codes.keyCodes["K_NP0"]  &&  Lkc.Lcode <= Codes.keyCodes["K_NPSLASH"] && activeKeyboard && !activeKeyboard['KM']) {
        // Number pad, numlock on
        if(Lkc.Lcode < 106) {
          var Lch = Lkc.Lcode-48;
        } else {
          Lch = Lkc.Lcode-64;
        }
        let ch = String._kmwFromCharCode(Lch); //I3319
        return ch;
      } else {
        return '';
      }
    }

    // Test for fall back to U_xxxxxx key id
    // For this first test, we ignore the keyCode and use the keyName
    public static forUnicodeKeynames(Lkc: KeyEvent) {
      let quiet = Lkc.Ltarg instanceof Mock;
      let keyName = Lkc.kName;

      // Test for fall back to U_xxxxxx key id
      // For this first test, we ignore the keyCode and use the keyName
      if(!keyName || keyName.substr(0,2) != 'U_') {
        return '';
      }
    
      var codePoint = parseInt(keyName.substr(2,6), 16);
      if (((0x0 <= codePoint) && (codePoint <= 0x1F)) || ((0x80 <= codePoint) && (codePoint <= 0x9F))) {
        // Code points [U_0000 - U_001F] and [U_0080 - U_009F] refer to Unicode C0 and C1 control codes.
        // Check the codePoint number and do not allow output of these codes via U_xxxxxx shortcuts.
        if(!quiet) {
          console.log("Suppressing Unicode control code: U_00" + codePoint.toString(16));
        }
        return '';
      } else {
        // String.fromCharCode() is inadequate to handle the entire range of Unicode
        // Someday after upgrading to ES2015, can use String.fromCodePoint()
        return String.kmwFromCharCode(codePoint);
      }
    }

    // Test for otherwise unimplemented keys on the the base default & shift layers.
    // Those keys must be blocked by keyboard rules if intentionally unimplemented; otherwise, this function will trigger.
    public static forBaseKeys(Lkc: KeyEvent, keyShiftState: number) {
      let n = Lkc.Lcode;
      // check if exact match to SHIFT's code.  Only the 'default' and 'shift' layers should have default key outputs.
      if(keyShiftState != 0 && keyShiftState != 1) {
        if(!(Lkc.Ltarg instanceof Mock)) {
          console.warn("KMW only defines default key output for the 'default' and 'shift' layers!");
        }
      }

      if(keyShiftState == 0 || keyShiftState == 1) { // keyShiftState can only be 0 or 1.  (1 being mapped from Codes.modifier['SHIFT'] by the caller.)
        try {
          if(n >= Codes.keyCodes['K_0'] && n <= Codes.keyCodes['K_9']) { // The number keys.
            return Codes.codesUS[keyShiftState][0][n-Codes.keyCodes['K_0']];
          } else if(n >= Codes.keyCodes['K_A'] && n <= Codes.keyCodes['K_Z']) { // The base letter keys
            return String.fromCharCode(n+(keyShiftState?0:32));  // 32 is the offset from uppercase to lowercase.
          } else if(n >= Codes.keyCodes['K_COLON'] && n <= Codes.keyCodes['K_BKQUOTE']) {
            return Codes.codesUS[keyShiftState][1][n-Codes.keyCodes['K_COLON']];
          } else if(n >= Codes.keyCodes['K_LBRKT'] && n <= Codes.keyCodes['K_QUOTE']) {
            return Codes.codesUS[keyShiftState][2][n-Codes.keyCodes['K_LBRKT']];
          }
        } catch (e) {
          if(!(Lkc.Ltarg instanceof Mock)) {
            console.error("Error detected with default mapping for key:  code = " + n + ", shift state = " + (keyShiftState == 1 ? 'shift' : 'default'));
          }
        }
      }

      return '';
    }
  }
}