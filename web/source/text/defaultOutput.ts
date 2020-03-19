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

    static codeForEvent(Lkc: KeyEvent) {
      let keyName = Lkc.kName;
      let n = Lkc.Lcode;

      var code = Codes.keyCodes[keyName];
      if(!code) {
        code = n;
      }

      return code;
    }

    /**
     * Serves as a default keycode lookup table.  This may be referenced safely by mnemonic handling without fear of side-effects.
     * Also used by Processor.defaultRuleOutput to generate output after filtering for special cases.
     */
    public static forAny(Lkc: KeyEvent, shiftState: number) {
      var char = '';

      let code = DefaultOutput.codeForEvent(Lkc);

      // A pretty simple table of lookups, corresponding VERY closely to the original defaultKeyOutput.
      if(char = DefaultOutput.forSpecialEmulation(Lkc)) {
        return char;
      } else if(char = DefaultOutput.forNumpadKeys(Lkc)) {
        return char;
      } else if(char = DefaultOutput.forUnicodeKeynames(Lkc)) {
        return char;
      } else if(char = DefaultOutput.forBaseKeys(Lkc, shiftState)) {
        return char;
      } else {
        // For headless and embeddded, we may well allow '\t'.  It's DOM mode that has other uses.
        // Not originally defined for text output within defaultKeyOutput.
        switch(code) {
          case Codes.keyCodes['K_TAB']:
          case Codes.keyCodes['K_TABBACK']:
          case Codes.keyCodes['K_TABFWD']:
            return '\t';
          default:
            return '';
        }
      }
    }

    public static isDOMCommand(Lkc: KeyEvent): boolean {
      let code = DefaultOutput.codeForEvent(Lkc);

      switch(code) {
        case Codes.keyCodes['K_TAB']:
        case Codes.keyCodes['K_TABBACK']:
        case Codes.keyCodes['K_TABFWD']:
          return true;
        default:
          return false;
      }
    }

    /**
     * forCommands - returns a boolean indicating if a non-text event should be triggered by the keystroke.
     */
    public static isCommand(Lkc: KeyEvent): boolean {
      let code = DefaultOutput.codeForEvent(Lkc);

      switch(code) {
        // Should we ever implement them:
        // case Codes.keyCodes['K_LEFT']:  // would not output text, but would alter the caret's position in the context.
        // case Codes.keyCodes['K_RIGHT']:
        //   return true;
        default:
          return this.isDOMCommand(Lkc);
      }
    }

    /**
     * This function is designed for future migration into a separate, explicitly-DOM-aware module.
     * It would then be assigned to this class via classic JS method extension practices as an 'override'
     * of `applyCommand`, reusing the base version of that name seen in this class, which is
     * designed for web-core.
     */
    /**
     * This function is designed for future migration into a separate, explicitly-DOM-aware module.
     * It would then be assigned to this class via classic JS method extension practices as an 'override'
     * of `applyCommand`, reusing the base version of that name seen in this class, which is
     * designed for web-core.
     * 
     * for___Commands - returns boolean indicating if a non-text event should be triggered by the keystroke.
     */
    public static applyDOMCommand(Lkc: KeyEvent, keyShiftState: number) {
      let code = DefaultOutput.codeForEvent(Lkc);
      let domManager = com.keyman.singleton.domManager;

      switch(code) {
        case Codes.keyCodes['K_TAB']:
          domManager.moveToNext(keyShiftState);
          break;
        case Codes.keyCodes['K_TABBACK']:
          domManager.moveToNext(true);
          break;
        case Codes.keyCodes['K_TABFWD']:
          domManager.moveToNext(false);
          break;
      }
    }

    public static applyCommand(Lkc: KeyEvent, keyShiftState: number) {
      DefaultOutput.applyDOMCommand(Lkc, keyShiftState);

      // Notes for potential default-handling extensions:
      // 
      // switch(code) {
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
      // }
      //
      // Note that these would be useful even outside of a DOM context.
    }

    /**
     * Codes matched here may need special handling to apply to an OutputTarget, but still produce output.
     */
    public static forSpecialEmulation(Lkc: KeyEvent): string {
      let code = DefaultOutput.codeForEvent(Lkc);

      switch(code) {
        case Codes.keyCodes['K_BKSP']:
          return '\b';
        case Codes.keyCodes['K_ENTER']:
          return '\n'; 
        case Codes.keyCodes['K_SPACE']:
          return ' ';
        // case Codes.keyCodes['K_DEL']:
        //   return '\u007f'; // 127, ASCII / Unicode control code for DEL.
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
    public static forUnicodeKeynames(Lkc: KeyEvent, ruleBehavior?: RuleBehavior) {
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
        if(ruleBehavior) {
          ruleBehavior.errorLog = ("Suppressing Unicode control code: U_00" + codePoint.toString(16));
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
    public static forBaseKeys(Lkc: KeyEvent, keyShiftState: number, ruleBehavior?: RuleBehavior) {
      let n = Lkc.Lcode;
      // check if exact match to SHIFT's code.  Only the 'default' and 'shift' layers should have default key outputs.
      if(keyShiftState != 0 && keyShiftState != 1) {
        if(ruleBehavior) {
          ruleBehavior.warningLog = "KMW only defines default key output for the 'default' and 'shift' layers!";
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
          if(ruleBehavior) {
            ruleBehavior.errorLog = "Error detected with default mapping for key:  code = " + n + ", shift state = " + (keyShiftState == 1 ? 'shift' : 'default');
          }
        }
      }

      return '';
    }
  }
}