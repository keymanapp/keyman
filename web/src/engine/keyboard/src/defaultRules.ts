/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Implementation of default rules
 */

import { ModifierKeyConstants } from '@keymanapp/common-types';
import Codes from './codes.js';
import type KeyEvent from './keyEvent.js';
import { type OutputTarget }  from './outputTarget.interface.js';

export enum EmulationKeystrokes {
  Enter = '\n',
  Backspace = '\b'
}

export class LogMessages {
  errorLog?: string;
  warningLog?: string;
}

/**
 * Defines a collection of static library functions that define KeymanWeb's default (implied) keyboard rule behaviors.
 */
export default class DefaultRules {
  codeForEvent(Lkc: KeyEvent) {
    return Codes.keyCodes[Lkc.kName] || Lkc.Lcode;;
  }

  /**
   * Serves as a default keycode lookup table.  This may be referenced safely by mnemonic handling without fear of side-effects.
   * Also used by Processor.defaultRuleBehavior to generate output after filtering for special cases.
   */
  public forAny(Lkc: KeyEvent, isMnemonic: boolean, logMessages?: LogMessages): string {
    var char = '';

    // A pretty simple table of lookups, corresponding VERY closely to the original defaultKeyOutput.
    if((char = this.forSpecialEmulation(Lkc)) != null) {
      return char;
    } else if(!isMnemonic && ((char = this.forNumpadKeys(Lkc)) != null)) {
      return char;
    } else if((char = this.forUnicodeKeynames(Lkc, logMessages)) != null) {
      return char;
    } else if((char = this.forBaseKeys(Lkc, logMessages)) != null) {
      return char;
    } else {
      // // For headless and embeddded, we may well allow '\t'.  It's DOM mode that has other uses.
      // // Not originally defined for text output within defaultKeyOutput.
      // // We can't enable it yet, as it'll cause hardware keystrokes in the DOM to output '\t' rather
      // // than rely on the browser-default handling.
      let code = this.codeForEvent(Lkc);
      switch(code) {
      //   case Codes.keyCodes['K_TAB']:
      //   case Codes.keyCodes['K_TABBACK']:
      //   case Codes.keyCodes['K_TABFWD']:
      //     return '\t';
        default:
          return null;
      }
    }
  }

  /**
   * isCommand - returns a boolean indicating if a non-text event should be triggered by the keystroke.
   */
  public isCommand(Lkc: KeyEvent): boolean {
    let code = this.codeForEvent(Lkc);

    switch(code) {
      // Should we ever implement them:
      // case Codes.keyCodes['K_LEFT']:  // would not output text, but would alter the caret's position in the context.
      // case Codes.keyCodes['K_RIGHT']:
      //   return true;
      default:
        return false;
    }
  }

  /**
   * Used when a RuleBehavior represents a non-text "command" within the Engine.  This will generally
   * trigger events that require context reset - often by moving the caret or by moving what OutputTarget
   * the caret is in.  However, we let those events perform the actual context reset.
   *
   * Note:  is extended by DOM-aware KeymanWeb code.
   */
  public applyCommand(Lkc: KeyEvent, outputTarget: OutputTarget): void {
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
   * Codes matched here generally have default implementations when in a browser but require emulation
   * for 'synthetic' `OutputTarget`s like `Mock`s, which have no default text handling.
   */
  public forSpecialEmulation(Lkc: KeyEvent): EmulationKeystrokes {
    let code = this.codeForEvent(Lkc);

    switch(code) {
      case Codes.keyCodes['K_BKSP']:
        return EmulationKeystrokes.Backspace;
      case Codes.keyCodes['K_ENTER']:
        return EmulationKeystrokes.Enter;
      // case Codes.keyCodes['K_DEL']:
      //   return '\u007f'; // 127, ASCII / Unicode control code for DEL.
      default:
        return null;
    }
  }

  // Should not be used for mnenomic keyboards.  forAny()'s use of this method checks first.
  public forNumpadKeys(Lkc: KeyEvent) {
    // Translate numpad keystrokes into their non-numpad equivalents
    if(Lkc.Lcode >= Codes.keyCodes["K_NP0"]  &&  Lkc.Lcode <= Codes.keyCodes["K_NPSLASH"]) {
      // Number pad, numlock on
      if(Lkc.Lcode < 106) {
        var Lch = Lkc.Lcode-48;
      } else {
        Lch = Lkc.Lcode-64;
      }
      let ch = String._kmwFromCharCode(Lch); //I3319
      return ch;
    } else {
      return null;
    }
  }

  // Test for fall back to U_xxxxxx key id
  // For this first test, we ignore the keyCode and use the keyName
  public forUnicodeKeynames(Lkc: KeyEvent, logMessages?: LogMessages) {
    const keyName = Lkc.kName;

    // Test for fall back to U_xxxxxx key id
    // For this first test, we ignore the keyCode and use the keyName
    if(!keyName || keyName.substr(0,2) != 'U_') {
      return null;
    }

    let result = '';
    const codePoints = keyName.substr(2).split('_');
    for(let codePoint of codePoints) {
      const codePointValue = parseInt(codePoint, 16);
      if (((0x0 <= codePointValue) && (codePointValue <= 0x1F)) || ((0x80 <= codePointValue) && (codePointValue <= 0x9F)) || isNaN(codePointValue)) {
        // Code points [U_0000 - U_001F] and [U_0080 - U_009F] refer to Unicode C0 and C1 control codes.
        // Check the codePoint number and do not allow output of these codes via U_xxxxxx shortcuts.
        // Also handles invalid identifiers (e.g. `U_ghij`) for which parseInt returns NaN
        if(logMessages) {
          logMessages.errorLog = ("Suppressing Unicode control code in " + keyName);
        }
        // We'll attempt to add valid chars
        continue;
      } else {
        // String.fromCharCode() is inadequate to handle the entire range of Unicode
        // Someday after upgrading to ES2015, can use String.fromCodePoint()
        result += String.kmwFromCharCode(codePointValue);
      }
    }
    return result ? result : null;
  }

  // Test for otherwise unimplemented keys on the the base default & shift layers.
  // Those keys must be blocked by keyboard rules if intentionally unimplemented; otherwise, this function will trigger.
  public forBaseKeys(Lkc: KeyEvent, logMessages?: LogMessages) {
    let n = Lkc.Lcode;
    let keyShiftState = Lkc.Lmodifiers;

    // check if exact match to SHIFT's code.  Only the 'default' and 'shift' layers should have default key outputs.
    // TODO:  Extend to allow AltGr as well - better mnemonic support.
    if (keyShiftState == ModifierKeyConstants.K_SHIFTFLAG) {
      keyShiftState = 1;
    } else if(keyShiftState != 0) {
      if(logMessages) {
        logMessages.warningLog = "KMW only defines default key output for the 'default' and 'shift' layers!";
      }
      return null;
    }

    // Now that keyShiftState is either 0 or 1, we can use the following structure to determine the default output.
    try {
      if(n == Codes.keyCodes['K_SPACE']) {
        return ' ';
      } else if(n >= Codes.keyCodes['K_0'] && n <= Codes.keyCodes['K_9']) { // The number keys.
        return Codes.codesUS[keyShiftState][0][n-Codes.keyCodes['K_0']];
      } else if(n >= Codes.keyCodes['K_A'] && n <= Codes.keyCodes['K_Z']) { // The base letter keys
        return String.fromCharCode(n+(keyShiftState?0:32));  // 32 is the offset from uppercase to lowercase.
      } else if(n >= Codes.keyCodes['K_COLON'] && n <= Codes.keyCodes['K_BKQUOTE']) {
        return Codes.codesUS[keyShiftState][1][n-Codes.keyCodes['K_COLON']];
      } else if(n >= Codes.keyCodes['K_LBRKT'] && n <= Codes.keyCodes['K_QUOTE']) {
        return Codes.codesUS[keyShiftState][2][n-Codes.keyCodes['K_LBRKT']];
      } else if(n == Codes.keyCodes['K_oE2']) {
        return keyShiftState ? '|' : '\\';
      }
    } catch (e) {
      if(logMessages) {
        logMessages.errorLog = "Error detected with default mapping for key:  code = " + n + ", shift state = " + (keyShiftState == 1 ? 'shift' : 'default');
      }
    }

    return null;
  }
}
