// Establishes key-code definitions.
/// <reference path="codes.ts" />

namespace com.keyman.text {
  export class Processor {

    /**
     * Get the default key string. If keyName is U_xxxxxx, use that Unicode codepoint.
     * Otherwise, lookup the  virtual key code (physical keyboard mapping)
     *
     * @param   {string}  keyName Name of the key
     * @param   {number}  n
     * @param   {number}  keyShiftState
     * @param   {boolean} usingOSK
     * @param   {Object=} Lelem
     * @return  {string}
     */
    defaultKeyOutput(keyName: string, n: number, keyShiftState: number, usingOSK: boolean, Lelem?: HTMLElement): string {
      let keyman = com.keyman.singleton;
      let domManager = keyman.domManager;

      var ch = '', checkCodes = false;
      var touchAlias = (Lelem && typeof(Lelem.base) != 'undefined');
      // check if exact match to SHIFT's code.  Only the 'default' and 'shift' layers should have default key outputs.
      if(keyShiftState == 0) {
        checkCodes = true;
      } else if (keyShiftState == Codes.modifierCodes['SHIFT']) {
        checkCodes = true; 
        keyShiftState = 1; // It's used as an index.
      } else {
        console.warn("KMW only defines default key output for the 'default' and 'shift' layers!");
      }

      // If this was triggered by the OSK -or- if it was triggered within a touch-aliased DIV element.
      if(touchAlias || usingOSK) {
        var code = Codes.keyCodes[keyName];
        if(!code) {
          code = n;
        }

        switch(code) {
          case Codes.keyCodes['K_BKSP']:  //Only desktop UI, not touch devices. TODO: add repeat while mouse down for desktop UI
            keyman.interface.defaultBackspace();
            break;
          case Codes.keyCodes['K_TAB']:
            domManager.moveToNext(keyShiftState);
            break;
          case Codes.keyCodes['K_TABBACK']:
            domManager.moveToNext(true);
            break;
          case Codes.keyCodes['K_TABFWD']:
            domManager.moveToNext(false);
            break;
          case Codes.keyCodes['K_ENTER']:
            // Insert new line in text area fields
            if(Lelem.nodeName == 'TEXTAREA' || (typeof Lelem.base != 'undefined' && Lelem.base.nodeName == 'TEXTAREA')) {
              return '\n';
            // Or move to next field from TEXT fields
            } else if(usingOSK) {
              var inputEle: HTMLInputElement;
              if(com.keyman.Util.instanceof(Lelem, "HTMLInputElement")) {
                inputEle = <HTMLInputElement> Lelem;
              } else if(typeof(Lelem.base) != 'undefined' && com.keyman.Util.instanceof(Lelem.base, "HTMLInputElement")) {
                inputEle = <HTMLInputElement> Lelem.base;
              }

              if (inputEle && (inputEle.type == 'search' || inputEle.type == 'submit')) {
                inputEle.disabled=false;
                inputEle.form.submit();
              } else {
                domManager.moveToNext(false);
              }
            }
            break;
          case Codes.keyCodes['K_SPACE']:
            return ' ';
          // break;
          //
          // // Problem:  clusters, and doing them right.
          // // The commented-out code below should be a decent starting point, but clusters make it complex.
          //
          // case VisualKeyboard.keyCodes['K_LEFT']:
          //   if(touchAlias) {
          //     var caretPos = keymanweb.getTextCaret(Lelem);
          //     keymanweb.setTextCaret(Lelem, caretPos - 1 >= 0 ? caretPos - 1 : 0);
          //   }
          //   break;
          // case VisualKeyboard.keyCodes['K_RIGHT']:
          //   if(touchAlias) {
          //     var caretPos = keymanweb.getTextCaret(Lelem);
          //     keymanweb.setTextCaret(Lelem, caretPos + 1);
          //   }
          //   if(code == VisualKeyboard.keyCodes['K_RIGHT']) {
          //     break;
          //   }
          // // Should we include this?  It could be tricky to do correctly...
          // case VisualKeyboard.keyCodes['K_DEL']:
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

      // TODO:  Refactor the overloading of the 'n' parameter here into separate methods.

      // Test for fall back to U_xxxxxx key id
      // For this first test, we ignore the keyCode and use the keyName
      if((keyName.substr(0,2) == 'U_')) {
        var codePoint = parseInt(keyName.substr(2,6), 16);
        if (((0x0 <= codePoint) && (codePoint <= 0x1F)) || ((0x80 <= codePoint) && (codePoint <= 0x9F))) {
          // Code points [U_0000 - U_001F] and [U_0080 - U_009F] refer to Unicode C0 and C1 control codes.
          // Check the codePoint number and do not allow output of these codes via U_xxxxxx shortcuts.
          console.log("Suppressing Unicode control code: U_00" + codePoint.toString(16));
          return ch;
        } else {
          // String.fromCharCode() is inadequate to handle the entire range of Unicode
          // Someday after upgrading to ES2015, can use String.fromCodePoint()
          ch=String.kmwFromCharCode(codePoint);
        }
        // Hereafter, we refer to keyCodes.
      } else if(checkCodes) { // keyShiftState can only be '1' or '2'.
        try {
          if(n >= Codes.keyCodes['K_0'] && n <= Codes.keyCodes['K_9']) { // The number keys.
            ch = Codes.codesUS[keyShiftState][0][n-Codes.keyCodes['K_0']];
          } else if(n >= Codes.keyCodes['K_A'] && n <= Codes.keyCodes['K_Z']) { // The base letter keys
            ch = String.fromCharCode(n+(keyShiftState?0:32));  // 32 is the offset from uppercase to lowercase.
          } else if(n >= Codes.keyCodes['K_COLON'] && n <= Codes.keyCodes['K_BKQUOTE']) {
            ch = Codes.codesUS[keyShiftState][1][n-Codes.keyCodes['K_COLON']];
          } else if(n >= Codes.keyCodes['K_LBRKT'] && n <= Codes.keyCodes['K_QUOTE']) {
            ch = Codes.codesUS[keyShiftState][2][n-Codes.keyCodes['K_LBRKT']];
          }
        } catch (e) {
          console.error("Error detected with default mapping for key:  code = " + n + ", shift state = " + (keyShiftState == 1 ? 'shift' : 'default'));
        }
      }
      return ch;
    }
  }
}