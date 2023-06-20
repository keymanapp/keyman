KeymanWeb.KR(new Keyboard_chirality());

/**
 * Please note that this is an experimental handwritten keyboard designed for initial testing of modifier chirality support for KeymanWeb.
 * As it is handcoded, it uses multiple codesize optimization techniques that Keyman Developer simply does not and will not employ, and it
 * will only work with KeymanWeb based implementations.  (Not supported for Keyman Desktop or Keyman for Mac.)
 *
 * (This keyboard was written before Developer's implementation of chirality support and was used to bootstrap KeymanWeb chirality, serving
 *  as the initial wave of testing.)
 */

function Keyboard_chirality() {
  // Refer to $KEYMAN_ROOT/common/web/keyboard-processor/src/text/codes.ts, same method name.
  // May be moved within common/web/types at some point in the future.
  var getModifierState = function(layerId) {
    // Refer to C:\keymanapp\keyman\common\web\keyboard-processor\src\keyboards\keyboardHarness.ts,
    // `MinimalKeyboardHarness`.
    let osk = keyman.osk;  // Codes endpoint, as part of the standard keyboard harness.

    var modifier=0;
    if(layerId.indexOf('shift') >= 0) {
      modifier |= osk.modifierCodes['SHIFT'];
    }

    // The chiral checks must not be directly exclusive due each other to visual OSK feedback.
    var ctrlMatched=false;
    if(layerId.indexOf('leftctrl') >= 0) {
      modifier |= osk.modifierCodes['LCTRL'];
      ctrlMatched=true;
    }
    if(layerId.indexOf('rightctrl') >= 0) {
      modifier |= osk.modifierCodes['RCTRL'];
      ctrlMatched=true;
    }
    if(layerId.indexOf('ctrl')  >= 0 && !ctrlMatched) {
      modifier |= osk.modifierCodes['CTRL'];
    }

    var altMatched=false;
    if(layerId.indexOf('leftalt') >= 0) {
      modifier |= osk.modifierCodes['LALT'];
      altMatched=true;
    }
    if(layerId.indexOf('rightalt') >= 0) {
      modifier |= osk.modifierCodes['RALT'];
      altMatched=true;
    }
    if(layerId.indexOf('alt')  >= 0 && !altMatched) {
      modifier |= osk.modifierCodes['ALT'];
    }

    return modifier;
  }

  this.KI = "Keyboard_chirality";
  this.KN = "Development Chirality Test Keyboard";
  this.KMBM = 0x001F;
  this.KV = {
      F: ' 1em "Arial"',
      K102: 0,
      KLS: { 'default': new Array("`", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=", "", "", "", "q", "w", "e", "r", "t", "y", "u", "i", "o", "p", "[", "]", "\\", "", "", "", "a", "s", "d", "f", "g", "h", "j", "k", "l", ";", "'",  "", "", "", "", "", "", "z", "x", "c", "v", "b", "n", "m", ",", ".", "/", "", "", "", "", "", ""),
               'shift': new Array("~", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "_", "+", "", "", "", "Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P", "{", "}", "|",  "", "", "", "A", "S", "D", "F", "G", "H", "J", "K", "L", ":", "\"", "", "", "", "", "", "", "Z", "X", "C", "V", "B", "N", "M", "<", ">", "?", "", "", "", "", "", ""),
            'leftctrl': new Array("",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "", "",  "", "ʍ", "ə", "ɹ", "ð", "ʏ", "ʉ", "ɨ", "ɵ", "ʘ", "",  "",  "",   "", "", "", "ɑ", "ʃ", "",  "ɸ", "ɣ", "ɥ", "ɟ", "",  "ɬ", "ː", "",   "", "", "", "", "", "", "ʒ", "χ", "ç", "ʋ", "β", "ɲ", "",  "",  "",  "",  "", "", "", "", "", ""),
      'leftctrl-shift': new Array("",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "", "", "ʡ", "",  "ɘ", "ʀ", "θ", "",  "ɯ", "ɪ", "",  "",  "",  "",  "",   "", "", "", "ɒ", "ᶘ", "",  "",  "ɢ", "ʜ", "",  "",  "ʟ", "ː", "",   "", "", "", "", "", "", "ᶚ", "",  "",  "",  "ʙ", "ɴ", "",  "",  "",  "ʔ", "", "", "", "", "", ""),
             'leftalt': new Array("",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "", "",  "", "", "ɛ", "ɽ", "ʈ", "ɥ", "ʊ", "",  "ɔ", "",  "",  "",  "",   "", "", "", "æ", "",  "ɖ", "",   "", "ɦ", "ʝ", "",  "ɭ", "",  "",   "", "", "", "", "", "", "ʐ", "",  "ɕ", "", "",  "ɳ", "",  "",  "",  "",  "", "", "", "", "", ""),
            'rightalt': new Array("",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "", "", "ʠ", "ɰ", "ɜ", "ɾ", "ƭ", "",  "ʌ", "",  "ø", "ƥ", "",  "",  "",   "", "", "", "ɐ", "σ", "ɗ", "",  "ɠ", "ħ", "ʄ", "ƙ", "ɮ", "",  "",   "", "", "", "", "", "", "ʑ", "",  "ƈ", "",  "ɓ", "ŋ", "ɱ", "",  "",  "",  "", "", "", "", "", ""),
       'leftalt-shift': new Array("",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "", "", "ʢ", "",  "œ", "ɻ", "",  "",  "",  "ᵻ", "ɞ", "",  "",  "",  "",   "", "", "", "",  "",  "",  "",  "",  "",  "",  "",  "ʎ", "",  "",   "", "", "", "", "", "", "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "", "", "", "", ""),
      'rightalt-shift': new Array("",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "", "", "",  "",  "ɶ", "ʁ", "",  "",  "ᵾ", "ᵼ", "ɤ", "",  "",  "",  "",   "", "", "", "ᴂ", "",  "",  "",  "ʛ", "ɧ", "",  "",  "ɺ", "",  "",   "", "", "", "", "", "", "",  "",  "",  "",  "",  "",  "",  "",  "",  "",  "", "", "", "", "", "")
      }
  };
  this.KH = '';
  this.KM = 0;
  this.KBVER = "1.0";

  this.dfltCodes = ["K_BKQUOTE","K_1","K_2","K_3","K_4","K_5","K_6","K_7","K_8","K_9","K_0",
      "K_HYPHEN","K_EQUAL","K_*","K_*","K_*","K_Q","K_W","K_E","K_R","K_T",
      "K_Y","K_U","K_I","K_O","K_P","K_LBRKT","K_RBRKT","K_BKSLASH","K_*",
      "K_*","K_*","K_A","K_S","K_D","K_F","K_G","K_H","K_J","K_K","K_L",
      "K_COLON","K_QUOTE","K_*","K_*","K_*","K_*","K_*","K_oE2",
      "K_Z","K_X","K_C","K_V","K_B","K_N","K_M","K_COMMA","K_PERIOD",
      "K_SLASH","K_*","K_*","K_*","K_*","K_*","K_SPACE"];

  this.gs = function (t, e) {
      return this.g0(t, e);
  };
  this.g0 = function (t, e) {
    var k = KeymanWeb, r = 0, m = 0;

    // Refer to C:\keymanapp\keyman\common\web\keyboard-processor\src\keyboards\keyboardHarness.ts,
    // `MinimalKeyboardHarness`.
    var Constants = keyman.osk;  // Holds anchor points for relevant Codes properties.

    // Handwritten time!
    var kls = this.KV.KLS;

    var layers = ['default', 'shift', 'leftctrl', 'leftctrl-shift', 'leftalt', 'rightalt', 'leftalt-shift', 'rightalt-shift'];

    // Maps keystrokes by base key-codes and array into the key symbols displayed in KLS.
    for(var i = 0; i < layers.length; i++) {
      // Obtain the modifier code to match for the selected layer.
      // The following uses a non-public property potentially subject to change in the future.
      var modCode = Constants.modifierCodes['VIRTUAL_KEY'] | getModifierState(layers[i]);
      var layer = layers[i];

      for(var key=0; key < kls[layer].length; key++) {
        var keySymbol = this.dfltCodes[key];

        if(keySymbol == "K_*") {
          continue;
        } else if(kls[layer][key] != '') {
          if (k.KKM(e, modCode, Constants.keyCodes[keySymbol])) {
            r = m = 1;
            if(k.KSM(e, Constants.modifierCodes['CAPS'])) {
              k.KO(0, t, kls[layer][key].toUpperCase());
            } else {
              k.KO(0, t, kls[layer][key]);
            }
            return r;
          }
        }
      }
    }

    return r;
  };
}