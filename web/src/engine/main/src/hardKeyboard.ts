import { EventEmitter } from "eventemitter3";
import { JSKeyboard, KeyMapping, KeyEvent, Codes } from "keyman/engine/keyboard";
import { type RuleBehavior } from 'keyman/engine/js-processor';
import { KeyEventSourceInterface } from 'keyman/engine/osk';
import { ModifierKeyConstants } from '@keymanapp/common-types';

interface EventMap {
  /**
   * Designed to pass key events off to any consuming modules/libraries.
   */
  'keyevent': (event: KeyEvent, callback?: (result: RuleBehavior, error?: Error) => void) => void
}

export default class HardKeyboard extends EventEmitter<EventMap> implements KeyEventSourceInterface<EventMap> { }

export function processForMnemonicsAndLegacy(s: KeyEvent, activeKeyboard: JSKeyboard, baseLayout: string): KeyEvent {
  // Mnemonic handling.
  if(activeKeyboard && activeKeyboard.isMnemonic) {
    // The following will never set a code corresponding to a modifier key, so it's fine to do this,
    // which may change the value of Lcode, here.

    s.setMnemonicCode(!!(s.Lmodifiers & ModifierKeyConstants.K_SHIFTFLAG), !!(s.Lmodifiers & ModifierKeyConstants.CAPITALFLAG));
  }

  // Other minor physical-keyboard adjustments
  if(activeKeyboard && !activeKeyboard.isMnemonic) {
    // Positional Layout

    /* 13/03/2007 MCD: Swedish: Start mapping of keystroke to US keyboard */
    var Lbase = KeyMapping.languageMap[baseLayout];
    if(Lbase && Lbase['k'+s.Lcode]) {
      s.Lcode=Lbase['k'+s.Lcode];
    }
    /* 13/03/2007 MCD: Swedish: End mapping of keystroke to US keyboard */

    // The second conditional component (re 0x60):  if CTRL or ALT is held down...
    // Do not remap for legacy keyboard compatibility, do not pass Go, do not collect $200.
    // This effectively only permits `default` and `shift` for legacy keyboards.
    //
    // Third:  DO, however, track direct presses of any main modifier key.  The OSK should
    // reflect the current modifier state even for legacy keyboards.
    if(!activeKeyboard.definesPositionalOrMnemonic &&
       !(s.Lmodifiers & Codes.modifierBitmasks.NON_LEGACY) &&
       !s.isModifier) {
      // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
      s = new KeyEvent({
        Lcode: KeyMapping._USKeyCodeToCharCode(s),
        Lmodifiers: 0,
        LisVirtualKey: false,
        vkCode: s.Lcode, // Helps to merge OSK and physical keystroke control paths.
        Lstates: s.Lstates,
        kName: '',
        device: s.device,
        isSynthetic: false
      });
    }
  }

  return s;
}
// Intended design:
// - KeyEventKeyboard:  website-integrated handler for hardware-keystroke input; interprets DOM events.
//   - app/web
// - AppPassthroughKeyboard:  WebView-hosted forwarding of hardware key events through to the Web engine.
//   - app/embed