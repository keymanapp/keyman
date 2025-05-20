import { EventEmitter } from "eventemitter3";
import { JSKeyboard, Keyboard, KeyMapping, KeyEvent, Codes, type ProcessorAction } from "keyman/engine/keyboard";
import { KeyEventSourceInterface } from 'keyman/engine/osk';
import { ModifierKeyConstants } from '@keymanapp/common-types';
import { KM_Core, KM_CORE_STATUS } from 'keyman/engine/core-adapter';

interface EventMap {
  /**
   * Designed to pass key events off to any consuming modules/libraries.
   */
  'keyevent': (event: KeyEvent, callback?: (result: ProcessorAction, error?: Error) => void) => void
}

export class HardKeyboardBase extends EventEmitter<EventMap> implements KeyEventSourceInterface<EventMap> { }

export function processForMnemonicsAndLegacy(s: KeyEvent, activeKeyboard: Keyboard, baseLayout: string): KeyEvent {
  if (!activeKeyboard) {
    return s;
  }

  if (activeKeyboard instanceof JSKeyboard) {
    // Mnemonic handling.
    if (activeKeyboard.isMnemonic) {
      // The following will never set a code corresponding to a modifier key, so it's fine to do this,
      // which may change the value of Lcode, here.

      s.setMnemonicCode(!!(s.Lmodifiers & ModifierKeyConstants.K_SHIFTFLAG), !!(s.Lmodifiers & ModifierKeyConstants.CAPITALFLAG));
    }
    // Other minor physical-keyboard adjustments
    if (!activeKeyboard.isMnemonic) {
      // Positional Layout

      /* 13/03/2007 MCD: Swedish: Start mapping of keystroke to US keyboard */
      const Lbase = KeyMapping.languageMap[baseLayout];
      if (Lbase && Lbase['k' + s.Lcode]) {
        s.Lcode = Lbase['k' + s.Lcode];
      }
      /* 13/03/2007 MCD: Swedish: End mapping of keystroke to US keyboard */

      // The second conditional component (re 0x60):  if CTRL or ALT is held down...
      // Do not remap for legacy keyboard compatibility, do not pass Go, do not collect $200.
      // This effectively only permits `default` and `shift` for legacy keyboards.
      //
      // Third:  DO, however, track direct presses of any main modifier key.  The OSK should
      // reflect the current modifier state even for legacy keyboards.
      if (!activeKeyboard.definesPositionalOrMnemonic &&
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
  } else {
    // KMX keyboard
    const status = KM_Core.instance.process_event(activeKeyboard.state, s.Lcode, s.Lmodifiers, 1, 0); // TODO-web-core: properly set keyDown and flags
    if (status != KM_CORE_STATUS.OK) {
      console.error('KeymanWeb: km_core_process_event failed with status: ' + status);
      return s;
    }
    // const core_actions = KM_Core.instance.state_get_actions(activeKeyboard.state);
/*
  process_backspace_action(engine, actions->code_points_to_delete);
  process_output_action(engine, actions->output);
  process_persist_action(engine, actions->persist_options);
  process_alert_action(actions->do_alert);
  gboolean result = process_emit_keystroke_action(engine, actions->emit_keystroke);
  process_capslock_action(actions->new_caps_lock_state);
  finish_process_actions(engine);
*/
  }

  return s;
}
// Intended design:
// - KeyEventKeyboard:  website-integrated handler for hardware-keystroke input; interprets DOM events.
//   - app/web
// - AppPassthroughKeyboard:  WebView-hosted forwarding of hardware key events through to the Web engine.
//   - app/embed