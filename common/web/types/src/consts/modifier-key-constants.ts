/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Modifier key bit-flags
 */

import { VisualKeyboardShiftState } from "../kvk/visual-keyboard.js";

export const ModifierKeyConstants = {
  // Define Keyman Developer modifier bit-flags (exposed for use by other modules)
  // Compare against /common/include/kmx_file.h.  CTRL+F "#define LCTRLFLAG" to find the secton.
  LCTRLFLAG:      0x0001, // Left Control flag
  RCTRLFLAG:      0x0002, // Right Control flag
  LALTFLAG:       0x0004, // Left Alt flag
  RALTFLAG:       0x0008, // Right Alt flag
  K_SHIFTFLAG:    0x0010, // Either shift flag
  K_CTRLFLAG:     0x0020, // Either ctrl flag
  K_ALTFLAG:      0x0040, // Either alt flag
  K_METAFLAG:     0x0080, // Either Meta-key flag (tentative).  Not usable in keyboard rules;
                          // Used internally (currently, only by KMW) to ensure Meta-key
                          // shortcuts safely bypass rules
                          // Meta key = Command key on macOS, Windows key on Windows/Linux
  CAPITALFLAG:    0x0100, // Caps lock on
  NOTCAPITALFLAG: 0x0200, // Caps lock NOT on
  NUMLOCKFLAG:    0x0400, // Num lock on
  NOTNUMLOCKFLAG: 0x0800, // Num lock NOT on
  SCROLLFLAG:     0x1000, // Scroll lock on
  NOTSCROLLFLAG:  0x2000, // Scroll lock NOT on
  ISVIRTUALKEY:   0x4000, // It is a Virtual Key Sequence
  VIRTUALCHARKEY: 0x8000, // Keyman 6.0: Virtual Key Cap Sequence NOT YET

  // Note: OTHER_MODIFIER = 0x10000, used by KMX+ for the
  // other modifier flag in layers, > 16 bit so not available here.
  // See keys_mod_other in keyman_core_ldml.ts
};

export const LDML_MODIFIER_TO_KVK_MODIFIER = new Map<number, number>();
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.LCTRLFLAG,      VisualKeyboardShiftState.KVKS_LCTRL);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.RCTRLFLAG,      VisualKeyboardShiftState.KVKS_RCTRL);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.LALTFLAG,       VisualKeyboardShiftState.KVKS_LALT);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.RALTFLAG,       VisualKeyboardShiftState.KVKS_RALT);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.K_SHIFTFLAG,    VisualKeyboardShiftState.KVKS_SHIFT);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.K_CTRLFLAG,     VisualKeyboardShiftState.KVKS_CTRL);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.K_ALTFLAG,      VisualKeyboardShiftState.KVKS_ALT);

export const KVK_MODIFIER_TO_LDML_MODIFIER = new Map<number, number>();
KVK_MODIFIER_TO_LDML_MODIFIER.set(VisualKeyboardShiftState.KVKS_LCTRL, ModifierKeyConstants.LCTRLFLAG);
KVK_MODIFIER_TO_LDML_MODIFIER.set(VisualKeyboardShiftState.KVKS_RCTRL, ModifierKeyConstants.RCTRLFLAG);
KVK_MODIFIER_TO_LDML_MODIFIER.set(VisualKeyboardShiftState.KVKS_LALT,  ModifierKeyConstants.LALTFLAG);
KVK_MODIFIER_TO_LDML_MODIFIER.set(VisualKeyboardShiftState.KVKS_RALT,  ModifierKeyConstants.RALTFLAG);
KVK_MODIFIER_TO_LDML_MODIFIER.set(VisualKeyboardShiftState.KVKS_SHIFT, ModifierKeyConstants.K_SHIFTFLAG);
KVK_MODIFIER_TO_LDML_MODIFIER.set(VisualKeyboardShiftState.KVKS_CTRL,  ModifierKeyConstants.K_CTRLFLAG);
KVK_MODIFIER_TO_LDML_MODIFIER.set(VisualKeyboardShiftState.KVKS_ALT,   ModifierKeyConstants.K_ALTFLAG);

export function translateLdmlModifiersToVisualKeyboardShift(modifiers: number): VisualKeyboardShiftState {
  if(modifiers == 0) {
    return VisualKeyboardShiftState.KVKS_NORMAL;
  }

  if(modifiers &
    (ModifierKeyConstants.CAPITALFLAG | ModifierKeyConstants.NUMLOCKFLAG | ModifierKeyConstants.SCROLLFLAG)
  ) {
    // Caps/Num/Scroll are not supported in .kvk, in combination or alone
    return null;
  }

  let shift: VisualKeyboardShiftState = 0;

  for(const mod of LDML_MODIFIER_TO_KVK_MODIFIER.keys()) {
    if(modifiers & mod) {
      shift |= LDML_MODIFIER_TO_KVK_MODIFIER.get(mod);
    }
  }

  return shift;
}

export function translateVisualKeyboardShiftToLdmlModifiers(shift: VisualKeyboardShiftState): number {
  if(shift == 0) {
    return 0;
  }

  let mod = 0;
  for(const state of KVK_MODIFIER_TO_LDML_MODIFIER.keys()) {
    if(shift & state) {
      mod |= KVK_MODIFIER_TO_LDML_MODIFIER.get(state);
    }
  }

  return mod;
}
