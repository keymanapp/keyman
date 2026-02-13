/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Modifier key bit-flags
 */

import { VisualKeyboardShiftState } from "../kvk/visual-keyboard.js";

/**
 * This type is declared as a `const` rather than as an `enum` for
 * historical reasons. Use `ModifierKeyConstant` instead where possible.
 */
export const ModifierKeyConstants = {
  // Define Keyman Developer modifier bit-flags (exposed for use by other
  // modules) Compare against /common/include/kmx_file.h.  CTRL+F "#define
  // LCTRLFLAG" to find the section.
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

/**
 * Defines the standard modifier key flags used by Keyman. Note that some keys
 * are chiral, and toggle key state is ignored if neither the __FLAG nor the
 * corresponding NOT__FLAG are set.
 */
export enum ModifierKeyConstant {
  NO_MODIFIER =    0,
  LCTRLFLAG =      0x0001, // Left Control flag
  RCTRLFLAG =      0x0002, // Right Control flag
  LALTFLAG =       0x0004, // Left Alt flag
  RALTFLAG =       0x0008, // Right Alt flag
  K_SHIFTFLAG =    0x0010, // Either shift flag
  K_CTRLFLAG =     0x0020, // Either ctrl flag
  K_ALTFLAG =      0x0040, // Either alt flag
  K_METAFLAG =     0x0080, // Either Meta-key flag (tentative).  Not usable in keyboard rules;
                           // Used internally (currently, only by KMW) to ensure Meta-key
                           // shortcuts safely bypass rules
                           // Meta key = Command key on macOS, Windows key on Windows/Linux
  CAPITALFLAG =    0x0100, // Caps lock on
  NOTCAPITALFLAG = 0x0200, // Caps lock NOT on
  NUMLOCKFLAG =    0x0400, // Num lock on
  NOTNUMLOCKFLAG = 0x0800, // Num lock NOT on
  SCROLLFLAG =     0x1000, // Scroll lock on
  NOTSCROLLFLAG =  0x2000, // Scroll lock NOT on
  ISVIRTUALKEY =   0x4000, // It is a Virtual Key Sequence
  VIRTUALCHARKEY = 0x8000, // Keyman 6.0: Virtual Key Cap Sequence NOT YET
};

export const LDML_MODIFIER_TO_KVK_MODIFIER = /* @__PURE__ */ (() => {
  const m = new Map<ModifierKeyConstant, VisualKeyboardShiftState>();
  m.set(ModifierKeyConstants.LCTRLFLAG,      VisualKeyboardShiftState.KVKS_LCTRL);
  m.set(ModifierKeyConstants.RCTRLFLAG,      VisualKeyboardShiftState.KVKS_RCTRL);
  m.set(ModifierKeyConstants.LALTFLAG,       VisualKeyboardShiftState.KVKS_LALT);
  m.set(ModifierKeyConstants.RALTFLAG,       VisualKeyboardShiftState.KVKS_RALT);
  m.set(ModifierKeyConstants.K_SHIFTFLAG,    VisualKeyboardShiftState.KVKS_SHIFT);
  m.set(ModifierKeyConstants.K_CTRLFLAG,     VisualKeyboardShiftState.KVKS_CTRL);
  m.set(ModifierKeyConstants.K_ALTFLAG,      VisualKeyboardShiftState.KVKS_ALT);
  return m;
})();

export const KVK_MODIFIER_TO_LDML_MODIFIER = /* @__PURE__ */ (() => {
  const m = new Map<VisualKeyboardShiftState, ModifierKeyConstant>();
  m.set(VisualKeyboardShiftState.KVKS_LCTRL, ModifierKeyConstants.LCTRLFLAG);
  m.set(VisualKeyboardShiftState.KVKS_RCTRL, ModifierKeyConstants.RCTRLFLAG);
  m.set(VisualKeyboardShiftState.KVKS_LALT,  ModifierKeyConstants.LALTFLAG);
  m.set(VisualKeyboardShiftState.KVKS_RALT,  ModifierKeyConstants.RALTFLAG);
  m.set(VisualKeyboardShiftState.KVKS_SHIFT, ModifierKeyConstants.K_SHIFTFLAG);
  m.set(VisualKeyboardShiftState.KVKS_CTRL,  ModifierKeyConstants.K_CTRLFLAG);
  m.set(VisualKeyboardShiftState.KVKS_ALT,   ModifierKeyConstants.K_ALTFLAG);
  return m;
})();

export function translateLdmlModifiersToVisualKeyboardShift(modifiers: ModifierKeyConstant): VisualKeyboardShiftState {
  if(modifiers == ModifierKeyConstant.NO_MODIFIER) {
    return VisualKeyboardShiftState.KVKS_NORMAL;
  }

  if(modifiers &
    (ModifierKeyConstant.CAPITALFLAG | ModifierKeyConstant.NUMLOCKFLAG | ModifierKeyConstant.SCROLLFLAG)
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

export function translateVisualKeyboardShiftToLdmlModifiers(shift: VisualKeyboardShiftState): ModifierKeyConstant {
  if(shift == VisualKeyboardShiftState.KVKS_NORMAL) {
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



function VkShiftStateToKmxShiftState(ShiftState: number): number {

  interface TVKToKMX {
    VK: VisualKeyboardShiftState; KMX: ModifierKeyConstant;
  }

  const Map: TVKToKMX[] = [
    {VK: VisualKeyboardShiftState.KVKS_SHIFT, KMX: ModifierKeyConstant.K_SHIFTFLAG},
    {VK: VisualKeyboardShiftState.KVKS_CTRL,  KMX: ModifierKeyConstant.K_CTRLFLAG},
    {VK: VisualKeyboardShiftState.KVKS_ALT,   KMX: ModifierKeyConstant.K_ALTFLAG},
    {VK: VisualKeyboardShiftState.KVKS_LCTRL, KMX: ModifierKeyConstant.LCTRLFLAG},
    {VK: VisualKeyboardShiftState.KVKS_RCTRL, KMX: ModifierKeyConstant.RCTRLFLAG},
    {VK: VisualKeyboardShiftState.KVKS_LALT,  KMX: ModifierKeyConstant.LALTFLAG},
    {VK: VisualKeyboardShiftState.KVKS_RALT,  KMX: ModifierKeyConstant.RALTFLAG}
  ];

  let result = 0;
  for(let i = 0; i < Map.length; i++) {
    if (ShiftState & Map[i].VK) {
      result |= Map[i].KMX;
    }
  }

  return result;
}

/**
 * Convert a VK modifier combination bitmask to a hyphen-separated string of
 * modifier names, e.g. for use in layer names and key identifiers. The name
 * order matches the bit flag order from ModifierKeyConstant, not the bit flag
 * order from VisualKeyboardShiftState, for historical reasons.
 */
export function visualKeyboardShiftToLayerName(shift: VisualKeyboardShiftState): string {

  // index is ModifierKeyConstant, not VisualKeyboardShiftState
  const modifierNames: string[] = [
      'leftctrl',
      'rightctrl',
      'leftalt',
      'rightalt',
      'shift',
      'ctrl',
      'alt'
    ];

  const mod = VkShiftStateToKmxShiftState(shift);
  if(mod == 0) {
    return 'default';
  }

  let result = '';
  for(let i = 0; i < modifierNames.length; i++) {
    if(mod & (1 << i)) {
      result += modifierNames[i] + '-';
    }
  }
  return result.substring(0, result.length - 1);
}
