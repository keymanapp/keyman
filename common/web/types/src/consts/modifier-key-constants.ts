/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Modifier key bit-flags
 */

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
}
