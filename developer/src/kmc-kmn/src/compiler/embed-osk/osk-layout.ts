/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-11-24
 */
import { USVirtualKeyCodes as V } from "@keymanapp/common-types";

/**
 * On-Screen Keyboard virtual key codes and positions for the Windows "kbdus"
 * hardware layout which Keyman for Windows uses as its basic positional layout,
 * mapped against the LDML supported base layouts. Only the character-producing
 * keys are included; frame keys are excluded.
 */
export const oskLayouts = {
  us: [
    [V.K_BKQUOTE, V.K_1, V.K_2, V.K_3, V.K_4, V.K_5, V.K_6, V.K_7, V.K_8, V.K_9, V.K_0, V.K_HYPHEN, V.K_EQUAL],
    [V.K_Q, V.K_W, V.K_E, V.K_R, V.K_T, V.K_Y, V.K_U, V.K_I, V.K_O, V.K_P, V.K_LBRKT, V.K_RBRKT, V.K_BKSLASH],
    [V.K_A, V.K_S, V.K_D, V.K_F, V.K_G, V.K_H, V.K_J, V.K_K, V.K_L, V.K_COLON, V.K_QUOTE],
    [V.K_Z, V.K_X, V.K_C, V.K_V, V.K_B, V.K_N, V.K_M, V.K_COMMA, V.K_PERIOD, V.K_SLASH],
    [V.K_SPACE],
  ],

  iso: [
    [V.K_BKQUOTE, V.K_1, V.K_2, V.K_3, V.K_4, V.K_5, V.K_6, V.K_7, V.K_8, V.K_9, V.K_0, V.K_HYPHEN, V.K_EQUAL],
    [V.K_Q, V.K_W, V.K_E, V.K_R, V.K_T, V.K_Y, V.K_U, V.K_I, V.K_O, V.K_P, V.K_LBRKT, V.K_RBRKT],       // differences from us: K_BKSLASH removed here
    [V.K_A, V.K_S, V.K_D, V.K_F, V.K_G, V.K_H, V.K_J, V.K_K, V.K_L, V.K_COLON, V.K_QUOTE, V.K_BKSLASH], // differences from us: K_BKSLASH add here
    [V.K_OE2, V.K_Z, V.K_X, V.K_C, V.K_V, V.K_B, V.K_N, V.K_M, V.K_COMMA, V.K_PERIOD, V.K_SLASH],       // differences from us: adds K_OE2, "102nd key"
    [V.K_SPACE],
  ],

  // abnt2, jis, ks may be needed in the future
};