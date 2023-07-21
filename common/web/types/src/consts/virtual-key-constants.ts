
// Define standard keycode numbers (exposed for use by other modules)
// TODO-LDML: merge with common\web\keyboard-processor\src\text\codes.ts

/**
 * May include non-US virtual key codes
 */
export const USVirtualKeyCodes = {
  K_BKSP:8,
  K_TAB:9,
  K_ENTER:13,
  K_SHIFT:16,
  K_CONTROL:17,
  K_ALT:18,
  K_PAUSE:19,
  K_CAPS:20,
  K_ESC:27,
  K_SPACE:32,
  K_PGUP:33,
  K_PGDN:34,
  K_END:35,
  K_HOME:36,
  K_LEFT:37,
  K_UP:38,
  K_RIGHT:39,
  K_DOWN:40,
  K_SEL:41,
  K_PRINT:42,
  K_EXEC:43,
  K_INS:45,
  K_DEL:46,
  K_HELP:47,
  K_0:48,
  K_1:49,
  K_2:50,
  K_3:51,
  K_4:52,
  K_5:53,
  K_6:54,
  K_7:55,
  K_8:56,
  K_9:57,
  K_A:65,
  K_B:66,
  K_C:67,
  K_D:68,
  K_E:69,
  K_F:70,
  K_G:71,
  K_H:72,
  K_I:73,
  K_J:74,
  K_K:75,
  K_L:76,
  K_M:77,
  K_N:78,
  K_O:79,
  K_P:80,
  K_Q:81,
  K_R:82,
  K_S:83,
  K_T:84,
  K_U:85,
  K_V:86,
  K_W:87,
  K_X:88,
  K_Y:89,
  K_Z:90,
  K_NP0:96,
  K_NP1:97,
  K_NP2:98,
  K_NP3:99,
  K_NP4:100,
  K_NP5:101,
  K_NP6:102,
  K_NP7:103,
  K_NP8:104,
  K_NP9:105,
  K_NPSTAR:106,
  K_NPPLUS:107,
  K_SEPARATOR:108,
  K_NPMINUS:109,
  K_NPDOT:110,
  K_NPSLASH:111,
  K_F1:112,
  K_F2:113,
  K_F3:114,
  K_F4:115,
  K_F5:116,
  K_F6:117,
  K_F7:118,
  K_F8:119,
  K_F9:120,
  K_F10:121,
  K_F11:122,
  K_F12:123,
  K_NUMLOCK:144,
  K_SCROLL:145,
  K_LSHIFT:160,
  K_RSHIFT:161,
  K_LCONTROL:162,
  K_RCONTROL:163,
  K_LALT:164,
  K_RALT:165,
  K_COLON:186,
  K_EQUAL:187,
  K_COMMA:188,
  K_HYPHEN:189,
  K_PERIOD:190,
  K_SLASH:191,
  K_BKQUOTE:192,
  K_LBRKT:219,
  /**
   * == K_OEM_5, 0xDC
   */
  K_BKSLASH:220,
  K_RBRKT:221,
  K_QUOTE:222,
  /**
   * ISO B00, key to right of left shift, not on US keyboard,
   * 0xE2, K_OEM_102
   */
  K_oE2:226,
  K_OE2:226,
  k_oC1:193,  // ISO B11, ABNT-2 key to left of right shift, not on US keyboard
  k_OC1:193,
  'K_?C1':193,
  'k_?C1':193,
  K_oDF:0xDF,
  K_ODF:0xDF,
  /*K_LOPT:50001,
  K_ROPT:50002,
  K_NUMERALS:50003,
  K_SYMBOLS:50004,
  K_CURRENCIES:50005,
  K_UPPER:50006,
  K_LOWER:50007,
  K_ALPHA:50008,
  K_SHIFTED:50009,
  K_ALTGR:50010,
  K_TABBACK:50011,
  K_TABFWD:50012*/
};

const k = USVirtualKeyCodes;

export type KeyMap = number[][];

export const USVirtualKeyMap: KeyMap = [
  // ` 1 2 3 4 5 6 7 8 9 0 - = [bksp]
  [ k.K_BKQUOTE, k.K_1, k.K_2, k.K_3, k.K_4, k.K_5, k.K_6, k.K_7, k.K_8, k.K_9, k.K_0, k.K_HYPHEN, k.K_EQUAL ],
  // [tab] Q W E R T Y U I O P [ ] \
  [ k.K_Q, k.K_W, k.K_E, k.K_R, k.K_T, k.K_Y, k.K_U, k.K_I, k.K_O, k.K_P, k.K_LBRKT, k.K_RBRKT, k.K_BKSLASH ],
  // [caps] A S D F G H J K L ; ' [enter]
  [ k.K_A, k.K_S, k.K_D, k.K_F, k.K_G, k.K_H, k.K_J, k.K_K, k.K_L, k.K_COLON, k.K_QUOTE ],
  // [shift] Z X C V B N M , . / [shift] *=oE2
  [ k.K_Z, k.K_X, k.K_C, k.K_V, k.K_B, k.K_N, k.K_M, k.K_COMMA, k.K_PERIOD, k.K_SLASH ],
  // space
  [ k.K_SPACE ],
];

export const ISOVirtualKeyMap: KeyMap = [
  // ` 1 2 3 4 5 6 7 8 9 0 - = [bksp]
  [ k.K_BKQUOTE, k.K_1, k.K_2, k.K_3, k.K_4, k.K_5, k.K_6, k.K_7, k.K_8, k.K_9, k.K_0, k.K_HYPHEN, k.K_EQUAL ],
  // [tab] Q W E R T Y U I O P [ ]
  [ k.K_Q, k.K_W, k.K_E, k.K_R, k.K_T, k.K_Y, k.K_U, k.K_I, k.K_O, k.K_P, k.K_LBRKT, k.K_RBRKT  ],
  // [caps] A S D F G H J K L ; ' \ [enter]
  [ k.K_A, k.K_S, k.K_D, k.K_F, k.K_G, k.K_H, k.K_J, k.K_K, k.K_L, k.K_COLON, k.K_QUOTE, k.K_BKSLASH ],
  // [shift] * Z X C V B N M , . / [shift] *=oE2
  [ k.K_oE2, k.K_Z, k.K_X, k.K_C, k.K_V, k.K_B, k.K_N, k.K_M, k.K_COMMA, k.K_PERIOD, k.K_SLASH ],
  // space
  [ k.K_SPACE ],
];

export const JISVirtualKeyMap: KeyMap = [
  // [Hankaku/Zenkaku] 1 2 3 4 5 6 7 8 9 0 - ^ ¥ [bksp]
  [ k.K_1, k.K_2, k.K_3, k.K_4, k.K_5, k.K_6, k.K_7, k.K_8, k.K_9, k.K_0, k.K_HYPHEN, k.K_EQUAL, k.K_BKSLASH /* YEN */ ],
  // [tab] Q W E R T Y U I O P @«`» [ [enter]
  [ k.K_Q, k.K_W, k.K_E, k.K_R, k.K_T, k.K_Y, k.K_U, k.K_I, k.K_O, k.K_P, k.K_BKQUOTE, k.K_LBRKT  ],
  // [caps] A S D F G H J K L ; : ] [enter]
  [ k.K_A, k.K_S, k.K_D, k.K_F, k.K_G, k.K_H, k.K_J, k.K_K, k.K_L, k.K_COLON, k.K_QUOTE, k.K_RBRKT ],
  // [shift] Z X C V B N M , . / _ [shift]
  [ k.K_Z, k.K_X, k.K_C, k.K_V, k.K_B, k.K_N, k.K_M, k.K_COMMA, k.K_PERIOD, k.K_SLASH, k.K_oE2 /* ろ */ ],
  // space
  [ k.K_SPACE ],
];

export const ABNT2VirtualKeyMap: KeyMap = [
  // ' 1 2 3 4 5 6 7 8 9 0 - = [bksp]
  [ k.K_BKQUOTE, k.K_1, k.K_2, k.K_3, k.K_4, k.K_5, k.K_6, k.K_7, k.K_8, k.K_9, k.K_0, k.K_HYPHEN, k.K_EQUAL ],
  // [tab] Q W E R T Y U I O P ´ [
  [ k.K_Q, k.K_W, k.K_E, k.K_R, k.K_T, k.K_Y, k.K_U, k.K_I, k.K_O, k.K_P, k.K_LBRKT, k.K_RBRKT  ],
  // [caps] A S D F G H J K L ç ~ ] [enter]
  [ k.K_A, k.K_S, k.K_D, k.K_F, k.K_G, k.K_H, k.K_J, k.K_K, k.K_L, k.K_COLON, k.K_QUOTE, k.K_BKSLASH ],
  // [shift] \ Z X C V B N M , . ; / [shift]
  [ k.K_oE2, k.K_Z, k.K_X, k.K_C, k.K_V, k.K_B, k.K_N, k.K_M, k.K_COMMA, k.K_PERIOD, k.K_SLASH, k.k_oC1 /* ABNT2 */ ],
  // space
  [ k.K_SPACE ],
];

/**
 * Map from a hardware constant to a keymap
 * For the 'key' see constants.layr_list_hardware_map
 */
export const HardwareToKeymap: Map<string, KeyMap> = new Map(
  [
    ["us",    USVirtualKeyMap],
    ["iso",   ISOVirtualKeyMap],
    ["jis",   JISVirtualKeyMap],
    ["abnt2", ABNT2VirtualKeyMap],
  ]
);

/**
 * Maps LDML VKey Names from CLDR VKey Enum in TR35 to Keyman virtual key codes
 */
export const LdmlVkeyNames: Record<string, number> = {
  'SPACE':     k.K_SPACE,      // 0x20,  // A03
  '0':         k.K_0,          // 0x30,  // E10
  '1':         k.K_1,          // 0x31,  // E01
  '2':         k.K_2,          // 0x32,  // E02
  '3':         k.K_3,          // 0x33,  // E03
  '4':         k.K_4,          // 0x34,  // E04
  '5':         k.K_5,          // 0x35,  // E05
  '6':         k.K_6,          // 0x36,  // E06
  '7':         k.K_7,          // 0x37,  // E07
  '8':         k.K_8,          // 0x38,  // E08
  '9':         k.K_9,          // 0x39,  // E09
  'A':         k.K_A,          // 0x41,  // C01
  'B':         k.K_B,          // 0x42,  // B05
  'C':         k.K_C,          // 0x43,  // B03
  'D':         k.K_D,          // 0x44,  // C03
  'E':         k.K_E,          // 0x45,  // D03
  'F':         k.K_F,          // 0x46,  // C04
  'G':         k.K_G,          // 0x47,  // C05
  'H':         k.K_H,          // 0x48,  // C06
  'I':         k.K_I,          // 0x49,  // D08
  'J':         k.K_J,          // 0x4A,  // C07
  'K':         k.K_K,          // 0x4B,  // C08
  'L':         k.K_L,          // 0x4C,  // C09
  'M':         k.K_M,          // 0x4D,  // B07
  'N':         k.K_N,          // 0x4E,  // B06
  'O':         k.K_O,          // 0x4F,  // D09
  'P':         k.K_P,          // 0x50,  // D10
  'Q':         k.K_Q,          // 0x51,  // D01
  'R':         k.K_R,          // 0x52,  // D04
  'S':         k.K_S,          // 0x53,  // C02
  'T':         k.K_T,          // 0x54,  // D05
  'U':         k.K_U,          // 0x55,  // D07
  'V':         k.K_V,          // 0x56,  // B05
  'W':         k.K_W,          // 0x57,  // D02
  'X':         k.K_X,          // 0x58,  // B02
  'Y':         k.K_Y,          // 0x59,  // D06
  'Z':         k.K_Z,          // 0x5A,  // B01
  'SEMICOLON': k.K_COLON,      // 0xBA,  // C10
  'EQUAL':     k.K_EQUAL,      // 0xBB,  // E12
  'COMMA':     k.K_COMMA,      // 0xBC,  // B08
  'HYPHEN':    k.K_HYPHEN,     // 0xBD,  // E11
  'PERIOD':    k.K_PERIOD,     // 0xBE,  // B09
  'SLASH':     k.K_SLASH,      // 0xBF,  // B10
  'GRAVE':     k.K_BKQUOTE,    // 0xC0,  // E00
  'LBRACKET':  k.K_LBRKT,      // 0xDB,  // D11
  'BACKSLASH': k.K_BKSLASH,    // 0xDC,  // D13
  'RBRACKET':  k.K_RBRKT,      // 0xDD,  // D12
  'QUOTE':     k.K_QUOTE,      // 0xDE,  // C11
  'LESS-THAN': k.K_oE2,        // 0xE2,  // B00  102nd key on European layouts, right of left shift.
  'ABNT2':     k.k_oC1,        // 0xC1,  // B11  Extra key, left of right-shift, ABNT2
};
