
// Define standard keycode numbers (exposed for use by other modules)

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
  K_oC1:193,  // ISO B11, ABNT-2 key to left of right shift, not on US keyboard
  K_OC1:193,
  'K_?C1':193,
  'k_?C1':193,
  K_oDF:0xDF,
  K_ODF:0xDF,
  K_LOPT:50001,
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
  K_TABFWD:50012
};

const k = USVirtualKeyCodes;

/** Map a CLDR scancode to a US VKey ala USVirtualKeyCodes */
export const CLDRScanToUSVirtualKeyCodes = {
  0x02: k.K_1,
  0x03: k.K_2,
  0x04: k.K_3,
  0x05: k.K_4,
  0x06: k.K_5,
  0x07: k.K_6,
  0x08: k.K_7,
  0x09: k.K_8,
  0x0A: k.K_9,
  0x0B: k.K_0,
  0x0C: k.K_HYPHEN,
  0x0D: k.K_EQUAL,

  0x10: k.K_Q,
  0x11: k.K_W,
  0x12: k.K_E,
  0x13: k.K_R,
  0x14: k.K_T,
  0x15: k.K_Y,
  0x16: k.K_U,
  0x17: k.K_I,
  0x18: k.K_O,
  0x19: k.K_P,
  0x1A: k.K_LBRKT,
  0x1B: k.K_RBRKT,

  0x1E: k.K_A,
  0x1F: k.K_S,
  0x20: k.K_D,
  0x21: k.K_F,
  0x22: k.K_G,
  0x23: k.K_H,
  0x24: k.K_J,
  0x25: k.K_K,
  0x26: k.K_L,
  0x27: k.K_COLON,
  0x28: k.K_QUOTE,
  0x29: k.K_BKQUOTE,

  0x2B: k.K_BKSLASH,
  0x2C: k.K_Z,
  0x2D: k.K_X,
  0x2E: k.K_C,
  0x2F: k.K_V,
  0x30: k.K_B,
  0x31: k.K_N,
  0x32: k.K_M,
  0x33: k.K_COMMA,
  0x34: k.K_PERIOD,
  0x35: k.K_SLASH,

  0x39: k.K_SPACE,

  0x56: k.K_oE2, // << Same as 0x7D; found on iso, abnt2
  0x73: k.K_oC1,
  0x7D: k.K_oE2, // << Same as 0x56; found on jis

};

export type KeyMap = number[][];

/**
 * Convert a scan code numerical KeyMap to VKeys
 * @param scans keymap to convert
 * @param badScans output: set of not-found scancodes
 * @returns
 */
export function CLDRScanToKeyMap(scans: KeyMap, badScans?: Set<number>): KeyMap {
  return scans.map((row) => row.map((scan) => CLDRScanToVkey(scan, badScans)));
}

/** Convert one scan code to vkey, or undefined */
export function CLDRScanToVkey(scan: number, badScans?: Set<number>): number {
  /** typescript fun to index the scan table */
  function hasScanCode(key: PropertyKey): key is keyof typeof CLDRScanToUSVirtualKeyCodes {
    return key in CLDRScanToUSVirtualKeyCodes;
  }
  if (hasScanCode(scan)) {
    return CLDRScanToUSVirtualKeyCodes[scan];
  } else {
    badScans?.add(scan);
    return undefined;
  }
}

