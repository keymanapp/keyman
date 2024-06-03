// TODO:  Move to separate folder:  'codes'
// We should start splitting off code needed by keyboards even without a KeyboardProcessor active.

// see also: common/web/types/src/kmx/kmx.ts

const Codes = {
  // Define Keyman Developer modifier bit-flags (exposed for use by other modules)
  // Compare against /common/include/kmx_file.h.  CTRL+F "#define LCTRLFLAG" to find the secton.
  modifierCodes: {
    "LCTRL":0x0001,           // LCTRLFLAG
    "RCTRL":0x0002,           // RCTRLFLAG
    "LALT":0x0004,            // LALTFLAG
    "RALT":0x0008,            // RALTFLAG
    "SHIFT":0x0010,           // K_SHIFTFLAG
    "CTRL":0x0020,            // K_CTRLFLAG
    "ALT":0x0040,             // K_ALTFLAG
    // TENTATIVE:  Represents command keys, which some OSes use for shortcuts we don't
    // want to block.  No rule will ever target a modifier set with this bit set to 1.
    "META":0x0080,            // K_METAFLAG
    "CAPS":0x0100,            // CAPITALFLAG
    "NO_CAPS":0x0200,         // NOTCAPITALFLAG
    "NUM_LOCK":0x0400,        // NUMLOCKFLAG
    "NO_NUM_LOCK":0x0800,     // NOTNUMLOCKFLAG
    "SCROLL_LOCK":0x1000,     // SCROLLFLAG
    "NO_SCROLL_LOCK":0x2000,  // NOTSCROLLFLAG
    "VIRTUAL_KEY":0x4000,     // ISVIRTUALKEY
    "VIRTUAL_CHAR_KEY":0x8000 // VIRTUALCHARKEY // Unused by KMW, but reserved for use by other Keyman engines.

    // Note: keys_mod_other = 0x10000, used by KMX+ for the
    // other modifier flag in layers, > 16 bit so not available here.
    // See keys_mod_other in keyman_core_ldml.ts
  } as {[name: string]: number},

  modifierBitmasks: {
    "ALL":0x007F,
    "ALT_GR_SIM": (0x0001 | 0x0004),
    "CHIRAL":0x001F,    // The base bitmask for chiral keyboards.  Includes SHIFT, which is non-chiral.
    "IS_CHIRAL":0x000F, // Used to test if a bitmask uses a chiral modifier.
    "NON_CHIRAL":0x0070, // The default bitmask, for non-chiral keyboards,
    // Represents all modifier codes not supported by KMW 1.0 legacy keyboards.
    "NON_LEGACY": 0x006F // ALL, but without the SHIFT bit
  } as {[name: string]: number},

  stateBitmasks: {
    "ALL":0x3F00,
    "CAPS":0x0300,
    "NUM_LOCK":0x0C00,
    "SCROLL_LOCK":0x3000
  } as {[name: string]: number},

  // Define standard keycode numbers (exposed for use by other modules)
  keyCodes: {
    "K_BKSP":8,"K_TAB":9,"K_ENTER":13,
    "K_SHIFT":16,"K_CONTROL":17,"K_ALT":18,"K_PAUSE":19,"K_CAPS":20,
    "K_ESC":27,"K_SPACE":32,"K_PGUP":33,
    "K_PGDN":34,"K_END":35,"K_HOME":36,"K_LEFT":37,"K_UP":38,
    "K_RIGHT":39,"K_DOWN":40,"K_SEL":41,"K_PRINT":42,"K_EXEC":43,
    "K_INS":45,"K_DEL":46,"K_HELP":47,"K_0":48,
    "K_1":49,"K_2":50,"K_3":51,"K_4":52,"K_5":53,"K_6":54,"K_7":55,
    "K_8":56,"K_9":57,"K_A":65,"K_B":66,"K_C":67,"K_D":68,"K_E":69,
    "K_F":70,"K_G":71,"K_H":72,"K_I":73,"K_J":74,"K_K":75,"K_L":76,
    "K_M":77,"K_N":78,"K_O":79,"K_P":80,"K_Q":81,"K_R":82,"K_S":83,
    "K_T":84,"K_U":85,"K_V":86,"K_W":87,"K_X":88,"K_Y":89,"K_Z":90,
    "K_NP0":96,"K_NP1":97,"K_NP2":98,
    "K_NP3":99,"K_NP4":100,"K_NP5":101,"K_NP6":102,
    "K_NP7":103,"K_NP8":104,"K_NP9":105,"K_NPSTAR":106,
    "K_NPPLUS":107,"K_SEPARATOR":108,"K_NPMINUS":109,"K_NPDOT":110,
    "K_NPSLASH":111,"K_F1":112,"K_F2":113,"K_F3":114,"K_F4":115,
    "K_F5":116,"K_F6":117,"K_F7":118,"K_F8":119,"K_F9":120,
    "K_F10":121,"K_F11":122,"K_F12":123,"K_NUMLOCK":144,"K_SCROLL":145,
    "K_LSHIFT":160,"K_RSHIFT":161,"K_LCONTROL":162,"K_RCONTROL":163,
    "K_LALT":164,"K_RALT":165,
    "K_COLON":186,"K_EQUAL":187,"K_COMMA":188,"K_HYPHEN":189,
    "K_PERIOD":190,"K_SLASH":191,"K_BKQUOTE":192,
    "K_LBRKT":219,"K_BKSLASH":220,"K_RBRKT":221,
    "K_QUOTE":222,"K_oE2":226,"K_OE2":226,
    "K_LOPT":50001,"K_ROPT":50002,
    "K_NUMERALS":50003,"K_SYMBOLS":50004,"K_CURRENCIES":50005,
    "K_UPPER":50006,"K_LOWER":50007,"K_ALPHA":50008,
    "K_SHIFTED":50009,"K_ALTGR":50010,
    "K_TABBACK":50011,"K_TABFWD":50012
  } as {[name: string]: number},

  codesUS: [
    ['0123456789',';=,-./`', '[\\]\''],
    [')!@#$%^&*(',':+<_>?~', '{|}"']
  ],

  isFrameKey(keyID: string): boolean {
    switch(keyID) {
      // TODO:  consider adding K_ALT, K_CTRL.
      // Not currently here as they typically don't show up on mobile layouts.
      case 'K_SHIFT':
      case 'K_LOPT':
      case 'K_ROPT':
      case 'K_NUMLOCK':  // Often used for numeric layers.
      case 'K_CAPS':
        return true;
      default:
        // 50000:  start of the range defining key-codes for special frame-key symbols
        // and specialized common layer-switching key IDs.  See .keyCodes above.
        if(Codes.keyCodes[keyID] >= 50000) { // A few are used by `sil_euro_latin`.
          return true; // is a 'K_' key defined for layer shifting or 'control' use.
        }
    }

    return false;
  },


  /**
   * Get modifier key state from layer id
   *
   * @param       {string}      layerId       layer id (e.g. ctrlshift)
   * @return      {number}                    modifier key state (desktop keyboards)
   */
   getModifierState(layerId: string): number {
    var modifier=0;
    if(layerId.indexOf('shift') >= 0) {
      modifier |= Codes.modifierCodes['SHIFT'];
    }

    // The chiral checks must not be directly exclusive due each other to visual OSK feedback.
    var ctrlMatched=false;
    if(layerId.indexOf('leftctrl') >= 0) {
      modifier |= Codes.modifierCodes['LCTRL'];
      ctrlMatched=true;
    }
    if(layerId.indexOf('rightctrl') >= 0) {
      modifier |= Codes.modifierCodes['RCTRL'];
      ctrlMatched=true;
    }
    if(layerId.indexOf('ctrl')  >= 0 && !ctrlMatched) {
      modifier |= Codes.modifierCodes['CTRL'];
    }

    var altMatched=false;
    if(layerId.indexOf('leftalt') >= 0) {
      modifier |= Codes.modifierCodes['LALT'];
      altMatched=true;
    }
    if(layerId.indexOf('rightalt') >= 0) {
      modifier |= Codes.modifierCodes['RALT'];
      altMatched=true;
    }
    if(layerId.indexOf('alt')  >= 0 && !altMatched) {
      modifier |= Codes.modifierCodes['ALT'];
    }

    return modifier;
  },

  /**
   * Get state key state from layer id
   *
   * @param       {string}      layerId       layer id (e.g. caps)
   * @return      {number}                    modifier key state (desktop keyboards)
   */
  getStateFromLayer(layerId: string): number {
    var modifier=0;

    if(layerId.indexOf('caps') >= 0) {
      modifier |= Codes.modifierCodes['CAPS'];
    } else {
      modifier |= Codes.modifierCodes['NO_CAPS'];
    }

    return modifier;
  }
}

export default Codes;
