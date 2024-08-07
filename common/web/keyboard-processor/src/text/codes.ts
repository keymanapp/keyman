/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyboard key codes and modifier bitmasks.
 */

import { ModifierKeyConstants, USVirtualKeyCodes } from '@keymanapp/common-types';

const Codes = {
  modifierCodes: {
    // Debug-mode keyboards compiled before Keyman 18.0 referenced the `ModifierKeyConstants`
    // constants via the names established below.  We must continue to support them, as they're
    // essentially part of the keyboard API now.
    "LCTRL":            ModifierKeyConstants.LCTRLFLAG,
    "RCTRL":            ModifierKeyConstants.RCTRLFLAG,
    "LALT":             ModifierKeyConstants.LALTFLAG,
    "RALT":             ModifierKeyConstants.RALTFLAG,
    "SHIFT":            ModifierKeyConstants.K_SHIFTFLAG,
    "CTRL":             ModifierKeyConstants.K_CTRLFLAG,
    "ALT":              ModifierKeyConstants.K_ALTFLAG,
    // TENTATIVE:  Represents command keys, which some OSes use for shortcuts we don't
    // want to block.  No rule will ever target a modifier set with this bit set to 1.
    "META":             ModifierKeyConstants.K_METAFLAG,
    "CAPS":             ModifierKeyConstants.CAPITALFLAG,
    "NO_CAPS":          ModifierKeyConstants.NOTCAPITALFLAG,
    "NUM_LOCK":         ModifierKeyConstants.NUMLOCKFLAG,
    "NO_NUM_LOCK":      ModifierKeyConstants.NOTNUMLOCKFLAG,
    "SCROLL_LOCK":      ModifierKeyConstants.SCROLLFLAG,
    "NO_SCROLL_LOCK":   ModifierKeyConstants.NOTSCROLLFLAG,
    "VIRTUAL_KEY":      ModifierKeyConstants.ISVIRTUALKEY,
    "VIRTUAL_CHAR_KEY": ModifierKeyConstants.VIRTUALCHARKEY // Unused by KMW, but reserved for use by other Keyman engines.
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
    ...USVirtualKeyCodes,
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
      modifier |= ModifierKeyConstants.K_SHIFTFLAG;
    }

    // The chiral checks must not be directly exclusive due each other to visual OSK feedback.
    var ctrlMatched=false;
    if(layerId.indexOf('leftctrl') >= 0) {
      modifier |= ModifierKeyConstants.LCTRLFLAG;
      ctrlMatched=true;
    }
    if(layerId.indexOf('rightctrl') >= 0) {
      modifier |= ModifierKeyConstants.RCTRLFLAG;
      ctrlMatched=true;
    }
    if(layerId.indexOf('ctrl')  >= 0 && !ctrlMatched) {
      modifier |= ModifierKeyConstants.K_CTRLFLAG;
    }

    var altMatched=false;
    if(layerId.indexOf('leftalt') >= 0) {
      modifier |= ModifierKeyConstants.LALTFLAG;
      altMatched=true;
    }
    if(layerId.indexOf('rightalt') >= 0) {
      modifier |= ModifierKeyConstants.RALTFLAG;
      altMatched=true;
    }
    if(layerId.indexOf('alt')  >= 0 && !altMatched) {
      modifier |= ModifierKeyConstants.K_ALTFLAG;
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
      modifier |= ModifierKeyConstants.CAPITALFLAG;
    } else {
      modifier |= ModifierKeyConstants.NOTCAPITALFLAG;
    }

    return modifier;
  }
}

export default Codes;
