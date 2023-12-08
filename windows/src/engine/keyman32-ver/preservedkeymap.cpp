/*
  Name:             preservedkeymap
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Dec 2012

  Modified Date:    10 Feb 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Dec 2012 - mcdurdin - I3623 - V9.0 - Add preserved key mapping from .kmx file
                    17 Jan 2013 - mcdurdin - I3762 - V9.0 - underlying layout cannot be controlled cleanly via TSF so support translation
                    11 Aug 2013 - mcdurdin - I3775 - V9.0 - Fail I3762 - AltGr combinations are not matched on French base layout
                    15 Apr 2014 - mcdurdin - I4128 - V9.0 - Shift states still not working with unprocessed keys in V9
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    03 Aug 2014 - mcdurdin - I4351 - V9.0 - RALT keys are not preserved correctly in TIP
                    27 Jan 2015 - mcdurdin - I4575 - V9.0 - Support output of TAB and ENTER for unmatched key events
                    27 Jan 2015 - mcdurdin - I4575 - V9.0 - Support output of TAB and ENTER for unmatched key events
                    10 Feb 2015 - mcdurdin - I4592 - V9.0 - If a computer does not have US keyboard installed, then AltGr rules can go wrong
*/
   // I3623   // I4169   // I4575   // I4575
#include "pch.h"

WORD USVKToScanCodeToLayoutVK(WORD VKey);   // I3762

struct PreservedKey
{
  GUID guid;
  TF_PRESERVEDKEY key;
};

class PreservedKeyMap
{
public:
  /**
   * Updates a map of preserved keys (with GUID) that are used in keyboard rules.
   * Passing in NULL for pPreservedKeys will cause cPreservedKeys to be set to the number
   * of preserved keys needed for the supplied pKeyboard; this should be used in creating
   * pPreservedKeys list to sufficient size. When pPreservedKeys list is passed the
   * cPreservedKeys will be the actual count of the number of unique pPreservedKeys
   *
   * @param   pKeyboard  the keyboard for which the rules will be extracted from
   * @param   pPreservedKeys  preallocated array for preserved keys to be filled in, or nullptr to retrieve required size
   * @param   cPreservedKeys  number of preserved keys in pPreservedKeys - or the size pPreservedKeys needs to be
   * @return  BOOL  return TRUE on success
   */
  BOOL MapKeyboard(km_core_keyboard *pKeyboard, PreservedKey **pPreservedKeys, size_t *cPreservedKeys);

private:
  BOOL m_BaseKeyboardUsesAltGr;   // I4592
  UINT ShiftToTSFShift(UINT ShiftFlags);
  BOOL MapUSCharToVK(UINT *puKey, UINT *puShiftFlags);
  BOOL MapKeyRule(km_core_keyboard_key *pKeyRule, TF_PRESERVEDKEY *pPreservedKey);
  BOOL IsMatchingKey(PreservedKey *pKey, PreservedKey *pKeys, size_t cKeys);
};


const struct
{
  UINT key;
  BOOL shift;
} USCharMap[] = {
  { VK_SPACE, FALSE }, // 20 ' '
  { '1', TRUE }, // 21 '!'
  { VK_QUOTE, TRUE }, // 22 '"'
  { '3', TRUE }, // 23 '#'
  { '4', TRUE }, // 24 '$'
  { '5', TRUE }, // 25 '%'
  { '7', TRUE }, // 26 '&'
  { VK_QUOTE, FALSE }, // 27 '''
  { '9', TRUE }, // 28 '('
  { '0', TRUE }, // 29 ')'
  { '8', TRUE }, // 2A '*'
  { VK_EQUAL, TRUE }, // 2B '+'
  { VK_COMMA, FALSE }, // 2C ','
  { VK_HYPHEN, FALSE }, // 2D '-'
  { VK_PERIOD, FALSE }, // 2E '.'
  { VK_SLASH, FALSE }, // 2F '/'

  { '0', FALSE }, // 30 '0'
  { '1', FALSE }, // 31 '1'
  { '2', FALSE }, // 32 '2'
  { '3', FALSE }, // 33 '3'
  { '4', FALSE }, // 34 '4'
  { '5', FALSE }, // 35 '5'
  { '6', FALSE }, // 36 '6'
  { '7', FALSE }, // 37 '7'
  { '8', FALSE }, // 38 '8'
  { '9', FALSE }, // 39 '9'
  { VK_COLON, TRUE }, // 3A ':'
  { VK_COLON, FALSE }, // 3B ';'
  { VK_COMMA, TRUE }, // 3C '<'
  { VK_EQUAL, FALSE }, // 3D '='
  { VK_PERIOD, TRUE }, // 3E '>'
  { VK_SLASH, TRUE }, // 3F '?'

  { '2', TRUE }, // 40 '@'
  { 'A', TRUE }, // 41 'A'
  { 'B', TRUE }, // 42 'B'
  { 'C', TRUE }, // 43 'C'
  { 'D', TRUE }, // 44 'D'
  { 'E', TRUE }, // 45 'E'
  { 'F', TRUE }, // 46 'F'
  { 'G', TRUE }, // 47 'G'
  { 'H', TRUE }, // 48 'H'
  { 'I', TRUE }, // 49 'I'
  { 'J', TRUE }, // 4A 'J'
  { 'K', TRUE }, // 4B 'K'
  { 'L', TRUE }, // 4C 'L'
  { 'M', TRUE }, // 4D 'M'
  { 'N', TRUE }, // 4E 'N'
  { 'O', TRUE }, // 4F 'O'

  { 'P', TRUE }, // 50 'P'
  { 'Q', TRUE }, // 51 'Q'
  { 'R', TRUE }, // 52 'R'
  { 'S', TRUE }, // 53 'S'
  { 'T', TRUE }, // 54 'T'
  { 'U', TRUE }, // 55 'U'
  { 'V', TRUE }, // 56 'V'
  { 'W', TRUE }, // 57 'W'
  { 'X', TRUE }, // 58 'X'
  { 'Y', TRUE }, // 59 'Y'
  { 'Z', TRUE }, // 5A 'Z'
  { VK_LBRKT, FALSE }, // 5B '['
  { VK_BKSLASH, FALSE }, // 5C '\'
  { VK_RBRKT, FALSE }, // 5D ']'
  { '6', TRUE }, // 5E '^'
  { VK_HYPHEN, TRUE }, // 5F '_'

  { VK_ACCENT, FALSE }, // 60 '`'
  { 'A', FALSE }, // 61 'a'
  { 'B', FALSE }, // 62 'b'
  { 'C', FALSE }, // 63 'c'
  { 'D', FALSE }, // 64 'd'
  { 'E', FALSE }, // 65 'e'
  { 'F', FALSE }, // 66 'f'
  { 'G', FALSE }, // 67 'g'
  { 'H', FALSE }, // 68 'h'
  { 'I', FALSE }, // 69 'i'
  { 'J', FALSE }, // 6A 'j'
  { 'K', FALSE }, // 6B 'k'
  { 'L', FALSE }, // 6C 'l'
  { 'M', FALSE }, // 6D 'm'
  { 'N', FALSE }, // 6E 'n'
  { 'O', FALSE }, // 6F 'o'

  { 'P', FALSE }, // 70 'p'
  { 'Q', FALSE }, // 71 'q'
  { 'R', FALSE }, // 72 'r'
  { 'S', FALSE }, // 73 's'
  { 'T', FALSE }, // 74 't'
  { 'U', FALSE }, // 75 'u'
  { 'V', FALSE }, // 76 'v'
  { 'W', FALSE }, // 77 'w'
  { 'X', FALSE }, // 78 'x'
  { 'Y', FALSE }, // 79 'y'
  { 'Z', FALSE }, // 7A 'z'
  { VK_LBRKT, TRUE }, // 7B '{'
  { VK_BKSLASH, TRUE }, // 7C '|'
  { VK_RBRKT, TRUE }, // 7D '}'
  { VK_ACCENT, TRUE }  // 7E '~'
};

BOOL PreservedKeyMap::MapUSCharToVK(UINT *puKey, UINT *puShiftFlags)
{
  if(*puKey >= 0x20 && *puKey < 0x7F)
  {
    *puShiftFlags = ISVIRTUALKEY | (USCharMap[*puKey - 0x20].shift ? K_SHIFTFLAG : 0);
    *puKey = USCharMap[*puKey - 0x20].key;
    return TRUE;
  }
  return FALSE;
}

UINT PreservedKeyMap::ShiftToTSFShift(UINT ShiftFlags)
{
  UINT res = 0;
  if(ShiftFlags & K_SHIFTFLAG) res |= TF_MOD_SHIFT;
  if(ShiftFlags & K_CTRLFLAG) res |= TF_MOD_CONTROL;
  if(ShiftFlags & K_ALTFLAG) res |= TF_MOD_ALT;
  if(ShiftFlags & LCTRLFLAG) res |= TF_MOD_LCONTROL;
  if(ShiftFlags & RCTRLFLAG) res |= TF_MOD_RCONTROL;
  if(ShiftFlags & LALTFLAG) res |= TF_MOD_LALT;
  if(ShiftFlags & RALTFLAG) {
    if(m_BaseKeyboardUsesAltGr) {
      res |= TF_MOD_RALT|TF_MOD_LCONTROL;   // We need to mimic the Windows RAlt+LCtrl == AltGr behaviour   // I4592
    } else {
      res |= TF_MOD_RALT;  // I3775 = no longer applicable ifwe use US English base keyboard  // I4351
    }
  }
  return res;
}

BOOL
PreservedKeyMap::MapKeyRule(km_core_keyboard_key *pKeyRule, TF_PRESERVEDKEY *pPreservedKey) {
  UINT ShiftFlags;
  UINT Key;

  Key        = pKeyRule->key;
  ShiftFlags = pKeyRule->modifier_flag;

  if (Key == VK_BACK || Key == VK_RETURN || Key == VK_TAB)  // I4575
  {
    //
    // We never map backspace, return or tab because these are the only supported virtual key outputs,
    // and result in recursion.  Sadly, this is an imperfect solution forced upon us by preserved key
    // limitations.
    //
    // Other virtual key output will be blocked with this version.
    return FALSE;
  }

  if (Key > 255) {
    //
    // Touch-defined keys have a value > 255, but these should never be preserved
    //
    return FALSE;
  }

  pPreservedKey->uVKey      = (UINT)USVKToScanCodeToLayoutVK((WORD)Key);  // I3762
  pPreservedKey->uModifiers = ShiftToTSFShift(ShiftFlags);

  return TRUE;
}

BOOL PreservedKeyMap::IsMatchingKey(PreservedKey *pKey, PreservedKey *pKeys, size_t cKeys)
{
  for(size_t i = 0; i < cKeys; i++)
  {
    if(pKeys[i].key.uModifiers == pKey->key.uModifiers && pKeys[i].key.uVKey == pKey->key.uVKey)
    {
      return TRUE;
    }
  }
  return FALSE;
}

BOOL
PreservedKeyMap::MapKeyboard(km_core_keyboard *pKeyboard, PreservedKey **pPreservedKeys, size_t *cPreservedKeys) {
  size_t cKeys = 0, cRules = 0, n = 0;
  DWORD i;

  m_BaseKeyboardUsesAltGr = KeyboardGivesCtrlRAltForRAlt();  // I4592

  // This is not the same as m_BaseKeyboardUsesAltGr -- we are turning
  // the simulation back on after (possibly) turning it off, for a
  // consistent experience. TODO: determine if m_BaseKeyboardUseAltGr
  // is still needed given we always use kbdus as base for Keyman 10+
  BOOL bSimulateAltGr = Globals::get_SimulateAltGr();

  // We only want to translate RALT and RALT+SHIFT for Ctrl+Alt rules.
  // So we exclude all our other favourite modifier keys.
  const UINT RALT_MATCHING_MASK = TF_MOD_CONTROL | TF_MOD_ALT | TF_MOD_LCONTROL | TF_MOD_RCONTROL | TF_MOD_LALT | TF_MOD_RALT;

  // This is where we will call down to the api to get list of keys used in the keyboard rules
  km_core_keyboard_key *kb_key_list;

  km_core_status err_status = km_core_keyboard_get_key_list(pKeyboard, &kb_key_list);
  if ((err_status != KM_CORE_STATUS_OK) || (kb_key_list ==nullptr)) {
    return FALSE;
  }

  km_core_keyboard_key *key_rule_it = kb_key_list;
  for (; key_rule_it->key; ++key_rule_it) {
    ++cRules;
  }
  cKeys = cRules;
  if (cKeys == 0) {
    km_core_keyboard_key_list_dispose(kb_key_list);
    return FALSE;
  }

  if (bSimulateAltGr) {
    // We might need twice as many preserved keys to map both LCtrl+LAlt+x and RAlt+x
    cKeys *= 2;
  }

  if (pPreservedKeys == NULL) {
    *cPreservedKeys = cKeys;
    km_core_keyboard_key_list_dispose(kb_key_list);
    return TRUE;
  }

  if (*cPreservedKeys < cKeys) {
    km_core_keyboard_key_list_dispose(kb_key_list);
    return FALSE;
  }

  PreservedKey *pKeys = *pPreservedKeys;


  for (i = 0; i < cRules; i++) {
    // If we have a key rule for the key, we should preserve it
    if (MapKeyRule(&kb_key_list[i], &pKeys[n].key)) {
      // Don't attempt to add the same preserved key twice. Bad things happen
      if (!IsMatchingKey(&pKeys[n], pKeys, n)) {
        CoCreateGuid(&pKeys[n].guid);
        n++;

        if (bSimulateAltGr && (pKeys[n - 1].key.uModifiers & RALT_MATCHING_MASK) == TF_MOD_RALT) {
          // Do this for RALT and RALT+SHIFT only, so we've tested against that mask
          // Copy the key and fix modifiers
          pKeys[n].key            = pKeys[n - 1].key;
          pKeys[n].key.uModifiers = (pKeys[n].key.uModifiers & ~TF_MOD_RALT) | TF_MOD_LCONTROL | TF_MOD_LALT;
          CoCreateGuid(&pKeys[n].guid);
          n++;
        }
      }
    }
  }

  km_core_keyboard_key_list_dispose(kb_key_list);

  *cPreservedKeys = n;  // return actual count of allocated keys, usually smaller than allocated count
  return TRUE;
}





extern "C" __declspec(dllexport) BOOL WINAPI GetKeyboardPreservedKeys(PreservedKey **pPreservedKeys, size_t *cPreservedKeys)
{
  PreservedKeyMap pkm;
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) {
    return FALSE;
  }

  if (!_td->lpActiveKeyboard) {
    return FALSE;
  }

  if (!_td->lpActiveKeyboard->lpCoreKeyboard) {
    return FALSE;
  }
  // use api to get key rules
  return pkm.MapKeyboard(_td->lpActiveKeyboard->lpCoreKeyboard, pPreservedKeys, cPreservedKeys);
}
