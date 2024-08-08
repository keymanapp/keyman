/*
 * Keyman is copyright (C) 2004 - 2024 SIL International. MIT License.
 *
 * Mnemonic layout support for Linux
 */


#include <vector>
#include <string>
#include <stdio.h>
#include "mc_kmxfile.h"
#include "keymap.h"

const int KMX_ShiftStateMap[] = {
    ISVIRTUALKEY,
    ISVIRTUALKEY | K_SHIFTFLAG,
    ISVIRTUALKEY | K_CTRLFLAG,
    ISVIRTUALKEY | K_SHIFTFLAG | K_CTRLFLAG,
    0,
    0,
    ISVIRTUALKEY | RALTFLAG,
    ISVIRTUALKEY | RALTFLAG | K_SHIFTFLAG,
    0,
    0};

/**
 * @brief Constructor
 * @param deadCharacter a deadkey
*/
DeadKey::DeadKey(KMX_WCHAR deadCharacter) {
  this->m_deadchar = deadCharacter;
}

/**
 * @brief  return dead character
 * @return deadkey character
*/
KMX_WCHAR DeadKey::KMX_DeadCharacter() {
  return this->m_deadchar;
}

/**
 * @brief  set Deadkey with values
 * @param baseCharacter     the base character
 * @param  combinedCharacter the combined character
*/
void DeadKey::KMX_AddDeadKeyRow(KMX_WCHAR baseCharacter, KMX_WCHAR combinedCharacter) {
  this->m_rgbasechar.push_back(baseCharacter);
  this->m_rgcombchar.push_back(combinedCharacter);
}

/**
 * @brief  check if character exists in DeadKey
 * @param  baseCharacter a character to be found
 * @return true if found;
 *         false if not found
*/
bool DeadKey::KMX_ContainsBaseCharacter(KMX_WCHAR baseCharacter) {
  std::vector<KMX_WCHAR>::iterator it;
  for (it = this->m_rgbasechar.begin(); it < m_rgbasechar.end(); it++) {
    if (*it == baseCharacter) {
      return true;
    }
  }
  return false;
}

/**
 * @brief  Find a keyvalue for given keycode, shiftstate and caps. A function similar to Window`s ToUnicodeEx() function.
 *
 * Contrary to what the function name might suggest, the function KMX_ToUnicodeEx does not process surrogate pairs.
 * This is because it is used in mcompile only which only deals with latin scripts.
 * In case this function should be used for surrogate pairs, they will be ignored and a message will be printed out
 *
 * @param  keycode  a key of the currently used keyboard Layout
 * @param  pwszBuff Buffer to store resulting character
 * @param  ss       a shiftstate of the currently used keyboard Layout
 * @param  caps     state of the caps key of the currently used keyboard Layout
 * @param  keymap   the currently used (underlying)keyboard Layout
 * @return -1 if a deadkey was found;
 *          0 if no translation is available;
 *          +1 if character was found and written to pwszBuff
*/
int KMX_ToUnicodeEx(guint keycode, PKMX_WCHAR pwszBuff, ShiftState rgkey_ss, int caps, GdkKeymap* keymap) {

  GdkKeymapKey* maps;
  guint* keyvals;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;

  if (!(ensureValidInputForKeyboardTranslation(convert_rgkey_Shiftstate_to_LinuxShiftstate(rgkey_ss), keycode))){
    g_free(keyvals);
    g_free(maps);
    return 0;
  }

  KMX_DWORD keyVal = (KMX_DWORD)KMX_get_KeyVal_From_KeyCode(keymap, keycode, rgkey_ss, caps);
  std::u16string str = convert_DeadkeyValues_To_U16str(keyVal);
  KMX_WCHAR firstchar = *(PKMX_WCHAR)str.c_str();

  if ((firstchar >= 0xD800) &&(firstchar <= 0xDFFF)) {
    wprintf(L"Surrogate pair found that is not processed in KMX_ToUnicodeEx\n");
    return 0;
  }

  pwszBuff[0] = firstchar;

  g_free(keyvals);
  g_free(maps);

  if (u16len(pwszBuff) < 1)
    return 0;

  if ((keyVal >= deadkey_min) && (keyVal <= deadkey_max))   // deadkeys
    return -1;
  else if (gdk_keyval_to_unicode(keyVal) == 0)              // NO UNICODE
    return 0;
  else                                                      // usable char
    return 1;
}

KMX_WCHAR KMX_DeadKeyMap(int index, std::vector<DeadKey*>* deadkeys, int deadkeyBase, std::vector<KMX_DeadkeyMapping>* deadkeyMappings) {  // I4327   // I4353
  for (size_t i = 0; i < deadkeyMappings->size(); i++) {
    if ((*deadkeyMappings)[i].deadkey == index) {
      return (*deadkeyMappings)[i].dkid;
    }
  }

  for (size_t i = 0; i < deadkeys->size(); i++) {
    if ((*deadkeys)[i]->KMX_DeadCharacter() == index) {
      return (KMX_WCHAR)(deadkeyBase + i);
    }
  }
  return 0xFFFF;
}

/**
 * @brief  Base class for dealing with rgkey
*/
class KMX_VirtualKey {
private:
  UINT m_vk;
  UINT m_sc;
  bool m_rgfDeadKey[10][2];
  std::u16string m_rgss[10][2];

public:
  KMX_VirtualKey(UINT scanCode) {
    this->m_vk = KMX_get_VKUS_From_KeyCodeUnderlying(scanCode);
    this->m_sc = scanCode;
    memset(this->m_rgfDeadKey, 0, sizeof(this->m_rgfDeadKey));
  }

/**
 * @brief  return member variable virtual key
*/
  UINT VK() {
    return this->m_vk;
  }

/**
 * @brief  return member variable scancode
*/
  UINT SC() {
    return this->m_sc;
  }

  std::u16string KMX_GetShiftState(ShiftState shiftState, bool capsLock) {
    return this->m_rgss[(UINT)shiftState][(capsLock ? 1 : 0)];
  }

  void KMX_SetShiftState(ShiftState shiftState, std::u16string value, bool isDeadKey, bool capsLock) {
    this->m_rgfDeadKey[(UINT)shiftState][(capsLock ? 1 : 0)] = isDeadKey;
    this->m_rgss[(UINT)shiftState][(capsLock ? 1 : 0)] = value;
  }

  bool KMX_IsSGCAPS() {
    std::u16string stBase = this->KMX_GetShiftState(Base, false);
    std::u16string stShift = this->KMX_GetShiftState(Shft, false);
    std::u16string stCaps = this->KMX_GetShiftState(Base, true);
    std::u16string stShiftCaps = this->KMX_GetShiftState(Shft, true);
    return (
        ((stCaps.size() > 0) &&
        (stBase.compare(stCaps) != 0) &&
        (stShift.compare(stCaps) != 0)) ||
        ((stShiftCaps.size() > 0) &&
        (stBase.compare(stShiftCaps) != 0) &&
        (stShift.compare(stShiftCaps) != 0)));
  }

  bool KMX_IsCapsEqualToShift() {
    std::u16string stBase = this->KMX_GetShiftState(Base, false);
    std::u16string stShift = this->KMX_GetShiftState(Shft, false);
    std::u16string stCaps = this->KMX_GetShiftState(Base, true);
    return (
        (stBase.size() > 0) &&
        (stShift.size() > 0) &&
        (stBase.compare(stShift) != 0) &&
        (stShift.compare(stCaps) == 0));
  }

  bool KMX_IsAltGrCapsEqualToAltGrShift() {
    std::u16string stBase  = this->KMX_GetShiftState(MenuCtrl, false);
    std::u16string stShift = this->KMX_GetShiftState(ShftMenuCtrl, false);
    std::u16string stCaps  = this->KMX_GetShiftState(MenuCtrl, true);
    return (
        (stBase.size() > 0) &&
        (stShift.size() > 0) &&
        (stBase.compare(stShift) != 0) &&
        (stShift.compare(stCaps) == 0));
  }

  bool KMX_IsXxxxGrCapsEqualToXxxxShift() {
    std::u16string stBase = this->KMX_GetShiftState(Xxxx, false);
    std::u16string stShift = this->KMX_GetShiftState(ShftXxxx, false);
    std::u16string stCaps = this->KMX_GetShiftState(Xxxx, true);
    return (
        (stBase.size() > 0) &&
        (stShift.size() > 0) &&
        (stBase.compare(stShift) != 0) &&
        (stShift.compare(stCaps) == 0));
    }

  bool KMX_IsEmpty() {
    for (int i = 0; i < 10; i++) {
      for (int j = 0; j <= 1; j++) {
        if (this->KMX_GetShiftState((ShiftState)i, (j == 1)).size() > 0) {
          return (false);
        }
      }
    }
    return true;
  }
/**
 * @brief  check if we use only keys used in mcompile
*/
  bool KMX_IsKeymanUsedKey() {
    return (this->m_vk >= 0x20 && this->m_vk <= 0x5F) || (this->m_vk >= 0x88);
  }

  UINT KMX_GetShiftStateValue(int capslock, int caps, ShiftState ss) {
    return KMX_ShiftStateMap[(int)ss] | (capslock ? (caps ? CAPITALFLAG : NOTCAPITALFLAG) : 0);
  }

/**
 * @brief  count the number of keys
*/
  int KMX_GetKeyCount(int MaxShiftState) {
    int nkeys = 0;

    // Get the CAPSLOCK value
    for (int ss = 0; ss <= MaxShiftState; ss++) {
      if (ss == Menu || ss == ShftMenu) {
        // Alt and Shift+Alt don't work, so skip them
        continue;
      }
      for (int caps = 0; caps <= 1; caps++) {
        std::u16string st = this->KMX_GetShiftState((ShiftState)ss, (caps == 1));
        // ctrl and shift+ctrl will be skipped since rgkey has no entries in m_rgss[2] m_rgss[3]
        if (st.size() == 0) {
          // No character assigned here
        } else if (this->m_rgfDeadKey[(int)ss][caps]) {
          // It's a dead key, append an @ sign.
          nkeys++;
        } else {
          bool isvalid = true;
          for (size_t ich = 0; ich < st.size(); ich++) {
            if (st[ich] < 0x20 || st[ich] == 0x7F) {
              isvalid = false;
              printf("invalid for: %i\n", st[ich]);
              break;
            }
          }
          if (isvalid) {
            nkeys++;
          }
        }
      }
    }
    return nkeys;
  }

  bool KMX_LayoutRow(int MaxShiftState, LPKMX_KEY key, std::vector<DeadKey*>* deadkeys, int deadkeyBase, BOOL bDeadkeyConversion, vec_dword_3D& all_vector, GdkKeymap* keymap) {  // I4552
    // Get the CAPSLOCK value
    /*int capslock =
          (this->KMX_IsCapsEqualToShift() ? 1 : 0) |
          (this->KMX_IsSGCAPS() ? 2 : 0) |
          (this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
          (this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);*/

    int capslock = 1;  // we do not use the equation to obtain capslock. On Linux we set capslock = 1

    for (int ss = 0; ss <= MaxShiftState; ss++) {
      if (ss == Menu || ss == ShftMenu) {
        // Alt and Shift+Alt don't work, so skip them
        continue;
      }
      for (int caps = 0; caps <= 1; caps++) {
        std::u16string st = this->KMX_GetShiftState((ShiftState)ss, (caps == 1));

        PKMX_WCHAR p;

        if (st.size() == 0) {
          // No character assigned here
        } else if (this->m_rgfDeadKey[(int)ss][caps]) {
          // It's a dead key, append an @ sign.
          key->dpContext = new KMX_WCHAR[1];
          *key->dpContext = 0;

          key->ShiftFlags = this->KMX_GetShiftStateValue(capslock, caps, (ShiftState)ss);
          // we already use VK_US so no need to convert it as we do on Windows
          key->Key = this->VK();
          key->Line = 0;

          if (bDeadkeyConversion) {  // I4552
            p = key->dpOutput = new KMX_WCHAR[2];
            *p++ = st[0];
            *p = 0;
          } else {
            p = key->dpOutput = new KMX_WCHAR[4];
            *p++ = UC_SENTINEL;
            *p++ = CODE_DEADKEY;
            *p++ = KMX_DeadKeyMap(st[0], deadkeys, deadkeyBase, &KMX_FDeadkeys);  // I4353
            *p = 0;
          }
          key++;
        } else {
          bool isvalid = true;
          for (size_t ich = 0; ich < st.size(); ich++) {
            if (st[ich] < 0x20 || st[ich] == 0x7F) {
              isvalid = false;
              printf("invalid 16 for: %i\n", st[ich]);
              break;
            }
          }
          if (isvalid) {
           /*
             * this is different to mcompile Windows !!!!
             * this->m_sc    stores SC-US = SCUnderlying
             * this->m_vk    stores VK-US ( not VK underlying !!)
             * key->Key      stores VK-US ( not VK underlying !!)
             * key->dpOutput stores character Underlying
             */
            KMX_DWORD sc_underlying = KMX_get_KeyCodeUnderlying_From_KeyCodeUS(keymap, all_vector, this->SC(), (ShiftState)ss, caps);

            key->Key = KMX_get_VKUS_From_KeyCodeUnderlying(sc_underlying);

            key->Line = 0;
            key->ShiftFlags = this->KMX_GetShiftStateValue(capslock, caps, (ShiftState)ss);

            key->dpContext = new KMX_WCHAR;
            *key->dpContext = 0;
            p = key->dpOutput = new KMX_WCHAR[st.size() + 1];
            for (size_t ich = 0; ich < st.size(); ich++) {
              *p++ = st[ich];
            }
            *p = 0;
            key++;
          }
        }
      }
    }
    return true;
  }
};

/**
 * @brief  Base class for KMX_loader
 */
class KMX_Loader {
private:
  KMX_BYTE lpKeyStateNull[256];
  UINT m_XxxxVk;

public:
  KMX_Loader() {
    m_XxxxVk = 0;
    memset(lpKeyStateNull, 0, sizeof(lpKeyStateNull));
  }

  UINT Get_XxxxVk() {
    return m_XxxxVk;
  }

  void Set_XxxxVk(UINT value) {
    m_XxxxVk = value;
  }

  ShiftState KMX_MaxShiftState() {
    return (Get_XxxxVk() == 0 ? ShftMenuCtrl : ShftXxxx);
  }

  bool KMX_IsControlChar(char16_t ch) {
    return (ch < 0x0020) || (ch >= 0x007F && ch <= 0x009F);
  }
};

/**
 * @brief  find the maximum index of a deadkey
   @param  p pointer to deadkey
 * @return index of deadkey
*/
int KMX_GetMaxDeadkeyIndex(KMX_WCHAR* p) {
  int n = 0;
  while (p && *p) {
    if (*p == UC_SENTINEL && *(p + 1) == CODE_DEADKEY)
      n = std::max(n, (int)*(p + 2));
    p = KMX_incxstr(p);
  }
  return n;
}

/**
 * @brief  Collect the key data, translate it to kmx and append to the existing keyboard
 *         It is important to understand that this function has different sorting order in rgkey compared to mcompile-windows!
 *         On Windows the values of rgkey are sorted according to the VK of the underlying keyboard
 *         On Linux   the values of rgkey are sorted according to the VK of the the US keyboard
 *         Since Linux Keyboards do not use a VK mcompile uses the VK of the the US keyboard because
 *         these are available in mcompile through USVirtualKeyToScanCode/ScanCodeToUSVirtualKey and an offset of 8
 * @param  kp                 pointer to keyboard
 * @param  all_vector         vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param  keymap             the currently used (underlying)keyboard Layout
 * @param  FDeadkeys          vector of all deadkeys for the currently used (underlying)keyboard Layout
 * @param  bDeadkeyConversion 1 to convert a deadkey to a character; 0 no conversion
 * @return true in case of success
 */
bool KMX_ImportRules(LPKMX_KEYBOARD kp, vec_dword_3D& all_vector, GdkKeymap** keymap, std::vector<KMX_DeadkeyMapping>* FDeadkeys, KMX_BOOL bDeadkeyConversion) {  // I4353   // I4552
  KMX_Loader loader;

  std::vector<KMX_VirtualKey*> rgKey;  //= new VirtualKey[256];
  std::vector<DeadKey*> alDead;
  std::vector<DeadKey*> alDead_byBasechar = create_deadkeys_by_basechar();

  rgKey.resize(256);

  // Scroll through the Scan Code (SC) values and get the valid Virtual Key (VK)
  // values in it. Then, store the SC in each valid VK so it can act as both a
  // flag that the VK is valid, and it can store the SC value.

  // Windows and Linux Keycodes start with 1; Mac keycodes start with 0
  for (UINT sc = 0x01; sc <= 0x7f; sc++) {
    /* HERE IS A BIG DIFFERENCE COMPARED TO MCOMPILE FOR WINDOWS:
    * mcompile on Windows fills rgkey.m_vk with the VK of the Underlying keyboard
    * mcompile for Linux  fills rgkey.m_vk with the VK of the US keyboard
    * this results in a different sorting order in rgkey[] !

    * Linux cannot get a VK for the underling Keyboard since this does not exist
    * Linux can only get a VK for the US Keyboard (by using USVirtualKeyToScanCode/ScanCodeToUSVirtualKey)
    * therefore we use VK_US in rgkey[ ] which we get from all_vector
    */
    KMX_VirtualKey* key = new KMX_VirtualKey(sc);

    if ((key->VK() != 0)) {
      rgKey[key->VK()] = key;
    } else {
      delete key;
    }
  }

  // in this part we skip shiftstates 4, 5, 8, 9
  for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if (rgKey[iKey] != NULL) {
      KMX_WCHAR sbBuffer[256];  // Scratchpad we use many places
      for (ShiftState ss = Base; ss <= loader.KMX_MaxShiftState(); ss = (ShiftState)((int)ss + 1)) {
        if (ss == Menu || ss == ShftMenu) {
          // Alt and Shift+Alt don't work, so skip them (ss 4+5)
          continue;
        }

        KMX_DWORD kc_underlying = KMX_get_KeyCodeUnderlying_From_VKUS(iKey);

        for (int caps = 0; caps <= 1; caps++) {
          int rc = KMX_ToUnicodeEx(kc_underlying, sbBuffer, ss, caps, *keymap);

          if (rc > 0) {
            if (*sbBuffer == 0) {
              rgKey[iKey]->KMX_SetShiftState(ss, u"", false, (caps));  // different to windows since behavior on Linux is different (see above)
            } else {
            if ((ss == Ctrl || ss == ShftCtrl)) {
                continue;
              }
              sbBuffer[rc] = 0;
              rgKey[iKey]->KMX_SetShiftState(ss, sbBuffer, false, (caps));  // different to windows since behavior on Linux is different (see above)
          }
          } else if (rc < 0) {
            sbBuffer[2] = 0;
            rgKey[iKey]->KMX_SetShiftState(ss, sbBuffer, true, (caps));  // different to windows since behavior on Linux is different (see above)

            refine_alDead(sbBuffer[0], alDead, alDead_byBasechar);
          }
        }
      }
    }
  }

  //-------------------------------------------------------------
  // Now that we've collected the key data, we need to
  // translate it to kmx and append to the existing keyboard
  //-------------------------------------------------------------

  int nDeadkey = 0;
  LPKMX_GROUP gp = new KMX_GROUP[kp->cxGroupArray + 4];  // leave space for old
  memcpy(gp, kp->dpGroupArray, sizeof(KMX_GROUP) * kp->cxGroupArray);

  //
  // Find the current highest deadkey index
  //

  kp->dpGroupArray = gp;
  for (UINT i = 0; i < kp->cxGroupArray; i++, gp++) {
    LPKMX_KEY kkp = gp->dpKeyArray;

    for (UINT j = 0; j < gp->cxKeyArray; j++, kkp++) {
      nDeadkey = std::max(nDeadkey, KMX_GetMaxDeadkeyIndex(kkp->dpContext));
      nDeadkey = std::max(nDeadkey, KMX_GetMaxDeadkeyIndex(kkp->dpOutput));
    }
  }

  kp->cxGroupArray++;
  gp = &kp->dpGroupArray[kp->cxGroupArray - 1];

  // calculate the required size of `gp->dpKeyArray`

  UINT nkeys = 0;
  for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if ((rgKey[iKey] != NULL) && rgKey[iKey]->KMX_IsKeymanUsedKey() && (!rgKey[iKey]->KMX_IsEmpty())) {
      nkeys += rgKey[iKey]->KMX_GetKeyCount(loader.KMX_MaxShiftState());
    }
  }

  gp->fUsingKeys = TRUE;
  gp->dpMatch = NULL;
  gp->dpName = NULL;
  gp->dpNoMatch = NULL;
  gp->cxKeyArray = nkeys;
  gp->dpKeyArray = new KMX_KEY[gp->cxKeyArray];

  nDeadkey++;  // ensure a 1-based index above the max deadkey value already in the keyboard

  //
  // Fill in the new rules
  //
  nkeys = 0;
  for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if ((rgKey[iKey] != NULL) && rgKey[iKey]->KMX_IsKeymanUsedKey() && (!rgKey[iKey]->KMX_IsEmpty())) {
      if (rgKey[iKey]->KMX_LayoutRow(loader.KMX_MaxShiftState(), &gp->dpKeyArray[nkeys], &alDead, nDeadkey, bDeadkeyConversion, all_vector, *keymap)) {  // I4552
        nkeys += rgKey[iKey]->KMX_GetKeyCount(loader.KMX_MaxShiftState());
      }
    }
  }

  gp->cxKeyArray = nkeys;

  //
  // Add nomatch control to each terminating 'using keys' group   // I4550
  //
  LPKMX_GROUP gp2 = kp->dpGroupArray;
  for (UINT i = 0; i < kp->cxGroupArray - 1; i++, gp2++) {
    if (gp2->fUsingKeys && gp2->dpNoMatch == NULL) {
      KMX_WCHAR* p = gp2->dpNoMatch = new KMX_WCHAR[4];
      *p++ = UC_SENTINEL;
      *p++ = CODE_USE;
      *p++ = (KMX_WCHAR)(kp->cxGroupArray);
      *p = 0;

      // I4550 - Each place we have a nomatch > use(baselayout) (this last group), we need to add all
      // the AltGr and ShiftAltGr combinations as rules to allow them to be matched as well.  Yes, this
      // loop is not very efficient but it's not worthy of optimisation.
      //
      UINT j;
      LPKMX_KEY kkp;
      for (j = 0, kkp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kkp++) {
        if ((kkp->ShiftFlags & (K_CTRLFLAG | K_ALTFLAG | LCTRLFLAG | LALTFLAG | RCTRLFLAG | RALTFLAG)) != 0) {
          gp2->cxKeyArray++;
          LPKMX_KEY kkp2 = new KMX_KEY[gp2->cxKeyArray];
          memcpy(kkp2, gp2->dpKeyArray, sizeof(KMX_KEY) * (gp2->cxKeyArray - 1));
          gp2->dpKeyArray = kkp2;
          kkp2 = &kkp2[gp2->cxKeyArray - 1];
          kkp2->dpContext = new KMX_WCHAR;
          *kkp2->dpContext = 0;
          kkp2->Key = kkp->Key;
          kkp2->ShiftFlags = kkp->ShiftFlags;
          kkp2->Line = 0;
          KMX_WCHAR* p = kkp2->dpOutput = new KMX_WCHAR[4];
          *p++ = UC_SENTINEL;
          *p++ = CODE_USE;
          *p++ = (KMX_WCHAR)(kp->cxGroupArray);
          *p = 0;
        }
      }
    }
  }

  // If we have deadkeys, then add a new group to translate the deadkeys per the deadkey tables
  // We only do this if not in deadkey conversion mode
  //

  if (alDead.size() > 0 && !bDeadkeyConversion) {  // I4552
    kp->cxGroupArray++;

    KMX_WCHAR* p = gp->dpMatch = new KMX_WCHAR[4];
    *p++ = UC_SENTINEL;
    *p++ = CODE_USE;
    *p++ = (KMX_WCHAR)kp->cxGroupArray;
    *p = 0;

    gp++;

    gp->fUsingKeys = FALSE;
    gp->dpMatch = NULL;
    gp->dpName = NULL;
    gp->dpNoMatch = NULL;
    gp->cxKeyArray = alDead.size();
    LPKMX_KEY kkp = gp->dpKeyArray = new KMX_KEY[alDead.size()];

    LPKMX_STORE sp = new KMX_STORE[kp->cxStoreArray + alDead.size() * 2];
    memcpy(sp, kp->dpStoreArray, sizeof(KMX_STORE) * kp->cxStoreArray);

    kp->dpStoreArray = sp;

    sp = &sp[kp->cxStoreArray];
    int nStoreBase = kp->cxStoreArray;
    kp->cxStoreArray += alDead.size() * 2;

    for (UINT i = 0; i < alDead.size(); i++) {
      DeadKey* dk = alDead[i];

      sp->dpName = NULL;
      sp->dwSystemID = 0;
      sp->dpString = new KMX_WCHAR[dk->KMX_Count() + 1];
      for (int j = 0; j < dk->KMX_Count(); j++)
        sp->dpString[j] = dk->KMX_GetBaseCharacter(j);
      sp->dpString[dk->KMX_Count()] = 0;
      sp++;

      sp->dpName = NULL;
      sp->dwSystemID = 0;
      sp->dpString = new KMX_WCHAR[dk->KMX_Count() + 1];
      for (int j = 0; j < dk->KMX_Count(); j++)
        sp->dpString[j] = dk->KMX_GetCombinedCharacter(j);
      sp->dpString[dk->KMX_Count()] = 0;
      sp++;

      kkp->Line = 0;
      kkp->ShiftFlags = 0;
      kkp->Key = 0;
      KMX_WCHAR* p = kkp->dpContext = new KMX_WCHAR[8];
      *p++ = UC_SENTINEL;
      *p++ = CODE_DEADKEY;
      *p++ = KMX_DeadKeyMap(dk->KMX_DeadCharacter(), &alDead, nDeadkey, FDeadkeys);  // I4353
      // *p++ = nDeadkey+i;
      *p++ = UC_SENTINEL;
      *p++ = CODE_ANY;
      *p++ = nStoreBase + i * 2 + 1;
      *p = 0;

      p = kkp->dpOutput = new KMX_WCHAR[5];
      *p++ = UC_SENTINEL;
      *p++ = CODE_INDEX;
      *p++ = nStoreBase + i * 2 + 2;
      *p++ = 2;
      *p = 0;
      kkp++;
    }
  }
return true;
}
