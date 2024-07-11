/*
 * Keyman is copyright (C) 2004 SIL International. MIT License.
 *
 * Mnemonic layout support for mac
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

/** @brief Constructor */
DeadKey::DeadKey(KMX_WCHAR deadCharacter) {
  this->m_deadchar = deadCharacter;
}

/** @brief  return dead character */
KMX_WCHAR DeadKey::KMX_DeadCharacter() {
  return this->m_deadchar;
}

/** @brief set Deadkey with values */
void DeadKey::KMX_AddDeadKeyRow(KMX_WCHAR baseCharacter, KMX_WCHAR combinedCharacter) {
  this->m_rgbasechar.push_back(baseCharacter);
  this->m_rgcombchar.push_back(combinedCharacter);
}

/** @brief check if character exists in DeadKey */
bool DeadKey::KMX_ContainsBaseCharacter(KMX_WCHAR baseCharacter) {
  std::vector<KMX_WCHAR>::iterator it;
  for (it = this->m_rgbasechar.begin(); it < m_rgbasechar.end(); it++) {
    if (*it == baseCharacter) {
      return true;
    }
  }
  return false;
}
//_S2 remove!
bool test_alDead_S2(std::vector<DeadKey*> ald);


/** @brief Find a keyvalue for given keycode, shiftstate and caps. A function similar to Window`s ToUnicodeEx() function. */
int mac_KMX_ToUnicodeEx(int keycode, PKMX_WCHAR pwszBuff, ShiftState ss_rgkey, int caps, const UCKeyboardLayout* keyboard_layout) {
  KMX_DWORD keyval;
  UInt32 isdk = 0;

  if (!ensureValidInputForKeyboardTranslation(mac_convert_rgkey_Shiftstate_to_MacShiftstate(ss_rgkey),keycode))
    return 0;

  keyval = mac_KMX_get_KeyVal_From_KeyCode_dk(keyboard_layout, keycode, mac_convert_rgkey_Shiftstate_to_MacShiftstate(ss_rgkey), caps, isdk);
  std::u16string str =std::u16string(1, keyval );
  KMX_WCHAR firstchar = *(PKMX_WCHAR)str.c_str();

  if ((firstchar >= 0xD800) && (firstchar <= 0xDFFF)) {
    wprintf(L"Surrogate pair found that is not processed in KMX_ToUnicodeEx\n");
    return 0;
  }

  pwszBuff[0] = firstchar;

  if (u16len(pwszBuff) < 1)
    return 0;

  if ((isdk) && (keycode != 0xFFFF))    // deadkeys
    return -1;
  if (keyval == 0)                      // no character
    return 0;
  else                                  // usable char
    return 1;
}

KMX_WCHAR mac_KMX_DeadKeyMap(int index, std::vector<DeadKey*>*deadkeys, int deadkeyBase, std::vector<KMX_DeadkeyMapping>*deadkeyMappings) {   // I4327   // I4353
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

/** @brief  Base class for dealing with rgkey*/
class mac_KMX_VirtualKey {
private:
  UINT m_vk;
  UINT m_sc;
  bool m_rgfDeadKey[10][2];
  std::u16string m_rgss[10][2];

public:
  mac_KMX_VirtualKey(UINT scanCode) {
    this->m_vk = mac_KMX_get_VKUS_From_KeyCodeUnderlying(scanCode);
    this->m_sc = scanCode;
    memset(this->m_rgfDeadKey, 0, sizeof(this->m_rgfDeadKey));
  }

/** @brief return member variable virtual key */
  UINT VK() {
    return this->m_vk;
  }

/** @brief return member variable scancode */
  UINT SC() {
    return this->m_sc;
  }

  std::u16string mac_KMX_GetShiftState(ShiftState shiftState, bool capsLock) {
    return this->m_rgss[(UINT)shiftState][(capsLock ? 1 : 0)];
  }

  void mac_KMX_SetShiftState(ShiftState shiftState, std::u16string value, bool isDeadKey, bool capsLock) {
    this->m_rgfDeadKey[(UINT)shiftState][(capsLock ? 1 : 0)] = isDeadKey;
    this->m_rgss[(UINT)shiftState][(capsLock ? 1 : 0)] = value;
  }

  bool mac_KMX_IsSGCAPS() {
        std::u16string stBase = this->mac_KMX_GetShiftState(Base, false);
        std::u16string stShift = this->mac_KMX_GetShiftState(Shft, false);
        std::u16string stCaps = this->mac_KMX_GetShiftState(Base, true);
        std::u16string stShiftCaps = this->mac_KMX_GetShiftState(Shft, true);
        return (
            ((stCaps.size() > 0) &&
            (stBase.compare(stCaps) != 0) &&
            (stShift.compare(stCaps) != 0)) ||
            ((stShiftCaps.size() > 0) &&
            (stBase.compare(stShiftCaps) != 0) &&
            (stShift.compare(stShiftCaps) != 0)));
      }

  bool mac_KMX_IsCapsEqualToShift() {
        std::u16string stBase = this->mac_KMX_GetShiftState(Base, false);
        std::u16string stShift = this->mac_KMX_GetShiftState(Shft, false);
        std::u16string stCaps = this->mac_KMX_GetShiftState(Base, true);
        return (
            (stBase.size() > 0) &&
            (stShift.size() > 0) &&
            (stBase.compare(stShift) != 0) &&
            (stShift.compare(stCaps) == 0));
      }

  bool mac_KMX_IsAltGrCapsEqualToAltGrShift() {
      std::u16string stBase = this->mac_KMX_GetShiftState(MenuCtrl, false);
      std::u16string stShift = this->mac_KMX_GetShiftState(ShftMenuCtrl, false);
      std::u16string stCaps = this->mac_KMX_GetShiftState(MenuCtrl, true);
      return (
          (stBase.size() > 0) &&
          (stShift.size() > 0) &&
          (stBase.compare(stShift) != 0) &&
          (stShift.compare(stCaps) == 0));
    }

  bool mac_KMX_IsXxxxGrCapsEqualToXxxxShift() {
  std::u16string stBase = this->mac_KMX_GetShiftState(Xxxx, false);
  std::u16string stShift = this->mac_KMX_GetShiftState(ShftXxxx, false);
  std::u16string stCaps = this->mac_KMX_GetShiftState(Xxxx, true);
  return (
      (stBase.size() > 0) &&
      (stShift.size() > 0) &&
      (stBase.compare(stShift) != 0) &&
      (stShift.compare(stCaps) == 0));
  }

  bool mac_KMX_IsEmpty() {
    for (int i = 0; i < 10; i++) {
      for (int j = 0; j <= 1; j++) {
        if (this->mac_KMX_GetShiftState((ShiftState)i, (j == 1)).size() > 0) {
          return (false);
        }
      }
    }
    return true;
  }

/** @brief check if we use only keys used in mcompile */
  bool mac_KMX_IsKeymanUsedKey() {
    return (this->m_vk >= 0x20 && this->m_vk <= 0x5F) || (this->m_vk >= 0x88);
  }

  UINT KMX_GetShiftStateValue(int capslock, int caps, ShiftState ss) {
    return KMX_ShiftStateMap[(int)ss] | (capslock ? (caps ? CAPITALFLAG : NOTCAPITALFLAG) : 0);
  }

/** @brief count the number of keys */
  int mac_KMX_GetKeyCount(int MaxShiftState) {
    int nkeys = 0;

    // Get the CAPSLOCK value
    for (int ss = 0; ss <= MaxShiftState; ss++) {
      if (ss == Menu || ss == ShftMenu) {
        // Alt and Shift+Alt don't work, so skip them
        continue;
      }
      for (int caps = 0; caps <= 1; caps++) {
        std::u16string st = this->mac_KMX_GetShiftState((ShiftState)ss, (caps == 1));
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
              wprintf(L"invalid for: %i\n", st[ich]);
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

/** @brief edit the row of kmx-file */
  bool mac_KMX_LayoutRow(int MaxShiftState, LPKMX_KEY key, std::vector<DeadKey*>*deadkeys, int deadkeyBase, BOOL bDeadkeyConversion, vec_dword_3D& all_vector, const UCKeyboardLayout* keyboard_layout) {  // I4552
    // Get the CAPSLOCK value
    /*int capslock =
          (this->mac_KMX_IsCapsEqualToShift() ? 1 : 0) |
          (this->mac_KMX_IsSGCAPS() ? 2 : 0) |
          (this->mac_KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
          (this->mac_KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);*/

    int capslock = 1;  // we do not use the equation to obtain capslock. on the mac we set capslock=1

    for (int ss = 0; ss <= MaxShiftState; ss++) {
      if (ss == Menu || ss == ShftMenu) {
        // Alt and Shift+Alt don't work, so skip them
        continue;
      }
      for (int caps = 0; caps <= 1; caps++) {
        std::u16string st = this->mac_KMX_GetShiftState((ShiftState)ss, (caps == 1));

        PKMX_WCHAR p;

        if (st.size() == 0) {
          // No character assigned here
        } else if (this->m_rgfDeadKey[(int)ss][caps]) {
          // It's a dead key, append an @ sign.
          key->dpContext = new KMX_WCHAR[1];
          *key->dpContext = 0;

          key->ShiftFlags = this->KMX_GetShiftStateValue(capslock, caps, (ShiftState)ss);
          // we already use VK_US so no need to convert it as we do on windows
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
            *p++ = mac_KMX_DeadKeyMap(st[0], deadkeys, deadkeyBase, &KMX_FDeadkeys);  // I4353
            *p = 0;
          }
          key++;
        } else {
          bool isvalid = true;
          for (size_t ich = 0; ich < st.size(); ich++) {
            if (st[ich] < 0x20 || st[ich] == 0x7F) {
              isvalid = false;
              wprintf(L"invalid 16 for: %i\n", st[ich]);
              break; 
              }
          }
          if (isvalid) {
            /*
             * this is different to mcompile windows !!!!
             * this->m_sc    stores SC-US = SCUnderlying
             * this->m_vk    stores VK-US ( not underlying !!)
             * key->Key      stores VK-US ( not underlying !!)
             * key->dpOutput stores character Underlying
             */
            KMX_DWORD sc_underlying = mac_KMX_get_KeyCodeUnderlying_From_KeyCodeUS(keyboard_layout, all_vector, this->SC(), (ShiftState)ss, caps);

            key->Key = mac_KMX_get_VKUS_From_KeyCodeUnderlying(sc_underlying);

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

/** @brief  Base class for KMX_loader*/
class mac_KMX_Loader {
private:
  KMX_BYTE lpKeyStateNull[256];
  UINT m_XxxxVk;

public:
  mac_KMX_Loader() {
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

  DeadKey* ProcessDeadKey(
    UINT iKeyDead,                              // The index into the VirtualKey of the dead key
    ShiftState shiftStateDead,                  // The shiftstate that contains the dead key
    std::vector<mac_KMX_VirtualKey*> rgKey,     // Our array of dead keys
    bool fCapsLock,                             // Was the caps lock key pressed?
    const UCKeyboardLayout* keyboard_layout) {  // The keyboard layout

    int max_shiftstate_pos = 1;  // use BASE + SHIFT only
    DeadKey* deadKey = new DeadKey(rgKey[iKeyDead]->mac_KMX_GetShiftState(shiftStateDead, fCapsLock)[0]);

    int ss_dead = mac_convert_rgkey_Shiftstate_to_MacShiftstate(shiftStateDead);
    KMX_DWORD keyval_underlying_dk  = mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout, mac_USVirtualKeyToScanCode[iKeyDead], ss_dead, 0);

    for (int i = 0; i < keycode_spacebar + 1; i++) {
      for (int j = 0; j <= max_shiftstate_pos; j++) {
        for (int caps = 0; caps < 1; caps++) {
          // e.g.   a   basechar
          KMX_DWORD basechar = mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout, i, ss_mac[j], caps);

          // e.g.   â   combchar
          KMX_DWORD kc_Underlying_dk = mac_KMX_get_KeyCodeUnderlying_From_VKUS(iKeyDead);
          KMX_DWORD combchar = mac_get_CombinedChar_From_DK(keyboard_layout, kc_Underlying_dk, ss_dead, i, ss_mac[j], caps);

          if (combchar == 0)
            continue;

          // push only for if combchar is not dk or combchar is dk with space
          if ((!(combchar== keyval_underlying_dk)) || (basechar == mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout, keycode_spacebar, ss_mac[j], caps))) {
            deadKey->KMX_AddDeadKeyRow(basechar, combchar);
            //printf( " processed basechar %i(%c) with  dk %i(%c) ===> combchar%i(%c)  \t\t ss[%i](%i)  caps%i  \n", basechar,basechar ,keyval_underlying_dk,keyval_underlying_dk, combchar,combchar, j, ss[j], caps );
          }
        }
      }
    }
   return deadKey;
  }
};

/** 
 * @brief find the maximum index of a deadkey
 * @param p pointer to deadkey
 * @return index of deadkey
 */
int mac_KMX_GetMaxDeadkeyIndex(KMX_WCHAR* p) {
  int n = 0;
  while (p && *p) {
    if (*p == UC_SENTINEL && *(p + 1) == CODE_DEADKEY)
      n = std::max(n, (int)*(p + 2));
    p = KMX_incxstr(p);
  }
  return n;
}

//################################################################################################################################################
//################################# Code beyond these lines needs to be included in mcompile #####################################################
//################################################################################################################################################


bool test_run_verify_S2(std::vector<mac_KMX_VirtualKey*> rgKey);
// _S2 need to go
void test_print_All_Entries_S2(std::vector<mac_KMX_VirtualKey*> rgKey);
//void test_print_All_Keys_S2(const UCKeyboardLayout * keyboard_layout);
//void test_print_certain_Keys_S2(const UCKeyboardLayout * keyboard_layout, int keycode);

/**
 * @brief  Collect the key data, translate it to kmx and append to the existing keyboard
 *         It is important to understand that this function has different sorting order in rgkey compared to mcompile-windows!
 *         On windows the values of rgkey are sorted according to the VK of the underlying keyboard
 *         On Linux   the values of rgkey are sorted according to the VK of the the US keyboard
 *         Since Linux Keyboards do not use a VK mcompile uses the VK of the the US keyboard because
 *         these are available in mcompile through USVirtualKeyToScanCode/ScanCodeToUSVirtualKey and an offset of 8
 * @param  kp pointer to keyboard
 * @param  all_vector vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param  keyboard_layout the currently used (underlying)keyboard Layout
 * @param  FDeadkeys vector of all deadkeys for the currently used (underlying)keyboard Layout
 * @param  bDeadkeyConversion 1 to convert a deadkey to a character; 0 no conversion
 * @return true in case of success
 */
bool mac_KMX_ImportRules( LPKMX_KEYBOARD kp, vec_dword_3D& all_vector, const UCKeyboardLayout** keyboard_layout, std::vector<KMX_DeadkeyMapping>* FDeadkeys, KMX_BOOL bDeadkeyConversion) { // I4353   // I4327
 mac_KMX_Loader loader;

  std::vector<mac_KMX_VirtualKey*> rgKey; //= new VirtualKey[256];
  std::vector<DeadKey*> alDead;

  rgKey.resize(256);

  // Scroll through the Scan Code (SC) values and get the valid Virtual Key (VK)
  // values in it. Then, store the SC in each valid VK so it can act as both a
  // flag that the VK is valid, and it can store the SC value.

  //Windows and Linux Keycodes start with 1; Mac keycodes start with 0
  for (UINT sc = 0x00; sc <= 0x7f; sc++) {
    /* HERE IS A BIG DIFFERENCE COMPARED TO MCOMPILE FOR WINDOWS:
    * mcompile on Windows fills rgkey.m_vk with the VK of the Underlying keyboard
    * mcompile for macOS  fills rgkey.m_vk with the VK of the US keyboard
    * this results in a different sorting order in rgkey[] !

    * macOS cannot get a VK for the underling Keyboard since this does not exist
    * macOS can only get a VK for the US Keyboard (by using USVirtualKeyToScanCode/ScanCodeToUSVirtualKey)
    * therefore in rgkey[ ] we use VK_US which we get from all_vector
    */
    mac_KMX_VirtualKey* key = new mac_KMX_VirtualKey(sc);

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

         //_S2 TOP_6 TODO to compare win-lin kmn-files skip ss6+7; MUST BE removed later!!!!
         /*if (ss == MenuCtrl || ss == ShftMenuCtrl) {
           continue;
          }*/

          KMX_DWORD kc_underlying = mac_KMX_get_KeyCodeUnderlying_From_VKUS(iKey);

          for (int caps = 0; caps <= 1; caps++) {
            int rc = mac_KMX_ToUnicodeEx(kc_underlying, sbBuffer, ss, caps, *keyboard_layout);

            if (rc > 0) {
              if (*sbBuffer == 0) {
                rgKey[iKey]->mac_KMX_SetShiftState(ss, u"", false, (caps));      // different to windows since behavior on the mac is different
              } else {
              if ((ss == Ctrl || ss == ShftCtrl)) {
                  continue;
                }
                  sbBuffer[rc] = 0;
                  rgKey[iKey]->mac_KMX_SetShiftState(ss, sbBuffer, false, (caps));    // different to windows since behavior on the mac is different
             }
          } else if (rc < 0) {
            sbBuffer[2] = 0;
            rgKey[iKey]->mac_KMX_SetShiftState(ss, sbBuffer, true, (caps ));      // different to windows since behavior on the mac is different

            sbBuffer[2] = 0;
            rgKey[iKey]->mac_KMX_SetShiftState(ss, sbBuffer, true, (caps == 0));
            DeadKey* dk = NULL;
            for (UINT iDead = 0; iDead < alDead.size(); iDead++) {
                dk = alDead[iDead];
                if (dk->KMX_DeadCharacter() == rgKey[iKey]->mac_KMX_GetShiftState(ss, caps == 0)[0]) {
                    break;
                }
                dk = NULL;
            }
            if (dk == NULL) {
              alDead.push_back(loader.ProcessDeadKey(iKey, ss, rgKey, caps == 0, *keyboard_layout));
            }
          }
        }
      }
    }
  }

  //-------------------------------------------------------------
  // Now that we've collected the key data, we need to
  // translate it to kmx and append to the existing keyboard
  //-------------------------------------------------------------

  // _S2 all of my tests need to go ............
  //test_print_All_Entries_S2(rgKey);
  //test_print_All_Keys_S2( * keyboard_layout);
  //test_print_certain_Keys_S2( * keyboard_layout, 14);
 //bool allOK =test_alDead_S2(alDead);
 bool allOK=true;
  if ( ! allOK )
    printf(" \n::::::::::::::::::::::::::::::::::::::::::::::::::: THERE ARE SOME WRONG DEADKEYS :::::::::::::::::::::::::::::::::::::::::::::::::: \n");
 if ( ! test_run_verify_S2(rgKey))
    printf(" \n::::::::::::::::::::::::::::::::::::::::::::::::::: THERE ARE SOME WRONG ENTRIES ::::::::::::::::::::::::::::::::::::::::::::::::::: \n");
  if (  test_run_verify_S2(rgKey) && allOK)
    printf("\n ::::::::::::::::::::::::::::::::::::::::::::::::::::::::: :) :) :) ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: \n");

  int nDeadkey = 0;
  LPKMX_GROUP gp = new KMX_GROUP[kp->cxGroupArray+4];  // leave space for old
  memcpy(gp, kp->dpGroupArray, sizeof(KMX_GROUP) * kp->cxGroupArray);

  //
  // Find the current highest deadkey index
  //

  kp->dpGroupArray = gp;
  for (UINT i = 0; i < kp->cxGroupArray; i++, gp++) {
    LPKMX_KEY kkp = gp->dpKeyArray;

    for (UINT j = 0; j < gp->cxKeyArray; j++, kkp++) {
      nDeadkey = std::max(nDeadkey, mac_KMX_GetMaxDeadkeyIndex(kkp->dpContext));
      nDeadkey = std::max(nDeadkey, mac_KMX_GetMaxDeadkeyIndex(kkp->dpOutput));
    }
  }

  kp->cxGroupArray++;
  gp = &kp->dpGroupArray[kp->cxGroupArray - 1];

  // calculate the required size of `gp->dpKeyArray`

  UINT nkeys = 0;
  for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if ((rgKey[iKey] != NULL) && rgKey[iKey]->mac_KMX_IsKeymanUsedKey() && (!rgKey[iKey]->mac_KMX_IsEmpty())) {
      nkeys += rgKey[iKey]->mac_KMX_GetKeyCount(loader.KMX_MaxShiftState());
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
    if ((rgKey[iKey] != NULL) && rgKey[iKey]->mac_KMX_IsKeymanUsedKey() && (!rgKey[iKey]->mac_KMX_IsEmpty())) {
      if (rgKey[iKey]->mac_KMX_LayoutRow(loader.KMX_MaxShiftState(), &gp->dpKeyArray[nkeys], &alDead, nDeadkey, bDeadkeyConversion, all_vector, *keyboard_layout)) {  // I4552
        nkeys += rgKey[iKey]->mac_KMX_GetKeyCount(loader.KMX_MaxShiftState());
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
      *p++ = mac_KMX_DeadKeyMap(dk->KMX_DeadCharacter(), &alDead, nDeadkey, FDeadkeys);  // I4353
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
// _S2 need to go

void print_entries_S2(int i, std::vector<mac_KMX_VirtualKey*> rgKey , std::string comp1,std::string comp2,std::string erg) {
  std::string b0 =string_from_u16string(rgKey[i]->mac_KMX_GetShiftState(Base,  0 ));
  std::string b1 =string_from_u16string(rgKey[i]->mac_KMX_GetShiftState(Base,  1 ));
  std::string s0 =string_from_u16string(rgKey[i]->mac_KMX_GetShiftState(Shft,  0 ));
  std::string s1 =string_from_u16string(rgKey[i]->mac_KMX_GetShiftState(Shft,  1 ));
  printf("\n entry nr %i has characters %-30s:(%s|%s|%s|%s)",i, erg.c_str(), (const char * ) b0.c_str(),(const char * ) b1.c_str(),(const char * ) s0.c_str(),(const char * ) s1.c_str());
}
bool verify_entries_S2(int i, std::vector<mac_KMX_VirtualKey*> rgKey, int res1,int res2,int res3,int res4 ,int res5=0, int res6=0, int res7=0, int res8=0 ) {
    std::vector <std::u16string> st_vec;
    std::vector <int> r_vec;
    bool all_OK=true;;

    r_vec.push_back(res1);         r_vec.push_back(res2);
    r_vec.push_back(res3);         r_vec.push_back(res4);
    r_vec.push_back(res5);         r_vec.push_back(res6);
    r_vec.push_back(res7);         r_vec.push_back(res8);

    st_vec.push_back(rgKey[i]->mac_KMX_GetShiftState(Base,0));
    st_vec.push_back(rgKey[i]->mac_KMX_GetShiftState(Base,1));
    st_vec.push_back(rgKey[i]->mac_KMX_GetShiftState(Shft,0));
    st_vec.push_back(rgKey[i]->mac_KMX_GetShiftState(Shft,1));
    st_vec.push_back(rgKey[i]->mac_KMX_GetShiftState(MenuCtrl,0));
    st_vec.push_back(rgKey[i]->mac_KMX_GetShiftState(MenuCtrl,1));
    st_vec.push_back(rgKey[i]->mac_KMX_GetShiftState(ShftMenuCtrl,0));
    st_vec.push_back(rgKey[i]->mac_KMX_GetShiftState(ShftMenuCtrl,1));

    for ( int k=0; k< st_vec.size();k++)
      if (r_vec[k] !=0 )   all_OK = (  *st_vec[k].c_str()  == r_vec[k]  ) && all_OK;

    if ( !all_OK)
      printf(" ERROR FOR i= Nr:%i, %i(%c) - %i (%c) - %i (%c) - %i (%c) \n",i, *st_vec[0].c_str(),*st_vec[1].c_str(),*st_vec[2].c_str(),*st_vec[3].c_str(),*st_vec[4].c_str(),*st_vec[5].c_str(),*st_vec[6].c_str(),*st_vec[7].c_str());
    return all_OK;
}
void test_print_All_Entries_S2( std::vector<mac_KMX_VirtualKey*> rgKey) {
  for ( int i=48; i<58; i++)
    print_entries_S2(i, rgKey," ","","");

  for ( int i=65; i<91; i++)
    print_entries_S2(i, rgKey, " ","", "" );

    print_entries_S2(186, rgKey, "ö", "Ö", "xc3 xb6");
    print_entries_S2(187, rgKey, "´", ";", "´ ´ ` `");
    print_entries_S2(188, rgKey, ".", ":", ", , ; ;");
    print_entries_S2(189, rgKey, "ß", "?", "ß ß ? ?");
    print_entries_S2(190, rgKey, ".", ":", ". . : :");
    print_entries_S2(191, rgKey, "-", "_", "- - _ _");
    print_entries_S2(192, rgKey, "^", "°", "^ ^ ° °");

    print_entries_S2(219, rgKey, "ü", "Ü", "ü,Ü;Ü;Ü");
    print_entries_S2(220, rgKey, "#", "'", "# # ' '");
    print_entries_S2(221, rgKey, "+", "*", "+ + * *");
    print_entries_S2(222, rgKey, "ä", "Ä", "ä Ä Ä Ä");
    print_entries_S2(226, rgKey, "<", ">", "< < > >");
}
/*void test_print_certain_Keys_S2(const UCKeyboardLayout * keyboard_layout, int keycode) {
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  UniChar unicodeString[maxStringlength];
  UInt32 deadkeystate = 0;
  OSStatus status;
 unicodeString[0]=0;
  //int shiftarray[] ={0,2,8,10};
  int shiftarray[] ={0,1,2,3,4,5,6,7,8,9,10};
  for (int k=0; k<2;k++) {
    for (int j=0; j<(sizeof(shiftarray)/ sizeof(int));j++) {
      status = UCKeyTranslate(keyboard_layout, keycode , kUCKeyActionDown, (shiftarray[j]+ 4*k), LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
       if (deadkeystate ==0)
        printf("    key nr %i , ss %i has values %i(%c)\n", keycode, shiftarray[j]+ 4*k, unicodeString[0], unicodeString[0] );
      // If this was a deadkey, append a space
      if (deadkeystate !=0) {
        status = UCKeyTranslate(keyboard_layout, keycode_spacebar , kUCKeyActionDown, (shiftarray[j]+ 4*k), LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
        printf(" dk with caps key nr %i , ss %i has values %i(%c)\n", keycode,shiftarray[j]+ 4*k, unicodeString[0], unicodeString[0] );
      }
    }
  }
}
*/
/*void test_print_All_Keys_S2(const UCKeyboardLayout * keyboard_layout){
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  UniChar unicodeString[maxStringlength];
  UInt32 deadkeystate = 0;
  OSStatus status;
 unicodeString[0]=0;
  int shiftarray[] ={0,2,8,10};

  for (int k=0; k<2;k++) {
    for (int j=0; j<4;j++) {
      for (int i=0; i< keycode_spacebar+1 ;i++) {
        status = UCKeyTranslate(keyboard_layout, i , kUCKeyActionDown, (shiftarray[j]+ 4*k), LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
        if (deadkeystate ==0)
          printf("    key nr %i , ss %i has values %i(%c)\n", i, shiftarray[j]+ 4*k, unicodeString[0], unicodeString[0] );
        // If this was a deadkey, append a space
        if (deadkeystate !=0) {
          status = UCKeyTranslate(keyboard_layout, keycode_spacebar , kUCKeyActionDown, (shiftarray[j]+ 4*k), LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
          printf(" dk key nr %i , ss %i has values %i(%c)\n", i,shiftarray[j]+ 4*k, unicodeString[0], unicodeString[0] );
        }
      }
    }
  }
}
*/
bool test_checkBasechar_S2 (DeadKey* DK, int index, KMX_WCHAR ch) {
   return ( DK->KMX_GetBaseCharacter(index) == ch) ;
}
bool test_checkCombchar_S2 (DeadKey* DK, int index, KMX_WCHAR ch) {
   return ( DK->KMX_GetCombinedCharacter(index) == ch) ;
}
bool test_alDead_S2(std::vector <DeadKey*> ald) {
  bool OK =true;

    OK = OK && test_checkBasechar_S2(ald[2],0,  0x61);
    OK = OK && test_checkBasechar_S2(ald[2],1,  0x41);
    OK = OK && test_checkBasechar_S2(ald[2],14, 0x65);
    OK = OK && test_checkBasechar_S2(ald[2],15, 0x45);
    OK = OK && test_checkBasechar_S2(ald[2],23 ,0x69 );
    OK = OK && test_checkBasechar_S2(ald[2],24 ,0x49 );
    OK = OK && test_checkBasechar_S2(ald[2],19 ,0x6f );
    OK = OK && test_checkBasechar_S2(ald[2],20 ,0x4f );
    OK = OK && test_checkBasechar_S2(ald[2],21 ,0x75 );
    OK = OK && test_checkBasechar_S2(ald[2],22 ,0x55 );
    OK = OK && test_checkBasechar_S2(ald[2],27 ,0x20 );
    OK = OK && test_checkBasechar_S2(ald[2],28 ,0x20 );

      OK = OK && test_checkCombchar_S2(ald[2],0,  0xe2);
      OK = OK && test_checkCombchar_S2(ald[2],1,  0xc2);
      OK = OK && test_checkCombchar_S2(ald[2],14 ,0xea);
      OK = OK && test_checkCombchar_S2(ald[2],15, 0xca);
      OK = OK && test_checkCombchar_S2(ald[2],23 ,0xee );
      OK = OK && test_checkCombchar_S2(ald[2],24 ,0xce );
      OK = OK && test_checkCombchar_S2(ald[2],19 ,0xf4 );
      OK = OK && test_checkCombchar_S2(ald[2],20 ,0xd4 );
      OK = OK && test_checkCombchar_S2(ald[2],21 ,0xfb );
      OK = OK && test_checkCombchar_S2(ald[2],22 ,0xdb );
      OK = OK && test_checkCombchar_S2(ald[2],27 ,0x5e );
      OK = OK && test_checkCombchar_S2(ald[2],28 ,0x5e );

    OK = OK && test_checkBasechar_S2(ald[0],0,  0x61);
    OK = OK && test_checkBasechar_S2(ald[0],1,  0x41);
    OK = OK && test_checkBasechar_S2(ald[0],12, 0x65);
    OK = OK && test_checkBasechar_S2(ald[0],13, 0x45);
    OK = OK && test_checkBasechar_S2(ald[0],24 ,0x69 );
    OK = OK && test_checkBasechar_S2(ald[0],25 ,0x49 );
    OK = OK && test_checkBasechar_S2(ald[0],18 ,0x6f );
    OK = OK && test_checkBasechar_S2(ald[0],19 ,0x4f );
    OK = OK && test_checkBasechar_S2(ald[0],20 ,0x75 );
    OK = OK && test_checkBasechar_S2(ald[0],21 ,0x55 );
    OK = OK && test_checkBasechar_S2(ald[0],36 ,0x20 );
    OK = OK && test_checkBasechar_S2(ald[0],37 ,0x20 );

      OK = OK && test_checkCombchar_S2(ald[0],0,  0xe2-1);
      OK = OK && test_checkCombchar_S2(ald[0],1,  0xc2-1);
      OK = OK && test_checkCombchar_S2(ald[0],12 ,0xea-1);
      OK = OK && test_checkCombchar_S2(ald[0],13, 0xca-1);
      OK = OK && test_checkCombchar_S2(ald[0],24 ,0xee -1);
      OK = OK && test_checkCombchar_S2(ald[0],25 ,0xce -1);
      OK = OK && test_checkCombchar_S2(ald[0],18 ,0xf4 -1);
      OK = OK && test_checkCombchar_S2(ald[0],19 ,0xd4 -1);
      OK = OK && test_checkCombchar_S2(ald[0],20 ,0xfb -1);
      OK = OK && test_checkCombchar_S2(ald[0],21 ,0xdb -1);
      OK = OK && test_checkCombchar_S2(ald[0],36 ,0xb4 );
      OK = OK && test_checkCombchar_S2(ald[0],37 ,0xb4 );

    OK = OK && test_checkBasechar_S2(ald[1],0,  0x61);
    OK = OK && test_checkBasechar_S2(ald[1],1,  0x41);
    OK = OK && test_checkBasechar_S2(ald[1],6, 0x65);
    OK = OK && test_checkBasechar_S2(ald[1],7, 0x45);
    OK = OK && test_checkBasechar_S2(ald[1],12 ,0x69 );
    OK = OK && test_checkBasechar_S2(ald[1],13 ,0x49 );
    OK = OK && test_checkBasechar_S2(ald[1],8 ,0x6f );
    OK = OK && test_checkBasechar_S2(ald[1],9 ,0x4f );
    OK = OK && test_checkBasechar_S2(ald[1],10 ,0x75 );
    OK = OK && test_checkBasechar_S2(ald[1],11 ,0x55 );
    OK = OK && test_checkBasechar_S2(ald[1],16 ,0x20 );
    OK = OK && test_checkBasechar_S2(ald[1],17 ,0x20 );

      OK = OK && test_checkCombchar_S2(ald[1],0,  0xe2-2);
      OK = OK && test_checkCombchar_S2(ald[1],1,  0xc2-2);
      OK = OK && test_checkCombchar_S2(ald[1],6 ,0xea-2);
      OK = OK && test_checkCombchar_S2(ald[1],7, 0xca-2);
      OK = OK && test_checkCombchar_S2(ald[1],12 ,0xee -2);
      OK = OK && test_checkCombchar_S2(ald[1],13 ,0xce -2);
      OK = OK && test_checkCombchar_S2(ald[1],8 ,0xf4 -2);
      OK = OK && test_checkCombchar_S2(ald[1],9 ,0xd4 -2);
      OK = OK && test_checkCombchar_S2(ald[1],10 ,0xfb -2);
      OK = OK && test_checkCombchar_S2(ald[1],11 ,0xdb -2);
      OK = OK && test_checkCombchar_S2(ald[1],16 ,0x60 );
      OK = OK && test_checkCombchar_S2(ald[1],17 ,0x60 );
return OK;
}
bool test_run_verify_S2(std::vector<mac_KMX_VirtualKey*> rgKey) {

  bool allOK= true;
  allOK =  verify_entries_S2(48, rgKey, 48,48,61,61)                           && allOK;
  allOK =  verify_entries_S2(48, rgKey, 48,48,61,61)                           && allOK;
  allOK =  verify_entries_S2(49, rgKey, 49,49,33,33)                           && allOK;
  allOK =  verify_entries_S2(50, rgKey, 50,50,34,34)                           && allOK;
  allOK =  verify_entries_S2(51, rgKey, 51,51,167,167)                         && allOK;
  allOK =  verify_entries_S2(52, rgKey, 52,52,36,36)                           && allOK;
  allOK =  verify_entries_S2(53, rgKey, 53,53,37,37)                           && allOK;
  allOK =  verify_entries_S2(54, rgKey, 54,54,38,38)                           && allOK;
  allOK =  verify_entries_S2(55, rgKey, 55,55,47,47)                           && allOK;
  allOK =  verify_entries_S2(56, rgKey, 56,56,40,40)                           && allOK;
  allOK =  verify_entries_S2(57, rgKey, 57,57,41,41)                           && allOK;

  for ( int i=65; i<89;i++)
    allOK =  verify_entries_S2(i, rgKey,i+32,i,i,i)                            && allOK;

  allOK =  verify_entries_S2(89, rgKey, 90+32,90,90,90)                        && allOK;
  allOK =  verify_entries_S2(90, rgKey, 89+32,89,89,89)                        && allOK;

  allOK =  verify_entries_S2(186, rgKey, 246,214,214,214)                      && allOK;
  allOK =  verify_entries_S2(187, rgKey, 180,180,96,96)                      && allOK;      // dk ´ `
  allOK =  verify_entries_S2(188, rgKey, 44,44,59,59)                          && allOK;
  allOK =  verify_entries_S2(189, rgKey, 223,223,63,63)                        && allOK;
  allOK =  verify_entries_S2(190, rgKey, 46,46,58,58)                          && allOK;
  allOK =  verify_entries_S2(191, rgKey, 45,45,95,95)                          && allOK;

  allOK =  verify_entries_S2(192, rgKey, 94,94,176,176)                      && allOK;      // dk ^ °
  allOK =  verify_entries_S2(219, rgKey, 252,220,220,220)                      && allOK;
  allOK =  verify_entries_S2(220, rgKey, 35,35,39,39)                          && allOK;
  allOK =  verify_entries_S2(221, rgKey, 43,43,42,42)                          && allOK;
  allOK =  verify_entries_S2(222, rgKey, 228,196,196,196)                      && allOK;
  allOK =  verify_entries_S2(226, rgKey, 60,60,62,62)                          && allOK;      // < >
// -------------------------------------------
  /*allOK =  verify_entries_S2(48, rgKey, 48,48,61,61,L'}',0,0,0)                && allOK;
  allOK =  verify_entries_S2(49, rgKey, 49,49,33,33,L'’',0,0,0)                && allOK;
  allOK =  verify_entries_S2(50, rgKey, 50,50,34,34,L'²',0,0,0)                && allOK;
  allOK =  verify_entries_S2(51, rgKey, 51,51,167,167,L'³',0,0,0)              && allOK;
  allOK =  verify_entries_S2(52, rgKey, 52,52,36,36,L'—',0,0,0)                && allOK;
  allOK =  verify_entries_S2(53, rgKey, 53,53,37,37,L'¡',0,0,0)                && allOK;
  allOK =  verify_entries_S2(54, rgKey, 54,54,38,38,L'¿',0,0,0)                && allOK;
  allOK =  verify_entries_S2(55, rgKey, 55,55,47,47,L'{',0,0,0)                && allOK;
  allOK =  verify_entries_S2(56, rgKey, 56,56,40,40,L'[',0,0,0)                && allOK;
  allOK =  verify_entries_S2(57, rgKey, 57,57,41,41,L']',0,0,0)                && allOK;*/

  return allOK;
}
