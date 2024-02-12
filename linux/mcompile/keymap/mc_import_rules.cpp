/*
  Name:             mc_import_rules
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      3 Aug 2014

  Modified Date:    6 Feb 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          03 Aug 2014 - mcdurdin - I4327 - V9.0 - Mnemonic layout compiler follow-up
                    03 Aug 2014 - mcdurdin - I4353 - V9.0 - mnemonic layout recompiler mixes up deadkey rules
                    31 Dec 2014 - mcdurdin - I4550 - V9.0 - logical flaw in mnemonic layout recompiler means that AltGr base keys are never processed
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
*/


#include <vector>
#include <string>
#include <stdio.h>
#include "km_types.h"
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
  0
};

  DeadKey::DeadKey(KMX_WCHAR deadCharacter) {
    this->m_deadchar = deadCharacter;
  }

  KMX_WCHAR DeadKey::KMX_DeadCharacter() {
    return this->m_deadchar;
  }

  void DeadKey::KMX_AddDeadKeyRow(KMX_WCHAR baseCharacter, KMX_WCHAR combinedCharacter) {
    this->m_rgbasechar.push_back(baseCharacter);
    this->m_rgcombchar.push_back(combinedCharacter);
  }

  bool DeadKey::KMX_ContainsBaseCharacter(KMX_WCHAR baseCharacter) {
    std::vector<KMX_WCHAR>::iterator it;
    for(it=this->m_rgbasechar.begin(); it<m_rgbasechar.end(); it++) {
      if(*it == baseCharacter) {
        return true;
      }
    }
    return false;
  }

int KMX_ToUnicodeEx(guint keycode, const BYTE *lpKeyState, PKMX_WCHAR pwszBuff, int shift_state_pos, int caps,GdkKeymap *keymap) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;
  KMX_DWORD out;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;

  if (!(shift_state_pos <= count))
   return 0;

   if (!(keycode <= 94))
    return 0;

  std::wstring character= KMX_get_CharsUnderlying_according_to_keycode_and_Shiftstate_GDK(keymap, keycode, ShiftState(shift_state_pos), caps);
  pwszBuff[0]= * (PKMX_WCHAR)  u16string_from_wstring(character).c_str();

  KMX_DWORD keyvals_dw= (KMX_DWORD) KMX_get_keyvals_From_Keycode(keymap, keycode, ShiftState(shift_state_pos), caps);

  g_free(keyvals);
  g_free(maps);

  if((keyvals_dw >=  deadkey_min) && (keyvals_dw <=  deadkey_max))          // deadkeys
    return -1;
  else if(gdk_keyval_to_unicode(keyvals_dw)  == 0)                          // NO UNICODE
    return 0;
  else                                                                      // usable char
    return 1;
}

int KMX_DeadKeyMap(int index, std::vector<DeadKey *> *deadkeys, int deadkeyBase, std::vector<KMX_DeadkeyMapping> *deadkeyMappings) {   // I4327   // I4353
  for(size_t i = 0; i < deadkeyMappings->size(); i++) {
    if((*deadkeyMappings)[i].deadkey == index) {
      return (*deadkeyMappings)[i].dkid;
    }
  }

  for(size_t i = 0; i < deadkeys->size(); i++) {
    if((*deadkeys)[i]->KMX_DeadCharacter() == index) {
      return deadkeyBase + i;
    }
  }
  return 0xFFFF;
}

class KMX_VirtualKey {
private:
  KMX_HKL m_hkl;      // _S2 QUESTION do I need this and is void* OK to assume? If I remove this, will there be changes in Data-Vectors?
  KMX_HKL m_hkl;      // _S2 QUESTION do I need this and is void* OK to assume? If I remove this, will there be changes in Data-Vectors?
  UINT m_vk;
  UINT m_sc;
  bool m_rgfDeadKey[10][2];
  std::wstring m_rgss[10][2];

public:

  KMX_VirtualKey(KMX_HKL hkl,UINT virtualKey, GdkKeymap **keymap) {
    this->m_sc=KMX_get_SCUnderlying_From_VKUS(virtualKey);
    this->m_hkl = hkl;
    this->m_vk = virtualKey;
    // _S2 TODO  deadkey
    // _S2 TODO  deadkey
    // memset(this->m_rgfDeadKey,0,sizeof(this->m_rgfDeadKey));
  }

  KMX_VirtualKey(UINT scanCode, KMX_HKL hkl, GdkKeymap **keymap) {
    this->m_vk = KMX_get_VKUS_From_KeyCodeUnderlying_GDK(*keymap, scanCode);
    this->m_hkl = hkl;
    this->m_sc = scanCode;
    //KMX_InitializeDeadkeys();       // _S2 to get all 0 in rgfDeadkey[ ][ ]- why were there numbers???
  }

  UINT VK() {
    return this->m_vk;
  }

  UINT SC() {
    return this->m_sc;
  }
  // _S2 can go later
  std::wstring get_m_rgss(int i,int j) {
    return m_rgss[i][j];
  }
  // _S2 can go later
  bool get_m_rgfDeadkey(int i,int j) {
    return m_rgfDeadKey[i][j];
  }

  // _S2 do we need this??
  void KMX_InitializeDeadkeys() {
    for ( int i=0; i<10;i++) {
      for ( int j=0; j<2;j++) {
        this->m_rgfDeadKey[i][j] = 0;
      }
    }
  }

  std::wstring KMX_GetShiftState(ShiftState shiftState, bool capsLock) {
    return this->m_rgss[(UINT)shiftState][(capsLock ? 1 : 0)];
  }
  
  void KMX_SetShiftState(ShiftState shiftState, std::wstring value, bool isDeadKey, bool capsLock) {
    this->m_rgfDeadKey[(UINT)shiftState][(capsLock ? 1 : 0)] = isDeadKey;
    this->m_rgss[(UINT)shiftState][(capsLock ? 1 : 0)] = value;
  }

// _S2 QUESTION why are there sometimes numbers in m_rgfDeadKey???
// _S2 QUESTION why are there sometimes numbers in m_rgfDeadKey???
  void KMX_SetShiftState(ShiftState shiftState, std::u16string value16, bool isDeadKey, bool capsLock) {
    std::wstring value = wstring_from_u16string(value16);
    this->m_rgfDeadKey[(UINT)shiftState][(capsLock ? 1 : 0)] = isDeadKey;
    this->m_rgss[(UINT)shiftState][(capsLock ? 1 : 0)] = value;
  }

// _S2 DESIGN NEEDED how to change those?
  bool KMX_IsSGCAPS() {
    std::wstring stBase = this->KMX_GetShiftState(Base, false);     // 0,0  a 4 ß
    std::wstring stShift = this->KMX_GetShiftState(Shft, false);    // 1,0  A $ ?
    std::wstring stCaps = this->KMX_GetShiftState(Base, true);      // 0,1  A 4 ẞ
    std::wstring stShiftCaps = this->KMX_GetShiftState(Shft, true); // 1,1  a $ ?
    return (
        ((stCaps.size() > 0) &&
        (stBase.compare(stCaps) != 0) &&                            // stBase != stCaps
        (stShift.compare(stCaps) != 0)) ||                          // stShift!= stCaps
        ((stShiftCaps.size() > 0) &&
        (stBase.compare(stShiftCaps) != 0) &&                       // stBase != stShiftCaps
        (stShift.compare(stShiftCaps) != 0)));                      // stShift!= stShiftCaps
  }

  bool KMX_IsCapsEqualToShift() {
    std::wstring stBase = this->KMX_GetShiftState(Base, false);     // 0,0  a 4 ß
    std::wstring stShift = this->KMX_GetShiftState(Shft, false);    // 1,0  A $ ?
    std::wstring stCaps = this->KMX_GetShiftState(Base, true);      // 0,1  A 4 ẞ
    return (
        (stBase.size() > 0) &&                                      // unshifted char inside
        (stShift.size() > 0) &&                                     // shifted   char inside
        (stBase.compare(stShift) != 0) &&                           // stBase != stShft
        (stShift.compare(stCaps) == 0));                            // stShft == stCaps
  }

  bool KMX_IsAltGrCapsEqualToAltGrShift() {
    std::wstring stBase = this->KMX_GetShiftState(MenuCtrl, false);       // 0,0
    std::wstring stShift = this->KMX_GetShiftState(ShftMenuCtrl, false);  // 1,0
    std::wstring stCaps = this->KMX_GetShiftState(MenuCtrl, true);        // 0,1
    return (
        (stBase.size() > 0) &&                                     // unshifted MenuCtrl/AltGr  char inside
        (stShift.size() > 0) &&                                    // shifted   MenuCtrl/AltGr  char inside
        (stBase.compare(stShift) != 0) &&                          // stBase != stShft
        (stShift.compare(stCaps) == 0));                           // stShft == stCaps
  }

  bool KMX_IsXxxxGrCapsEqualToXxxxShift() {
    std::wstring stBase = this->KMX_GetShiftState(Xxxx, false);
    std::wstring stShift = this->KMX_GetShiftState(ShftXxxx, false);
    std::wstring stCaps = this->KMX_GetShiftState(Xxxx, true);
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

  bool KMX_IsKeymanUsedKey() {
    return (this->m_vk >= 0x20 && this->m_vk <= 0x5F) || (this->m_vk >= 0x88);
  }
  
  UINT KMX_GetShiftStateValue(int capslock, int caps, ShiftState ss) {
    return KMX_ShiftStateMap[(int)ss] | (capslock ? (caps ? CAPITALFLAG : NOTCAPITALFLAG) : 0);
  }

  int KMX_GetKeyCount(int MaxShiftState) {
    int nkeys = 0;

    // Get the CAPSLOCK value
    //_S2 not used in original code; can be deleted
    /*int capslock =
        (this->KMX_IsCapsEqualToShift() ? 1 : 0) |
        (this->KMX_IsSGCAPS() ? 2 : 0) |
        (this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);*/

    for (int ss = 0; ss <= MaxShiftState; ss++) {
      if (ss == Menu || ss == ShftMenu) {
        // Alt and Shift+Alt don't work, so skip them
        continue;
      }
      for (int caps = 0; caps <= 1; caps++) {
        std::wstring st = this->KMX_GetShiftState((ShiftState) ss, (caps == 1));

        if (st.size() == 0) {
          // No character assigned here
        } else if (this->m_rgfDeadKey[(int)ss][caps]) {
          // It's a dead key, append an @ sign.
          nkeys++;
        } else {
          bool isvalid = true;
          for (size_t ich = 0; ich < st.size(); ich++) {
            if(st[ich] < 0x20 || st[ich] == 0x7F) { isvalid=false; break; }
          }
          if(isvalid) {
            nkeys++;
          }
        }
      }
    }
    return nkeys;
  }

  bool KMX_LayoutRow(int MaxShiftState, LPKMX_KEY key, std::vector<DeadKey*> *deadkeys, int deadkeyBase, BOOL bDeadkeyConversion,v_dw_3D &All_Vector, GdkKeymap * keymap) {   // I4552
    // Get the CAPSLOCK value

// _S2 needs to go later                                    // this should be true for char, number, special
bool   b1= this->KMX_IsCapsEqualToShift();                  // but is false for numbers  and    special
bool   b2= this->KMX_IsSGCAPS();
bool   b3= this->KMX_IsAltGrCapsEqualToAltGrShift();
bool   b4= this->KMX_IsXxxxGrCapsEqualToXxxxShift();

int i1 = this->KMX_IsCapsEqualToShift() ? 1 : 0;
int i2 = this->KMX_IsSGCAPS() ? 2 : 0;
int i3 = this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0;
int i4 = this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0;

    int capslock =
        (this->KMX_IsCapsEqualToShift() ? 1 : 0) |
        (this->KMX_IsSGCAPS() ? 2 : 0) |
        (this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);

    // _S2 DESIGN NEEDED on how to replace capslock
    capslock=1;   // _S2
    // _S2 TODO capslock is not calculated correctly for linux. therefore key->ShiftFlags will be wrong for numbers, special characters

    for (int ss = 0; ss <= MaxShiftState; ss++) {
      if (ss == Menu || ss == ShftMenu) {
        // Alt and Shift+Alt don't work, so skip them
        continue;
      }
      for (int caps = 0; caps <= 1; caps++) {
        std::wstring st = this->KMX_GetShiftState((ShiftState) ss, (caps == 1));
        PKMX_WCHAR p;

        if (st.size() == 0) {
          // No character assigned here
        }
        // _S2 TODO deadkeys don't work yet/ if true is in m_rgfDeadKey
        else if (this->m_rgfDeadKey[(int)ss][caps]) {
          // It's a dead key, append an @ sign.
          key->dpContext = new KMX_WCHAR[1];
          *key->dpContext = 0;

          key->ShiftFlags = this->KMX_GetShiftStateValue(capslock, caps, (ShiftState) ss);
          // _S2 we already use VK_US so no need to convert it
          key->Key = this->VK();
          key->Line = 0;

          if(bDeadkeyConversion) {   // I4552
            p = key->dpOutput = new KMX_WCHAR[2];
            *p++ = st[0];
            *p = 0;
          } else {

            p = key->dpOutput = new KMX_WCHAR[4];
            *p++ = UC_SENTINEL;
            *p++ = CODE_DEADKEY;
            *p++ = KMX_DeadKeyMap(st[0], deadkeys, deadkeyBase, &KMX_FDeadkeys);   // I4353
            *p = 0;
          }
          key++;
        } 
        else {
          bool isvalid = true;
          for (size_t ich = 0; ich < st.size(); ich++) {
            if(st[ich] < 0x20 || st[ich] == 0x7F) { isvalid=false; break; }
          }
          if(isvalid) {
            // this is different to mcompile windows !!!!
            // this->m_sc    stores SC-US = SCUnderlying
            // this->m_vk    stores VK-US ( not underlying !!)
            // key->Key      stores VK-US ( not underlying !!)
            // key->dpOutput stores character Underlying

            KMX_DWORD SC_Underlying_gdk = KMX_get_KeyCodeUnderlying_From_KeycodeUS_GDK(keymap, All_Vector,this->SC(), (ShiftState) ss, caps);
            key->Key = KMX_get_VKUS_From_KeyCodeUnderlying_GDK( keymap, SC_Underlying_gdk);

            key->Line = 0;
            key->ShiftFlags = this->KMX_GetShiftStateValue(capslock, caps, (ShiftState) ss);

            key->dpContext = new KMX_WCHAR; 
            *key->dpContext = 0;
            p = key->dpOutput = new KMX_WCHAR[st.size() + 1];

            for(size_t ich = 0; ich < st.size(); ich++) {
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

class KMX_Loader {
private:
  KMX_BYTE lpKeyStateNull[256];
  KMX_UINT m_XxxxVk;

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

  void KMX_FillKeyState(KMX_BYTE *lpKeyState, ShiftState ss, bool fCapsLock) {
    lpKeyState[VK_SHIFT] = (((ss & Shft) != 0) ? 0x80 : 0x00);
    lpKeyState[VK_CONTROL] = (((ss & Ctrl) != 0) ? 0x80 : 0x00);
    lpKeyState[VK_MENU] = (((ss & Menu) != 0) ? 0x80 : 0x00);
    if (Get_XxxxVk() != 0) {
      // The Xxxx key has been assigned, so let's include it
      lpKeyState[Get_XxxxVk()] = (((ss & Xxxx) != 0) ? 0x80 : 0x00);
    }
    lpKeyState[VK_CAPITAL] = (fCapsLock ? 0x01 : 0x00);
  }

  bool KMX_IsControlChar(wchar_t ch) {
    return (ch < 0x0020) || (ch >= 0x007F && ch <= 0x009F);
  }
  bool KMX_IsControlChar(char16_t ch) {
    return (ch < 0x0020) || (ch >= 0x007F && ch <= 0x009F);
  }

  void KMX_ClearKeyboardBuffer() {
    KMX_WCHAR sb[16];
    for( int i=0; i<16; i++) {
      sb[i] = L'\0';
    }
  }
};

int KMX_GetMaxDeadkeyIndex(KMX_WCHAR *p) {
  int n = 0;
  while(p && *p) {
    if(*p == UC_SENTINEL && *(p+1) == CODE_DEADKEY)
      n = std::max(n, (int) *(p+2));
    p = KMX_incxstr(p);
  }
  return n;
}

bool KMX_ImportRules(LPKMX_KEYBOARD kp,v_dw_3D  &All_Vector, GdkKeymap **keymap, std::vector<KMX_DeadkeyMapping> *FDeadkeys, KMX_BOOL bDeadkeyConversion) {   // I4353   // I4552
  KMX_Loader loader;

  KMX_HKL hkl = NULL;

  BYTE lpKeyState[256];// = new KeysEx[256];
  std::vector<KMX_VirtualKey*> rgKey; //= new VirtualKey[256];
  std::vector<DeadKey*> alDead;

  //_S2 REVIEW
   std::vector<DeadKey*> alDead2 ;
   std::vector<DeadKey*> alDead_cpl = create_alDead();
   std::vector<DeadKey*> alDead_cpl = create_alDead();

  rgKey.resize(256);

  // Scroll through the Scan Code (SC) values and get the valid Virtual Key (VK)
  // values in it. Then, store the SC in each valid VK so it can act as both a
  // flag that the VK is valid, and it can store the SC value.

  for(UINT sc = 0x01; sc <= 0x7f; sc++) {
    // fills m_vk with the VK of the US keyboard
    // ( mcompile win uses MapVirtualKeyEx() to fill m_vk with the VK of the Underlying keyboard)
    // Linux cant get a VK for the US Keyboard using USVirtualKeyToScanCode/ScanCodeToUSVirtualKey
    // Linux cannot get a VK for the underling Keyboard
    // this "connection" is possible only while using All_Vector
    KMX_VirtualKey *key = new KMX_VirtualKey(sc, hkl, keymap);

   if((key->VK() != 0) ) {
        rgKey[key->VK()] = key;
    } else {
        delete key;
    }
  }

  for(UINT ke = VK_NUMPAD0; ke <= VK_NUMPAD9; ke++) {
      rgKey[ke] = new KMX_VirtualKey(hkl, ke, keymap);
  }

  // _S2 ???? which numbers for VK_DIVIDE, VK_CANCEL, VK_DECIMAL ?
  rgKey[VK_DIVIDE] = new KMX_VirtualKey(hkl, VK_DIVIDE, keymap);
  rgKey[VK_CANCEL] = new KMX_VirtualKey(hkl, VK_CANCEL, keymap);
  rgKey[VK_DECIMAL] = new KMX_VirtualKey(hkl, VK_DECIMAL, keymap);

  /*
  // _S2 DESIGN NEEDED do we need special shift state now or later?
    // See if there is a special shift state added
    for(UINT vk = 0; vk <= VK_OEM_CLEAR; vk++) {
        UINT sc = MapVirtualKeyEx(vk, 0, hkl);
        UINT vkL = MapVirtualKeyEx(sc, 1, hkl);
        UINT vkR = MapVirtualKeyEx(sc, 3, hkl);
        if((vkL != vkR) &&
            (vk != vkL)) {
            switch(vk) {
                case VK_LCONTROL:
                case VK_RCONTROL:
                case VK_LSHIFT:
                case VK_RSHIFT:
                case VK_LMENU:
                case VK_RMENU:
                    break;

                default:
                    loader.Set_XxxxVk(vk);
                    break;
            }
        }
    }
  */


    // in this part we skip shiftstates 4, 5, 8, 9
    for(UINT iKey = 0; iKey < rgKey.size(); iKey++) {

      if(rgKey[iKey] != NULL) {
        KMX_WCHAR sbBuffer[256];     // Scratchpad we use many places

        for(ShiftState ss = Base; ss <= loader.KMX_MaxShiftState(); ss = (ShiftState)((int)ss + 1)) {
          if(ss == Menu || ss == ShftMenu) {
            // Alt and Shift+Alt don't work, so skip them
            continue;
          }

          //_S2 to compare win-lin kmn-files skip ss6+7; MUST BE restored/removed later!!!!
          if(ss == MenuCtrl|| ss == ShftMenuCtrl) {
            continue;
          }

          KMX_DWORD SC_US = KMX_get_KeyCodeUnderlying_From_VKUS(iKey);

          // _S2 TODO deadkey not finished; Ctrl, Shft +40 not tested
          // _S2 TODO deadkey not finished; Ctrl, Shft +40 not tested
          for(int caps = 0; caps <= 1; caps++) {
            loader.KMX_ClearKeyboardBuffer();
            loader.KMX_FillKeyState(lpKeyState, ss, (caps == 0));
            int rc = KMX_ToUnicodeEx(SC_US, lpKeyState, sbBuffer, ss, caps, *keymap);

            if(rc > 0) {
              if(*sbBuffer == 0) {
                //rgKey[iKey]->KMX_SetShiftState(ss, L"", false, (caps == 0));
                rgKey[iKey]->KMX_SetShiftState(ss, L"", false, (caps));
              }
              else {
                if((rc == 1) &&
                  (ss == Ctrl || ss == ShftCtrl) &&
                  (rgKey[iKey]->VK() == ((UINT)sbBuffer[0] + 0x40))) {
                      // _S2 TODO is this the same behavior on Linux?
                      // if rc ==1 : it got 1  char && +40 in Buffer CTRl pressed
                      // It's dealing with control characters. If ToUnicodeEx gets
                      // VK_A with the Ctrl key pressed, it will write 0x01 to sBuffer[0],
                      // without Ctrl it's 0x41. The if detects this case.
                      // && CTRl +0x40 in the buffer ( which indicates a ctrl press)

                      // ToUnicodeEx has an internal knowledge about those
                      // VK_A ~ VK_Z keys to produce the control characters,
                      // when the conversion rule is not provided in keyboard
                      // layout files
                      continue;
                  }
                if( (ss == Ctrl || ss == ShftCtrl) ) {
                continue;
              }
              sbBuffer[rc] = 0;
              //rgKey[iKey]->KMX_SetShiftState(ss, KeyVal_Other, false, (caps==0));
              rgKey[iKey]->KMX_SetShiftState(ss, sbBuffer, false, (caps));    //_S2
            }
          }
          else if(rc < 0) {
            sbBuffer[2] = 0;
            //rgKey[iKey]->SetShiftState(ss, sbBuffer, true, (caps == 0));
            rgKey[iKey]->KMX_SetShiftState(ss, sbBuffer, true, (caps ));   //_S2
            rgKey[iKey]->KMX_SetShiftState(ss, sbBuffer, true, (caps ));   //_S2

            // It's a dead key; let's flush out whats stored in the keyboard state.
            loader.KMX_ClearKeyboardBuffer();
            DeadKey *dk = NULL;
            refine_alDead(sbBuffer[0], alDead, &alDead_cpl);

           /* for(UINT iDead = 0; iDead < alDead.size(); iDead++) {
                dk = alDead[iDead];
                if(dk->KMX_DeadCharacter() == rgKey[iKey]->KMX_GetShiftState(ss, caps == 0)[0]) {
                    break;
                }
                dk = NULL;
            }
            if(dk == NULL) {
              //alDead.push_back(loader.KMX_ProcessDeadKey(iKey, ss, lpKeyState, rgKey, caps == 0, hkl, *keymap));
              alDead2 = create_alDead();
              alDead = reduce_alDead(alDead2);
            }*/
          }
        }
      }
    }
  }
  //  do we need to sort alDead??
  sort_alDead(alDead, &alDead_cpl);

  //-------------------------------------------------------------
  // Now that we've collected the key data, we need to
  // translate it to kmx and append to the existing keyboard
  //-------------------------------------------------------------

  int nDeadkey = 0;
  LPKMX_GROUP gp = new KMX_GROUP[kp->cxGroupArray+4];  // leave space for old
  memcpy(gp, kp->dpGroupArray, sizeof(KMX_GROUP) * kp->cxGroupArray);

  //
  // Find the current highest deadkey index
  //

  kp->dpGroupArray = gp;
  for(UINT i = 0; i < kp->cxGroupArray; i++, gp++) {
    //if(gp->fUsingKeys && gp->dpNoMatch == NULL) {   // I4550
     // WCHAR *p = gp->dpNoMatch = new WCHAR[4];
     // *p++ = UC_SENTINEL;
     // *p++ = CODE_USE;
     // *p++ = (WCHAR)(kp->cxGroupArray + 1);
     // *p = 0;
    //}
    LPKMX_KEY kkp = gp->dpKeyArray;

    for(UINT j = 0; j < gp->cxKeyArray; j++, kkp++) {
      nDeadkey = std::max(nDeadkey, KMX_GetMaxDeadkeyIndex(kkp->dpContext));
      nDeadkey = std::max(nDeadkey, KMX_GetMaxDeadkeyIndex(kkp->dpOutput));
    }
  }

  kp->cxGroupArray++;
  gp = &kp->dpGroupArray[kp->cxGroupArray-1];
  UINT nKeys = 0;
  for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if ((rgKey[iKey] != NULL) && rgKey[iKey]->KMX_IsKeymanUsedKey() && (!rgKey[iKey]->KMX_IsEmpty())) {
      nKeys+= rgKey[iKey]->KMX_GetKeyCount(loader.KMX_MaxShiftState());
    }
  }


  nDeadkey++; // ensure a 1-based index above the max deadkey value already in the keyboard

  gp->fUsingKeys = TRUE;
  gp->dpMatch = NULL;
  gp->dpName = NULL;
  gp->dpNoMatch = NULL;
  gp->cxKeyArray = nKeys;
  gp->dpKeyArray = new KMX_KEY[gp->cxKeyArray];
  nKeys = 0;


  //
  // Fill in the new rules
  //
  for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if ((rgKey[iKey] != NULL) && rgKey[iKey]->KMX_IsKeymanUsedKey() && (!rgKey[iKey]->KMX_IsEmpty())) {
      if(rgKey[iKey]->KMX_LayoutRow(loader.KMX_MaxShiftState(), &gp->dpKeyArray[nKeys], &alDead, nDeadkey, bDeadkeyConversion, All_Vector,*keymap)) {   // I4552
        nKeys+=rgKey[iKey]->KMX_GetKeyCount(loader.KMX_MaxShiftState());
      }
    }
  }

  gp->cxKeyArray = nKeys;

  //
  // Add nomatch control to each terminating 'using keys' group   // I4550
  //
  LPKMX_GROUP gp2 = kp->dpGroupArray;
  for(UINT i = 0; i < kp->cxGroupArray - 1; i++, gp2++) {
    if(gp2->fUsingKeys && gp2->dpNoMatch == NULL) {
      KMX_WCHAR *p = gp2->dpNoMatch = new KMX_WCHAR[4];
      KMX_WCHAR *q = p;
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
      for(j = 0, kkp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kkp++) {
          // _S2 AHA! missing 26 lines from here: since capalock is not correct this loop  will never be done
        if((kkp->ShiftFlags & (K_CTRLFLAG|K_ALTFLAG|LCTRLFLAG|LALTFLAG|RCTRLFLAG|RALTFLAG)) != 0) {
          gp2->cxKeyArray++;
          LPKMX_KEY kkp2 = new KMX_KEY[gp2->cxKeyArray];
          memcpy(kkp2, gp2->dpKeyArray, sizeof(KMX_KEY)*(gp2->cxKeyArray-1));
          gp2->dpKeyArray = kkp2;
          kkp2 = &kkp2[gp2->cxKeyArray-1];
          kkp2->dpContext = new KMX_WCHAR; 
          *kkp2->dpContext = 0;
          kkp2->Key = kkp->Key;
          kkp2->ShiftFlags = kkp->ShiftFlags;
          kkp2->Line = 0;
          KMX_WCHAR *p = kkp2->dpOutput = new KMX_WCHAR[4];
          KMX_WCHAR *q=p;
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
  // DK_PART
  if (alDead.size() > 0 && !bDeadkeyConversion) {   // I4552
    kp->cxGroupArray++;

    KMX_WCHAR *p = gp->dpMatch = new KMX_WCHAR[4];
    *p++ = UC_SENTINEL;
    *p++ = CODE_USE;
    *p++ = (KMX_WCHAR) kp->cxGroupArray;
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

    for(UINT i = 0; i < alDead.size(); i++) {
      DeadKey *dk = alDead[i];

      sp->dpName = NULL;
      sp->dwSystemID = 0;
      sp->dpString = new KMX_WCHAR[dk->KMX_Count() + 1];
      for(int j = 0; j < dk->KMX_Count(); j++)
        sp->dpString[j] = dk->KMX_GetBaseCharacter(j);
      sp->dpString[dk->KMX_Count()] = 0;
      sp++;

      sp->dpName = NULL;
      sp->dwSystemID = 0;
      sp->dpString = new KMX_WCHAR[dk->KMX_Count() + 1];
      for(int j = 0; j < dk->KMX_Count(); j++)
        sp->dpString[j] = dk->KMX_GetCombinedCharacter(j);
      sp->dpString[dk->KMX_Count()] = 0;
      sp++;

      kkp->Line = 0;
      kkp->ShiftFlags = 0;
      kkp->Key = 0;
      KMX_WCHAR *p = kkp->dpContext = new KMX_WCHAR[8];
      *p++ = UC_SENTINEL;
      *p++ = CODE_DEADKEY;
      *p++ = KMX_DeadKeyMap(dk->KMX_DeadCharacter(), &alDead, nDeadkey, FDeadkeys);   // I4353
      // *p++ = nDeadkey+i;
      *p++ = UC_SENTINEL;
      *p++ = CODE_ANY;
      *p++ = nStoreBase + i*2 + 1;
      *p = 0;

      p = kkp->dpOutput = new KMX_WCHAR[5];
      *p++ = UC_SENTINEL;
      *p++ = CODE_INDEX;
      *p++ = nStoreBase + i*2 + 2;
      *p++ = 2;
      *p = 0;
      kkp++;
    }
  }
return true;
}

const int CODE__SIZE[] = {
   -1,   // undefined                0x00
    1,   // CODE_ANY                 0x01
    2,   // CODE_INDEX               0x02
    0,   // CODE_CONTEXT             0x03
    0,   // CODE_NUL                 0x04
    1,   // CODE_USE                 0x05
    0,   // CODE_RETURN              0x06
    0,   // CODE_BEEP                0x07
    1,   // CODE_DEADKEY             0x08
   -1,  // unused                   0x09
    2,   // CODE_EXTENDED            0x0A
   -1,  // CODE_EXTENDEDEND         0x0B (unused)
    1,   // CODE_SWITCH              0x0C
   -1,  // CODE_KEY                 0x0D (never used)
    0,   // CODE_CLEARCONTEXT        0x0E
    1,   // CODE_CALL                0x0F
   -1,  // UC_SENTINEL_EXTENDEDEND  0x10 (not valid with UC_SENTINEL)
    1,   // CODE_CONTEXTEX           0x11
    1,   // CODE_NOTANY              0x12
    2,   // CODE_SETOPT              0x13
    3,   // CODE_IFOPT               0x14
    1,   // CODE_SAVEOPT             0x15
    1,   // CODE_RESETOPT            0x16
    3,   // CODE_IFSYSTEMSTORE       0x17
    2    // CODE_SETSYSTEMSTORE      0x18
};
