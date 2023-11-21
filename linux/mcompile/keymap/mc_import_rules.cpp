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

class DeadKey {
private:
  KMX_WCHAR m_deadchar;
  std::vector<KMX_WCHAR> m_rgbasechar;
  std::vector<KMX_WCHAR> m_rgcombchar;

public:
/*  DeadKey(WCHAR deadCharacter) {
    this->m_deadchar = deadCharacter;
  }
*/
  KMX_WCHAR KMX_DeadCharacter() {
    return this->m_deadchar;
  }
/*
  void AddDeadKeyRow(WCHAR baseCharacter, WCHAR combinedCharacter) {
    this->m_rgbasechar.push_back(baseCharacter);
    this->m_rgcombchar.push_back(combinedCharacter);
  }
*/
  int KMX_Count() {
    return this->m_rgbasechar.size();
  }

  KMX_WCHAR KMX_GetBaseCharacter(int index) {
    return this->m_rgbasechar[index];
  }

  KMX_WCHAR KMX_GetCombinedCharacter(int index) {
    return this->m_rgcombchar[index];
  }
/*
  bool ContainsBaseCharacter(WCHAR baseCharacter) {
    std::vector<WCHAR>::iterator it;
    for(it=this->m_rgbasechar.begin(); it<m_rgbasechar.end(); it++) {
      if(*it == baseCharacter) {
        return true;
      }
    }
    return false;
  }*/
};


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
  KMX_HKL m_hkl;      // _S2 do I need this and is void* OK to assume?
  UINT m_vk;
  UINT m_sc;
  bool m_rgfDeadKey[10][2];
  std::wstring m_rgss[10][2];

public:
// _S2 can be deleted later
 /* KMX_VirtualKey(KMX_HKL hkl, UINT KMX_virtualKey) {
    this->m_sc = MapVirtualKeyEx(virtualKey, 0, hkl);
    this->m_hkl = hkl;
    this->m_vk = KMX_virtualKey;
    memset(this->m_rgfDeadKey,0,sizeof(this->m_rgfDeadKey));
  }*/
  // _S2 can be deleted later
  /*KMX_VirtualKey(UINT scanCode, KMX_HKL hkl) {
    //    this->m_vk = MapVirtualKeyEx(scanCode, 1, hkl);
    this->m_hkl = hkl;
    this->m_sc = scanCode;
  }*/


  /*KMX_VirtualKey(KMX_HKL hkl, UINT KMX_virtualKey, v_dw_3D All_Vector) {
    this->m_sc = MapVirtualKeyEx(virtualKey, 0, hkl); // second para =0: MAPVK_VK_TO_VSC=1
                                                        //the uCode parameter is a virtual-key code and is
                                                        //translated into a scan code. If it is a virtual-key
                                                        //code that does not distinguish between left- and
                                                        //right-hand keys, the left-hand scan code is returned.
                                                        //If there is no translation, the function returns 0.
    this->m_sc = get_SC_From_VirtualKey_Other(KMX_virtualKey, All_Vector);
    this->m_hkl = hkl;
    this->m_vk = KMX_virtualKey;
    memset(this->m_rgfDeadKey,0,sizeof(this->m_rgfDeadKey));
  }*/

  /*KMX_VirtualKey(UINT scanCode, KMX_HKL hkl, v_dw_3D All_Vector) {
    // _S2 this->m_vk = MapVirtualKeyEx(scanCode, 1, hkl);  // second para= 1: MAPVK_VSC_TO_VK =1
    //                                                  The first parameter is a scan code and is
    //                                                  translated into a virtual-key code that does not
    //                                                  distinguish between left- and right-hand keys.
    //                                                  If there is no translation, the function returns 0.
    //                                                  SC -> VK
    this->m_vk = get_VirtualKey_Other_From_SC(scanCode, All_Vector);
    this->m_hkl = hkl;
    this->m_sc = scanCode;
  }*/

  KMX_VirtualKey(UINT scanCode, KMX_HKL hkl, v_dw_3D All_Vector, GdkKeymap **keymap) {
    this->m_vk = get_VirtualKey_Other_GDK(*keymap, scanCode);
    this->m_hkl = hkl;
    this->m_sc = scanCode ;
  }

  KMX_VirtualKey(KMX_HKL hkl,UINT scanCode,  v_dw_3D All_Vector, GdkKeymap **keymap) {
    this->m_vk = get_VirtualKey_Other_GDK(*keymap, scanCode);
    this->m_hkl = hkl;
    this->m_sc = scanCode ;
    // _S2 ?? memset(this->m_rgfDeadKey,0,sizeof(this->m_rgfDeadKey));
  }

  UINT VK() {
    return this->m_vk;
  }

  UINT SC() {
    return this->m_sc;
  }

  std::wstring KMX_GetShiftState(ShiftState shiftState, bool capsLock) {
    return this->m_rgss[(UINT)shiftState][(capsLock ? 1 : 0)];
  }
  
  void KMX_SetShiftState(ShiftState shiftState, std::wstring value, bool isDeadKey, bool capsLock) {
    this->m_rgfDeadKey[(UINT)shiftState][(capsLock ? 1 : 0)] = isDeadKey;
    this->m_rgss[(UINT)shiftState][(capsLock ? 1 : 0)] = value;
  }

  bool KMX_IsSGCAPS() {
    std::wstring stBase = this->KMX_GetShiftState(Base, false);
    std::wstring stShift = this->KMX_GetShiftState(Shft, false);
    std::wstring stCaps = this->KMX_GetShiftState(Base, true);
    std::wstring stShiftCaps = this->KMX_GetShiftState(Shft, true);
    return (
        ((stCaps.size() > 0) &&
        (stBase.compare(stCaps) != 0) &&
        (stShift.compare(stCaps) != 0)) ||
        ((stShiftCaps.size() > 0) &&
        (stBase.compare(stShiftCaps) != 0) &&
        (stShift.compare(stShiftCaps) != 0)));
  }

  bool KMX_IsCapsEqualToShift() {
    std::wstring stBase = this->KMX_GetShiftState(Base, false);
    std::wstring stShift = this->KMX_GetShiftState(Shft, false);
    std::wstring stCaps = this->KMX_GetShiftState(Base, true);
    return (
        (stBase.size() > 0) &&
        (stShift.size() > 0) &&
        (stBase.compare(stShift) != 0) &&
        (stShift.compare(stCaps) == 0));
  }

  bool KMX_IsAltGrCapsEqualToAltGrShift() {
    std::wstring stBase = this->KMX_GetShiftState(MenuCtrl, false);
    std::wstring stShift = this->KMX_GetShiftState(ShftMenuCtrl, false);
    std::wstring stCaps = this->KMX_GetShiftState(MenuCtrl, true);
    return (
        (stBase.size() > 0) &&
        (stShift.size() > 0) &&
        (stBase.compare(stShift) != 0) &&
        (stShift.compare(stCaps) == 0));
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
    return 
      KMX_ShiftStateMap[(int)ss] |
      (capslock ? (caps ? CAPITALFLAG : NOTCAPITALFLAG) : 0);
  }

  int KMX_GetKeyCount(int MaxShiftState) {
    int nkeys = 0;

    // Get the CAPSLOCK value

bool   b1= this->KMX_IsCapsEqualToShift();
bool   b2= this->KMX_IsSGCAPS();
bool   b3= this->KMX_IsAltGrCapsEqualToAltGrShift();
bool   b4= this->KMX_IsXxxxGrCapsEqualToXxxxShift() ;

int i1 = this->KMX_IsCapsEqualToShift() ? 1 : 0;
int i2 = this->KMX_IsSGCAPS() ? 2 : 0;
int i3 = this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0;
int i4 = this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0;




    int capslock =
        (this->KMX_IsCapsEqualToShift() ? 1 : 0) |
        (this->KMX_IsSGCAPS() ? 2 : 0) |
        (this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);

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

  bool KMX_LayoutRow(int MaxShiftState, LPKMX_KEY key, std::vector<DeadKey*> *deadkeys, int deadkeyBase, BOOL bDeadkeyConversion,v_dw_3D &All_Vector) {   // I4552
    // Get the CAPSLOCK value


bool   b1= this->KMX_IsCapsEqualToShift();
bool   b2= this->KMX_IsSGCAPS();
bool   b3= this->KMX_IsAltGrCapsEqualToAltGrShift();
bool   b4= this->KMX_IsXxxxGrCapsEqualToXxxxShift() ;

int i1 = this->KMX_IsCapsEqualToShift() ? 1 : 0;
int i2 = this->KMX_IsSGCAPS() ? 2 : 0;
int i3 = this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0;
int i4 = this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0;



    int capslock =
        (this->KMX_IsCapsEqualToShift() ? 1 : 0) |
        (this->KMX_IsSGCAPS() ? 2 : 0) |
        (this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);

    for (int ss = 0; ss <= MaxShiftState; ss++) {
      if (ss == Menu || ss == ShftMenu) {
        // Alt and Shift+Alt don't work, so skip them
        continue;
      }
      for (int caps = 0; caps <= 1; caps++) {
        std::wstring st = this->KMX_GetShiftState((ShiftState) ss, (caps == 1));
        PKMX_WCHAR p;  // was PWSTR p;

        if (st.size() == 0) {
          // No character assigned here
        }
        // _S2 deadkeys don't work yet
        else if (this->m_rgfDeadKey[(int)ss][caps]) {
          // It's a dead key, append an @ sign.
          key->dpContext = new KMX_WCHAR[1];
          *key->dpContext = 0;
          key->ShiftFlags = this->KMX_GetShiftStateValue(capslock, caps, (ShiftState) ss);

            // _S2 this fun returns the shifted Char it goes wrog for numbers, special here!!
          key->Key = KMX_VKUnderlyingLayoutToVKUS(All_Vector,this->VK());
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
        } else {
          bool isvalid = true;
          for (size_t ich = 0; ich < st.size(); ich++) {
            if(st[ich] < 0x20 || st[ich] == 0x7F) { isvalid=false; break; }
          }
            
          if(isvalid) {
            // _S2 this fun returns the shifted Char instead of keyname -> use gdk function!!
            key->Key = KMX_VKUnderlyingLayoutToVKUS(All_Vector,this->VK());
            key->Line = 0;
            key->ShiftFlags = this->KMX_GetShiftStateValue(capslock, caps, (ShiftState) ss);
            key->dpContext = new KMX_WCHAR; *key->dpContext = 0;
            p = key->dpOutput = new KMX_WCHAR[st.size() + 1];
            for(size_t ich = 0; ich < st.size(); ich++) *p++ = st[ich];
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

  ShiftState MaxShiftState() {
    return (Get_XxxxVk() == 0 ? ShftMenuCtrl : ShftXxxx);
  }
/*
  void FillKeyState(BYTE *lpKeyState, ShiftState ss, bool fCapsLock) {
    lpKeyState[VK_SHIFT] = (((ss & Shft) != 0) ? 0x80 : 0x00);
    lpKeyState[VK_CONTROL] = (((ss & Ctrl) != 0) ? 0x80 : 0x00);
    lpKeyState[VK_MENU] = (((ss & Menu) != 0) ? 0x80 : 0x00);
    if (Get_XxxxVk() != 0) {
      // The Xxxx key has been assigned, so let's include it
      lpKeyState[Get_XxxxVk()] = (((ss & Xxxx) != 0) ? 0x80 : 0x00);
    }
    lpKeyState[VK_CAPITAL] = (fCapsLock ? 0x01 : 0x00);
  }

  bool IsControlChar(wchar_t ch) {
    return (ch < 0x0020) || (ch >= 0x007F && ch <= 0x009F);
  }

  DeadKey *ProcessDeadKey(
      UINT iKeyDead,              // The index into the VirtualKey of the dead key
      ShiftState shiftStateDead,  // The shiftstate that contains the dead key
      BYTE *lpKeyStateDead,       // The key state for the dead key
      std::vector<VirtualKey*> rgKey,          // Our array of dead keys
      bool fCapsLock,             // Was the caps lock key pressed?
      HKL hkl) {                  // The keyboard layout

    BYTE lpKeyState[256];
    DeadKey *deadKey = new DeadKey(rgKey[iKeyDead]->GetShiftState(shiftStateDead, fCapsLock)[0]);

    for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
      if (rgKey[iKey] != NULL) {
        WCHAR sbBuffer[16];

        for (ShiftState ss = Base; ss <= MaxShiftState(); ss = (ShiftState)((int)ss+1)) {
          int rc = 0;
          if (ss == Menu || ss == ShftMenu) {
            // Alt and Shift+Alt don't work, so skip them
            continue;
          }

          for (int caps = 0; caps <= 1; caps++) {
            // First the dead key
            while (rc >= 0) {
              // We know that this is a dead key coming up, otherwise
              // this function would never have been called. If we do
              // *not* get a dead key then that means the state is 
              // messed up so we run again and again to clear it up.
              // Risk is technically an infinite loop but per Hiroyama
              // that should be impossible here.
              rc = ToUnicodeEx(rgKey[iKeyDead]->VK(), rgKey[iKeyDead]->SC(), lpKeyStateDead, sbBuffer, _countof(sbBuffer), 0, hkl);
            }

            // Now fill the key state for the potential base character
            FillKeyState(lpKeyState, ss, (caps != 0));

            rc = ToUnicodeEx(rgKey[iKey]->VK(), rgKey[iKey]->SC(), lpKeyState, sbBuffer, _countof(sbBuffer), 0, hkl);
            if (rc == 1) {
              // That was indeed a base character for our dead key.
              // And we now have a composite character. Let's run
              // through one more time to get the actual base 
              // character that made it all possible?
              WCHAR combchar = sbBuffer[0];
              rc = ToUnicodeEx(rgKey[iKey]->VK(), rgKey[iKey]->SC(), lpKeyState, sbBuffer, _countof(sbBuffer), 0, hkl);

              WCHAR basechar = sbBuffer[0];

              if (deadKey->DeadCharacter() == combchar) {
                // Since the combined character is the same as the dead key,
                // we must clear out the keyboard buffer.
                ClearKeyboardBuffer(VK_DECIMAL, rgKey[VK_DECIMAL]->SC(), hkl);
              }

              if ((((ss == Ctrl) || (ss == ShftCtrl)) &&
                  (IsControlChar(basechar))) ||
                  (basechar == combchar)) {
                // ToUnicodeEx has an internal knowledge about those 
                // VK_A ~ VK_Z keys to produce the control characters, 
                // when the conversion rule is not provided in keyboard 
                // layout files

                // Additionally, dead key state is lost for some of these
                // character combinations, for unknown reasons.

                // Therefore, if the base character and combining are equal,
                // and its a CTRL or CTRL+SHIFT state, and a control character
                // is returned, then we do not add this "dead key" (which
                // is not really a dead key).
                continue;
              }

              if (!deadKey->ContainsBaseCharacter(basechar)) {
                deadKey->AddDeadKeyRow(basechar, combchar);
              }
            } else if (rc > 1) {
              // Not a valid dead key combination, sorry! We just ignore it.
            } else if (rc < 0) {
              // It's another dead key, so we ignore it (other than to flush it from the state)
              ClearKeyboardBuffer(VK_DECIMAL, rgKey[VK_DECIMAL]->SC(), hkl);
            }
          }
        }
      }
    }
    return deadKey;
  }

  void KMX_ClearKeyboardBuffer(UINT vk, UINT sc, HKL hkl) {
    WCHAR sb[16];
    int rc = 0;
    do {
      rc = ::ToUnicodeEx(vk, sc, lpKeyStateNull, sb, _countof(sb), 0, hkl);
    } while(rc != 1 && rc != 0);
  }*/
};

// _S2 where to put this??
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

// _S2 where to put this??
PKMX_WCHAR KMX_incxstr(PKMX_WCHAR p) {

  if (*p == 0)
    return p;
  if (*p != UC_SENTINEL) {
    if (*p >= 0xD800 && *p <= 0xDBFF && *(p + 1) >= 0xDC00 && *(p + 1) <= 0xDFFF)
      return p + 2;
    return p + 1;
  }
  // UC_SENTINEL(FFFF) with UC_SENTINEL_EXTENDEDEND(0x10) == variable length
  if (*(p + 1) == CODE_EXTENDED) {
    p += 2;
    while (*p && *p != UC_SENTINEL_EXTENDEDEND)
      p++;

    if (*p == 0)        return p;
    return p + 1;
  }

  if (*(p + 1) > CODE_LASTCODE || CODE__SIZE[*(p + 1)] == -1) {
    return p + 1;
  }

  int deltaptr = 2 + CODE__SIZE[*(p + 1)];

  // check for \0 between UC_SENTINEL(FFFF) and next printable character
  for (int i = 0; i < deltaptr; i++) {
    if (*p == 0)
      return p;
    p++;
  }
  return p;
}

int KMX_GetMaxDeadkeyIndex(KMX_WCHAR *p) {
  int n = 0;
  while(p && *p) {
    if(*p == UC_SENTINEL && *(p+1) == CODE_DEADKEY)

     // n = max(n, *(p+2));
    if( !(n > (*p+2)))       // _S2 p+4 ??
      n= (*p+2);

    p = KMX_incxstr(p);
  }
  return n;
}

bool IsKeymanUsedKey_S2(std::wstring SC_Other) { 

  int SC_US = (int) (*SC_Other.c_str());

  if ((SC_US>= 0x20 && SC_US <= 0x7A) || (SC_US >= 0x88 && SC_US < 0xFF))
    return true;
  else
    return false;

}

bool KMX_ImportRules(KMX_WCHAR *kbid, LPKMX_KEYBOARD kp,v_dw_3D  &All_Vector, GdkKeymap **keymap, std::vector<KMX_DeadkeyMapping> *FDeadkeys, KMX_BOOL bDeadkeyConversion) {   // I4353   // I4552
  KMX_Loader loader;
  const size_t BUF_sz= 256;

  // _S2 do I need that for Linux??
  KMX_WCHAR inputHKL[12];
  u16sprintf(inputHKL,BUF_sz ,L"%08.8x", (unsigned int) u16tol(kbid, NULL, 16));   // _S2 wsprintf(inputHKL, L"%08.8x", (unsigned int) wcstol(kbid, NULL, 16));


  /*
    // _S2 do I need that for Linux??
    int cKeyboards = GetKeyboardLayoutList(0, NULL);
    HKL *rghkl = new HKL[cKeyboards];
    GetKeyboardLayoutList(cKeyboards, rghkl);
    HKL hkl = LoadKeyboardLayout(inputHKL, KLF_NOTELLSHELL);
    if(hkl == NULL) {
        puts("Sorry, that keyboard does not seem to be valid.");
        delete[] rghkl;
        return false;
    }
    */
  KMX_HKL hkl = NULL;               //_S2 added: but can I do this?? hkl is not needed in Linux??

  BYTE lpKeyState[256];// = new KeysEx[256];
  std::vector<KMX_VirtualKey*> rgKey; //= new VirtualKey[256];
  std::vector<DeadKey*> alDead;

  rgKey.resize(256);

  // _S2 scroll through OTHER
  // Scroll through the Scan Code (SC) values and get the valid Virtual Key (VK)
  // values in it. Then, store the SC in each valid VK so it can act as both a 
  // flag that the VK is valid, and it can store the SC value.
    // _S2 this does not find exactly the same keys as the windows version does(windows finds more)

  int keycountS =0;
  for(UINT sc = 0x01; sc <= 0x7f; sc++) {
    // fills m_vk with the VK of the US keyboard which is not right!!
    // ( mcompile win uses MapVirtualKeyEx() to fill m_vk with the VK of the Other keyboard)
    // Linux cant get a VK for the US Keyboard using USVirtualKeyToScanCode/ScanCodeToUSVirtualKey
    // Linux cannot get a VK for Other Keyboard
    // it could return SC if that helps
    KMX_VirtualKey *key = new KMX_VirtualKey(sc, hkl, All_Vector, keymap);

  // _S2 ToDo
  // either it gives the correct rgkeys (all non-char filled with special char) or
  // it gives not all rgkeys but nr, a-z are filled correctly
   if((key->VK() != 0) && (key->VK()< 255)) {  // if used without ScanCodeToUSVirtualKey[]
   // if((key->VK() != 0) ) {
      keycountS++;
        rgKey[key->VK()] = key;
    } else {
        delete key;
    }
  }

  for(UINT ke = VK_NUMPAD0; ke <= VK_NUMPAD9; ke++) {
      rgKey[ke] = new KMX_VirtualKey(hkl, ke, All_Vector, keymap);
  }

  rgKey[VK_DIVIDE] = new KMX_VirtualKey(hkl, VK_DIVIDE, All_Vector, keymap);
  rgKey[VK_CANCEL] = new KMX_VirtualKey(hkl, VK_CANCEL, All_Vector, keymap);
  rgKey[VK_DECIMAL] = new KMX_VirtualKey(hkl, VK_DECIMAL, All_Vector, keymap);

/*
 // _S2 do we need special shift state now or later?
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
  // _S2 test rgkey can go later
  for(UINT iKey = 100; iKey < rgKey.size(); iKey++) {
      if(rgKey[iKey] != NULL) {
          wprintf(L" Key Nr %i is available\n",iKey);
      }
  }

  // _S2 in this part we skip shiftstates 4, 5, 8, 9
  for(UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if(rgKey[iKey] != NULL) {
      WCHAR sbBuffer[256];     // Scratchpad we use many places

      UINT SC_Other = Lin_KM__map(iKey, All_Vector);

      for(ShiftState ss = Base; ss <= loader.MaxShiftState(); ss = (ShiftState)((int)ss + 1)) {
        if(ss == Menu || ss == ShftMenu) {
          // Alt and Shift+Alt don't work, so skip them
          continue;
        }

        KMX_DWORD SC_US;
        if( SC_Other>7)
          SC_US=  (KMX_DWORD)(8+ USVirtualKeyToScanCode[ SC_Other ]);
        else SC_US = 0;


//wprintf(L" for vk of %i we get Keycode US of %i  SC_Other %i: ( on Key US (%i) we find char %i (%c) ) \n", SC_Other, 8+USVirtualKeyToScanCode[SC_Other], 8+USVirtualKeyToScanCode[SC_Other] ,
//8+USVirtualKeyToScanCode[SC_Other],get_VirtualKey_Other_GDK(*keymap, 8+USVirtualKeyToScanCode[SC_Other]),get_VirtualKey_Other_GDK(*keymap, 8+USVirtualKeyToScanCode[SC_Other]) );


        for(int caps = 0; caps <= 1; caps++) {

          //_S2 TODO get char  - do I need rc ?? ( was rc = ToUnicodeEx...)
          //std::wstring VK_Other_OLD = get_VirtualKey_Other_from_iKey(SC_Other, ss, caps, All_Vector);
          std::wstring VK_Other = get_KeySyms_according_to_Shiftstate( *keymap, SC_US, ss, caps);
          //std::wstring VK_Other3 = get_KeySyms_according_to_Shiftstate( *keymap, SC_Other, ss, caps);


         //std::wstring   VK_OtherTEST6 = get_KeySyms_according_to_Shiftstate( *keymap, 51, MenuCtrl, 0);
         // std::wstring  VK_OtherTEST16 = get_KeySyms_according_to_Shiftstate( *keymap, 51, MenuCtrl, 1);

          /*std::wstring VK_OtherTEST ;
          std::wstring  VK_OtherTEST0 = get_KeySyms_according_to_Shiftstate( *keymap, 58, Base, 0);
          std::wstring  VK_OtherTEST1= get_KeySyms_according_to_Shiftstate( *keymap, 58, Shft, 0);
          std::wstring  VK_OtherTEST2 = get_KeySyms_according_to_Shiftstate( *keymap, 58, Ctrl, 0);
          std::wstring  VK_OtherTEST3 = get_KeySyms_according_to_Shiftstate( *keymap, 58, ShftCtrl, 0);
          std::wstring  VK_OtherTEST4 = get_KeySyms_according_to_Shiftstate( *keymap, 58, Menu, 0);
          std::wstring  VK_OtherTEST5 = get_KeySyms_according_to_Shiftstate( *keymap, 58, ShftMenu, 0);
          std::wstring  VK_OtherTEST7 = get_KeySyms_according_to_Shiftstate( *keymap, 58, ShftMenuCtrl, 0);
          std::wstring  VK_OtherTEST8 = get_KeySyms_according_to_Shiftstate( *keymap, 58, Xxxx, 0);
          std::wstring  VK_OtherTEST9 = get_KeySyms_according_to_Shiftstate( *keymap, 58, ShftXxxx, 0);
          std::wstring  VK_OtherTEST10 = get_KeySyms_according_to_Shiftstate( *keymap, 58, Base, 1);
          std::wstring  VK_OtherTEST11 = get_KeySyms_according_to_Shiftstate( *keymap, 58, Shft, 1);
          std::wstring  VK_OtherTEST12 = get_KeySyms_according_to_Shiftstate( *keymap, 58, Ctrl, 1);
          std::wstring  VK_OtherTEST13 = get_KeySyms_according_to_Shiftstate( *keymap, 58, ShftCtrl, 1);
          std::wstring  VK_OtherTEST14 = get_KeySyms_according_to_Shiftstate( *keymap, 58, Menu, 1);
          std::wstring  VK_OtherTEST15 = get_KeySyms_according_to_Shiftstate( *keymap, 58, ShftMenu, 1);
          std::wstring  VK_OtherTEST17 = get_KeySyms_according_to_Shiftstate( *keymap, 58, ShftMenuCtrl, 1);
          std::wstring  VK_OtherTEST18 = get_KeySyms_according_to_Shiftstate( *keymap, 58, Xxxx, 1);
          std::wstring  VK_OtherTEST19 = get_KeySyms_according_to_Shiftstate( *keymap, 58, ShftXxxx, 1);
         */
          // _S2 needs to be changed - it's temporary to get the same keys as keyman does when using USVirtualKeyToScanCode
        if (!IsKeymanUsedKey_S2(VK_Other))
          VK_Other =L"\0";

          //_S2 TODO do I need that ??
          //if rc >0: it got 1 or more char AND buffer is empty ( nothing inside ) {
            if(VK_Other == L"") {
              //rgKey[iKey]->KMX_SetShiftState(ss, L"", false, (caps == 0));
              rgKey[iKey]->KMX_SetShiftState(ss, L"", false, (caps));
            }

            //_S2 TODO
            //else   // if rc ==1 : it got 1  char && +40 in Buffer CTRl pressed    {
                //It's dealing with control characters. If ToUnicodeEx gets VK_A with the Ctrl key pressed,
                //it will write 0x01 to sBuffer[0] , without Ctrl it's 0x41. The if detects this case.
            if( (ss == Ctrl || ss == ShftCtrl) /*&& CTRl +0x40 in the buffer ( which indicates a ctrl press)   */) {
              continue;
            }

            //_S2 TODO fill m_rgfDeadkey ( m_rgfDeadkey will be done later)
            //rgKey[iKey]->KMX_SetShiftState(ss, VK_Other, false, (caps==0));
            rgKey[iKey]->KMX_SetShiftState(ss, VK_Other, false, (caps));


        //_S2 TODO
        // _S2 handle deadkeys later
        // if rc <0:  it got a deadkey   {
            // fill m_rgss and m_rgfDeadkey and alDead
            //SET_SHIFTSTATES( deadkey)   //sbuffer is value out of ToUnicodeEx / AllVector
            // do more stuff for deadkeys...
        // } from rc<0
        }
      }
    }
  }

  //_S2 this gan co later
  //std::vector< int > TestValues = {48,49,50,52,53,54,55,56,57,54,65,89,189,188};
  //std::vector< int > TestValues = {48,49,50,51,52,53,54,55,56,57,65,66,67,88,89,90, 176,177,196,214,220};
  std::vector< int > TestValues = {48,49,50,51,52,53,54,55,56,57,65,66,67,88,89,90};
 // std::vector< int > TestValues = {65};
  wprintf(L"-----------------\nNow some tests:\n");
  for ( int i=0; i < (int) TestValues.size();i++) {
    wprintf(L"Results for %i\t: %s  %s  %s  %s   \n", TestValues[i], rgKey[TestValues[i]]->KMX_GetShiftState(Base,0).c_str(),   rgKey[TestValues[i]]->KMX_GetShiftState(Base,1).c_str() ,  rgKey[TestValues[i]]->KMX_GetShiftState(Shft,0).c_str(), rgKey[TestValues[i]]->KMX_GetShiftState(Shft,1).c_str());
  }
  wprintf(L"-----------------\n");

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
      nKeys+= rgKey[iKey]->KMX_GetKeyCount(loader.MaxShiftState());
      wprintf(L" iKey = %i, Delta:  %i \n", iKey, rgKey[iKey]->KMX_GetKeyCount(loader.MaxShiftState()));
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
      //wprintf(L"********************************* I use Key Nr %i\n",iKey);
      // for each item, 
      if(rgKey[iKey]->KMX_LayoutRow(loader.MaxShiftState(), &gp->dpKeyArray[nKeys], &alDead, nDeadkey, bDeadkeyConversion, All_Vector)) {   // I4552
        nKeys+=rgKey[iKey]->KMX_GetKeyCount(loader.MaxShiftState());
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
      *p++ = UC_SENTINEL;
      *p++ = CODE_USE;
      *p++ = (KMX_WCHAR)(kp->cxGroupArray);
      *p = 0;

      // _S2 TODO not sure if this works OK -> we need to use more shiftstates than base+Shift
      // I4550 - Each place we have a nomatch > use(baselayout) (this last group), we need to add all 
      // the AltGr and ShiftAltGr combinations as rules to allow them to be matched as well.  Yes, this
      // loop is not very efficient but it's not worthy of optimisation.
      //
      UINT j;
      LPKMX_KEY kkp;
      for(j = 0, kkp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kkp++) {
        if((kkp->ShiftFlags & (K_CTRLFLAG|K_ALTFLAG|LCTRLFLAG|LALTFLAG|RCTRLFLAG|RALTFLAG)) != 0) {
          gp2->cxKeyArray++;
          LPKMX_KEY kkp2 = new KMX_KEY[gp2->cxKeyArray];
          memcpy(kkp2, gp2->dpKeyArray, sizeof(KMX_KEY)*(gp2->cxKeyArray-1));
          gp2->dpKeyArray = kkp2;
          kkp2 = &kkp2[gp2->cxKeyArray-1];
          kkp2->dpContext = new KMX_WCHAR; *kkp2->dpContext = 0;
          kkp2->Key = kkp->Key;
          kkp2->ShiftFlags = kkp->ShiftFlags;
          kkp2->Line = 0;
          KMX_WCHAR *p = kkp2->dpOutput = new KMX_WCHAR[4];
          *p++ = UC_SENTINEL;
          *p++ = CODE_USE;
          *p++ = (KMX_WCHAR)(kp->cxGroupArray);
          *p = 0;
        }
      }
    }
  }

  // _S2 TODO not sure if this works OK -> we need to use deadkeys...
  // If we have deadkeys, then add a new group to translate the deadkeys per the deadkey tables
  // We only do this if not in deadkey conversion mode
  //

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