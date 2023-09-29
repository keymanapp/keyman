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

enum ShiftState {
    Base = 0,                    // 0
    Shft = 1,                    // 1
    Ctrl = 2,                    // 2
    ShftCtrl = Shft | Ctrl,          // 3
    Menu = 4,                    // 4 -- NOT USED
    ShftMenu = Shft | Menu,          // 5 -- NOT USED
    MenuCtrl = Menu | Ctrl,          // 6
    ShftMenuCtrl = Shft | Menu | Ctrl,   // 7
    Xxxx = 8,                    // 8
    ShftXxxx = Shft | Xxxx,          // 9
};
/*
const int ShiftStateMap[] = {
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
   */
class DeadKey {
private:
  WCHAR m_deadchar;
  std::vector<WCHAR> m_rgbasechar;
  std::vector<WCHAR> m_rgcombchar;
/*
public:
  DeadKey(WCHAR deadCharacter) {
    this->m_deadchar = deadCharacter;
  }

  WCHAR DeadCharacter() {
    return this->m_deadchar;
  }

  void AddDeadKeyRow(WCHAR baseCharacter, WCHAR combinedCharacter) {
    this->m_rgbasechar.push_back(baseCharacter);
    this->m_rgcombchar.push_back(combinedCharacter);
  }

  int Count() {
    return this->m_rgbasechar.size();
  }

  WCHAR GetBaseCharacter(int index) {
    return this->m_rgbasechar[index];
  }

  WCHAR GetCombinedCharacter(int index) {
    return this->m_rgcombchar[index];
  }

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
/*

int DeadKeyMap(int index, std::vector<DeadKey *> *deadkeys, int deadkeyBase, std::vector<DeadkeyMapping> *deadkeyMappings) {   // I4327   // I4353
  for(size_t i = 0; i < deadkeyMappings->size(); i++) {
    if((*deadkeyMappings)[i].deadkey == index) {
      return (*deadkeyMappings)[i].dkid;
    }
  }

  for(size_t i = 0; i < deadkeys->size(); i++) {
    if((*deadkeys)[i]->DeadCharacter() == index) {
      return deadkeyBase + i;
    }
  }
  return 0xFFFF;
}
*/
class KMX_VirtualKey {
private:
  KMX_HKL m_hkl;      // _S2 do I need this and is void* OK to assume?
  UINT m_vk;
  UINT m_sc;
  bool m_rgfDeadKey[10][2];
  std::wstring m_rgss[10][2];

public:
// _S2 can be deleted later
  KMX_VirtualKey(KMX_HKL hkl, UINT KMX_virtualKey) {/*
    this->m_sc = MapVirtualKeyEx(virtualKey, 0, hkl);
    this->m_hkl = hkl;
    this->m_vk = KMX_virtualKey;
    memset(this->m_rgfDeadKey,0,sizeof(this->m_rgfDeadKey));*/
  }
  // _S2 can be deleted later
  KMX_VirtualKey(UINT scanCode, KMX_HKL hkl) {
    //    this->m_vk = MapVirtualKeyEx(scanCode, 1, hkl);
    this->m_hkl = hkl;
    this->m_sc = scanCode;
  }


  KMX_VirtualKey(KMX_HKL hkl, UINT KMX_virtualKey, v_dw_3D All_Vector) {/*
    this->m_sc = MapVirtualKeyEx(virtualKey, 0, hkl); // second para =0: MAPVK_VK_TO_VSC=1
                                                        //the uCode parameter is a virtual-key code and is
                                                        //translated into a scan code. If it is a virtual-key
                                                        //code that does not distinguish between left- and
                                                        //right-hand keys, the left-hand scan code is returned.
                                                        //If there is no translation, the function returns 0.*/



    this->m_sc = get_SC_From_VirtualKey_Other(KMX_virtualKey, All_Vector);
    this->m_hkl = hkl;
    this->m_vk = KMX_virtualKey;
    memset(this->m_rgfDeadKey,0,sizeof(this->m_rgfDeadKey));
  }

  KMX_VirtualKey(UINT scanCode, KMX_HKL hkl, v_dw_3D All_Vector) {
    // _S2 this->m_vk = MapVirtualKeyEx(scanCode, 1, hkl);  // second para= 1: MAPVK_VSC_TO_VK =1
    //                                                  The first parameter is a scan code and is
    //                                                  translated into a virtual-key code that does not
    //                                                  distinguish between left- and right-hand keys.
    //                                                  If there is no translation, the function returns 0.
    //                                                  SC -> VK
    this->m_vk = get_VirtualKey_Other_From_SC(scanCode, All_Vector);
    this->m_hkl = hkl;
    this->m_sc = scanCode;
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

/*
  bool IsSGCAPS() {
    std::wstring stBase = this->GetShiftState(Base, false);
    std::wstring stShift = this->GetShiftState(Shft, false);
    std::wstring stCaps = this->GetShiftState(Base, true);
    std::wstring stShiftCaps = this->GetShiftState(Shft, true);
    return (
        ((stCaps.size() > 0) &&
        (stBase.compare(stCaps) != 0) &&
        (stShift.compare(stCaps) != 0)) ||
        ((stShiftCaps.size() > 0) &&
        (stBase.compare(stShiftCaps) != 0) &&
        (stShift.compare(stShiftCaps) != 0)));
  }

  bool IsCapsEqualToShift() {
    std::wstring stBase = this->GetShiftState(Base, false);
    std::wstring stShift = this->GetShiftState(Shft, false);
    std::wstring stCaps = this->GetShiftState(Base, true);
    return (
        (stBase.size() > 0) &&
        (stShift.size() > 0) &&
        (stBase.compare(stShift) != 0) &&
        (stShift.compare(stCaps) == 0));
  }

  bool IsAltGrCapsEqualToAltGrShift() {
    std::wstring stBase = this->GetShiftState(MenuCtrl, false);
    std::wstring stShift = this->GetShiftState(ShftMenuCtrl, false);
    std::wstring stCaps = this->GetShiftState(MenuCtrl, true);
    return (
        (stBase.size() > 0) &&
        (stShift.size() > 0) &&
        (stBase.compare(stShift) != 0) &&
        (stShift.compare(stCaps) == 0));
  }

  bool IsXxxxGrCapsEqualToXxxxShift() {
    std::wstring stBase = this->GetShiftState(Xxxx, false);
    std::wstring stShift = this->GetShiftState(ShftXxxx, false);
    std::wstring stCaps = this->GetShiftState(Xxxx, true);
    return (
        (stBase.size() > 0) &&
        (stShift.size() > 0) &&
        (stBase.compare(stShift) != 0) &&
        (stShift.compare(stCaps) == 0));
  }

  bool IsEmpty() {
    for (int i = 0; i < 10; i++) {
      for (int j = 0; j <= 1; j++) {
        if (this->GetShiftState((ShiftState)i, (j == 1)).size() > 0) {
          return (false);
        }
      }
    }
    return true;
  }
    
  bool IsKeymanUsedKey() {
    return (this->m_vk >= 0x20 && this->m_vk <= 0x5F) || (this->m_vk >= 0x88);
  }

  UINT GetShiftStateValue(int capslock, int caps, ShiftState ss) {
    return 
      ShiftStateMap[(int)ss] |
      (capslock ? (caps ? CAPITALFLAG : NOTCAPITALFLAG) : 0);
  }

  int GetKeyCount(int MaxShiftState) {
    int nkeys = 0;

    // Get the CAPSLOCK value
    int capslock =
        (this->IsCapsEqualToShift() ? 1 : 0) |
        (this->IsSGCAPS() ? 2 : 0) |
        (this->IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);

    for (int ss = 0; ss <= MaxShiftState; ss++) {
      if (ss == Menu || ss == ShftMenu) {
        // Alt and Shift+Alt don't work, so skip them
        continue;
      }
      for (int caps = 0; caps <= 1; caps++) {
        std::wstring st = this->GetShiftState((ShiftState) ss, (caps == 1));

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

  bool LayoutRow(int MaxShiftState, LPKEY key, std::vector<DeadKey*> *deadkeys, int deadkeyBase, BOOL bDeadkeyConversion) {   // I4552
    // Get the CAPSLOCK value
    int capslock =
        (this->IsCapsEqualToShift() ? 1 : 0) |
        (this->IsSGCAPS() ? 2 : 0) |
        (this->IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);

    for (int ss = 0; ss <= MaxShiftState; ss++) {
      if (ss == Menu || ss == ShftMenu) {
        // Alt and Shift+Alt don't work, so skip them
        continue;
      }
      for (int caps = 0; caps <= 1; caps++) {
        std::wstring st = this->GetShiftState((ShiftState) ss, (caps == 1));
        PWSTR p;

        if (st.size() == 0) {
          // No character assigned here
        } else if (this->m_rgfDeadKey[(int)ss][caps]) {
          // It's a dead key, append an @ sign.
          key->dpContext = new WCHAR[1]; 
          *key->dpContext = 0;
          key->ShiftFlags = this->GetShiftStateValue(capslock, caps, (ShiftState) ss);
          key->Key = VKUnderlyingLayoutToVKUS(this->VK());
          key->Line = 0;

          if(bDeadkeyConversion) {   // I4552
            p = key->dpOutput = new WCHAR[2];
            *p++ = st[0];
            *p = 0;
          } else {
            p = key->dpOutput = new WCHAR[4];
            *p++ = UC_SENTINEL;
            *p++ = CODE_DEADKEY;
            *p++ = DeadKeyMap(st[0], deadkeys, deadkeyBase, &FDeadkeys);   // I4353
            *p = 0;
          }
          key++;
        } else {
          bool isvalid = true;
          for (size_t ich = 0; ich < st.size(); ich++) {
            if(st[ich] < 0x20 || st[ich] == 0x7F) { isvalid=false; break; }
          }
            
          if(isvalid) {
            key->Key = VKUnderlyingLayoutToVKUS(this->VK());
            key->Line = 0;
            key->ShiftFlags = this->GetShiftStateValue(capslock, caps, (ShiftState) ss);
            key->dpContext = new WCHAR; *key->dpContext = 0;
            p = key->dpOutput = new WCHAR[st.size() + 1];
            for(size_t ich = 0; ich < st.size(); ich++) *p++ = st[ich];
            *p = 0;
            key++;
          }
        }
      }
    }
    return true;
  }

*/
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
/*
int GetMaxDeadkeyIndex(WCHAR *p) {
  int n = 0;
  while(p && *p) {
    if(*p == UC_SENTINEL && *(p+1) == CODE_DEADKEY)
      n = max(n, *(p+2));
    p = incxstr(p);
  }
  return n;
}
*/


// _S2 has to go !!
bool  write_rgKey_ToFile(std::vector<KMX_VirtualKey*> rgKey ){
  std::string RGKey_FileName="/Projects/keyman/keyman/linux/mcompile/keymap/rgKey_lin.txt";

  std::wofstream TxTFile(RGKey_FileName);
  for ( int i=0; i< rgKey.size();i++) {
    if(rgKey[i] != NULL) {
        TxTFile << rgKey[i]->VK() << "-" << rgKey[i]->SC()<< " -> ( " << rgKey[i]->KMX_GetShiftState(Base, 0) << "-" << rgKey[i]->KMX_GetShiftState(Base, 1) << " )"
        << " *-* ( " << rgKey[i]->KMX_GetShiftState(Shft, 0) << "-" << rgKey[i]->KMX_GetShiftState(Shft, 1) << " )";
        TxTFile << "\n";
    }
  }
  TxTFile.close();
  return true;
}


// _S2 where to put this??
std::wstring  get_VirtualKey_US_from_iKey(KMX_DWORD iKey, ShiftState &ss, int &caps, v_dw_3D &All_Vector) {

  int icaps;
  KMX_DWORD pos = get_position_From_VirtualKey_US(iKey, All_Vector);

  if (ss >9)
    return L"";

  if( ss < All_Vector[0][pos].size()-1) {

    if ( ss % 2 == 0)
      icaps = ss+2-caps;

    if ( ss % 2 == 1)
      icaps = ss+caps;

    return std::wstring(1, (int) All_Vector[0][pos][icaps]);
  }
  return L"";
}

bool KMX_ImportRules(KMX_WCHAR *kbid, LPKMX_KEYBOARD kp,v_dw_3D  &All_Vector,std::vector<KMX_DeadkeyMapping> *FDeadkeys, KMX_BOOL bDeadkeyConversion) {   // I4353   // I4552
 wprintf(L"\n ##### KMX_ImportRules of mc_import_rules started #####\n");
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
  for(UINT sc = 0x01; sc <= 0x7f; sc++) {
    KMX_VirtualKey *key = new KMX_VirtualKey(sc, hkl, All_Vector);      // _S2 get this from my Vector
    uint key_vk = key->VK() ;
   if(key->VK() != 0) {
      rgKey[key->VK()] = key;
    } else {
      delete key;
    }
  }

 /* // where in rgkey do I store Numpad???
  // _S2 do we need NUMPAD now or later? If so we need to add Numpad values to All_Vector ( which has only values a-z)
// _S2 use KMX_VirtualKey !!
  // _S2 do I need NUMPAD + SPECIAL_SHIFT for first draft ??
  // add the special keys that do not get added from the code above
  for(UINT ke = VK_NUMPAD0; ke <= VK_NUMPAD9; ke++) {
      rgKey[ke] = new KMX_VirtualKey(hkl, ke, All_Vector);
  }

  rgKey[VK_DIVIDE] = new KMX_VirtualKey(hkl, VK_DIVIDE, All_Vector);
  rgKey[VK_CANCEL] = new KMX_VirtualKey(hkl, VK_CANCEL, All_Vector);
  rgKey[VK_DECIMAL] = new KMX_VirtualKey(hkl, VK_DECIMAL, All_Vector);
 */

/*  // _S2 do we need special shift state now or later?
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

  // _S2 in this part we skip shiftstates 4, 5, 8, 9
  for(UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if(rgKey[iKey] != NULL) {
      WCHAR sbBuffer[256];     // Scratchpad we use many places

      for(ShiftState ss = Base; ss <= loader.MaxShiftState(); ss = (ShiftState)((int)ss + 1)) {
        if(ss == Menu || ss == ShftMenu) {
          // Alt and Shift+Alt don't work, so skip them
          continue;
        }

        for(int caps = 0; caps <= 1; caps++) {

          //_S2 TODO
          //_S2 get char  - do I need rc ?? ( was rc = ToUnicodeEx...)
          std::wstring VK_US = get_VirtualKey_US_from_iKey(iKey, ss, caps, All_Vector);

          //_S2 TODO
          //do I need that ??
          //if rc >0: it got 1 or more char AND buffer is empty ( nothing inside ) {
            if(VK_US == L"") {
                  rgKey[iKey]->KMX_SetShiftState(ss, L"", false, (caps == 0));
            }

            //_S2 TODO
            //else   // if rc ==1 : it got 1  char && +40 in Buffer CTRl pressed    {
                //It's dealing with control characters. If ToUnicodeEx gets VK_A with the Ctrl key pressed,
                //it will write 0x01 to sBuffer[0] , without Ctrl it's 0x41. The if detects this case.
            if( (ss == Ctrl || ss == ShftCtrl) /*&& CTRl +0x40 in the buffer ( which indicates a ctrl press)   */) {
                continue;
            }

            //_S2 TODO
            // fill m_rgss and m_rgfDeadkey ( m_rgfDeadkey will be done later)
            rgKey[iKey]->KMX_SetShiftState(ss, VK_US, false, (caps == 0));
          //}  // from rc==1
        // } // from rc > 0


        //_S2 TODO
        // _S2 handle deadkeys later
        // if rc <0:  it got a deadkey   {
            // fill m_rgss and m_rgfDeadkey
            //SET_SHIFTSTATES( deadkey)   //sbuffer is value out of ToUnicodeEx / AllVector
            // do more stuff for deadkeys...
        // } from rc<0
        }
      }
    }
  }



  // _S2 can go later: check if all correct
  /*write_rgKey_ToFile(rgKey)  ;
  v_dw_2D  V_lin,V_win,V_map;
  write_RGKEY_FileToVector(V_lin, "rgKey_lin.txt");
  write_RGKEY_FileToVector(V_win, "rgKey_Win.txt");
  write_RGKEY_FileToVector(V_map, "map.txt");
  CompareVector_To_VectorOfFile_RGKEY( V_win, V_lin,V_map);*/



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

int STOP=0;
    /*for(UINT j = 0; j < gp->cxKeyArray; j++, kkp++) {
      nDeadkey = max(nDeadkey, KMX_GetMaxDeadkeyIndex(kkp->dpContext));
      nDeadkey = max(nDeadkey, KMX_GetMaxDeadkeyIndex(kkp->dpOutput));
    }
    */
  }



int STOP2=0;

  /*
  kp->cxGroupArray++;
  gp = &kp->dpGroupArray[kp->cxGroupArray-1];

  UINT nKeys = 0;
  for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if ((rgKey[iKey] != NULL) && rgKey[iKey]->IsKeymanUsedKey() && (!rgKey[iKey]->IsEmpty())) {
      nKeys+= rgKey[iKey]->GetKeyCount(loader.MaxShiftState());
    }
  }

  nDeadkey++; // ensure a 1-based index above the max deadkey value already in the keyboard

  gp->fUsingKeys = TRUE;
  gp->dpMatch = NULL;
  gp->dpName = NULL;
  gp->dpNoMatch = NULL;
  gp->cxKeyArray = nKeys;
  gp->dpKeyArray = new KEY[gp->cxKeyArray];
  nKeys = 0;

  //
  // Fill in the new rules
  //

  for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if ((rgKey[iKey] != NULL) && rgKey[iKey]->IsKeymanUsedKey() && (!rgKey[iKey]->IsEmpty())) {
      // for each item, 
      if(rgKey[iKey]->LayoutRow(loader.MaxShiftState(), &gp->dpKeyArray[nKeys], &alDead, nDeadkey, bDeadkeyConversion)) {   // I4552
        nKeys+=rgKey[iKey]->GetKeyCount(loader.MaxShiftState());
      }
    }
  }

  gp->cxKeyArray = nKeys;

  //
  // Add nomatch control to each terminating 'using keys' group   // I4550
  //
  LPGROUP gp2 = kp->dpGroupArray;
  for(UINT i = 0; i < kp->cxGroupArray - 1; i++, gp2++) {
    if(gp2->fUsingKeys && gp2->dpNoMatch == NULL) {
      WCHAR *p = gp2->dpNoMatch = new WCHAR[4];
      *p++ = UC_SENTINEL;
      *p++ = CODE_USE;
      *p++ = (WCHAR)(kp->cxGroupArray);
      *p = 0;

      //
      // I4550 - Each place we have a nomatch > use(baselayout) (this last group), we need to add all 
      // the AltGr and ShiftAltGr combinations as rules to allow them to be matched as well.  Yes, this
      // loop is not very efficient but it's not worthy of optimisation.
      //
      UINT j;
      LPKEY kkp;
      for(j = 0, kkp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kkp++) {
        if((kkp->ShiftFlags & (K_CTRLFLAG|K_ALTFLAG|LCTRLFLAG|LALTFLAG|RCTRLFLAG|RALTFLAG)) != 0) {
          gp2->cxKeyArray++;
          LPKEY kkp2 = new KEY[gp2->cxKeyArray];
          memcpy(kkp2, gp2->dpKeyArray, sizeof(KEY)*(gp2->cxKeyArray-1));
          gp2->dpKeyArray = kkp2;
          kkp2 = &kkp2[gp2->cxKeyArray-1];
          kkp2->dpContext = new WCHAR; *kkp2->dpContext = 0;
          kkp2->Key = kkp->Key;
          kkp2->ShiftFlags = kkp->ShiftFlags;
          kkp2->Line = 0;
          WCHAR *p = kkp2->dpOutput = new WCHAR[4];
          *p++ = UC_SENTINEL;
          *p++ = CODE_USE;
          *p++ = (WCHAR)(kp->cxGroupArray);
          *p = 0;
        }
      }
    }
  }

  //
  // If we have deadkeys, then add a new group to translate the deadkeys per the deadkey tables
  // We only do this if not in deadkey conversion mode
  //

  if (alDead.size() > 0 && !bDeadkeyConversion) {   // I4552
    kp->cxGroupArray++;

    WCHAR *p = gp->dpMatch = new WCHAR[4];
    *p++ = UC_SENTINEL;
    *p++ = CODE_USE;
    *p++ = (WCHAR) kp->cxGroupArray;
    *p = 0;

    gp++;

    gp->fUsingKeys = FALSE;
    gp->dpMatch = NULL;
    gp->dpName = NULL;
    gp->dpNoMatch = NULL;
    gp->cxKeyArray = alDead.size();
    LPKEY kkp = gp->dpKeyArray = new KEY[alDead.size()];

    LPSTORE sp = new STORE[kp->cxStoreArray + alDead.size() * 2];
    memcpy(sp, kp->dpStoreArray, sizeof(STORE) * kp->cxStoreArray);

    kp->dpStoreArray = sp;

    sp = &sp[kp->cxStoreArray];
    int nStoreBase = kp->cxStoreArray;
    kp->cxStoreArray += alDead.size() * 2;

    for(UINT i = 0; i < alDead.size(); i++) {
      DeadKey *dk = alDead[i];

      sp->dpName = NULL;
      sp->dwSystemID = 0;
      sp->dpString = new WCHAR[dk->Count() + 1];
      for(int j = 0; j < dk->Count(); j++) 
        sp->dpString[j] = dk->GetBaseCharacter(j);
      sp->dpString[dk->Count()] = 0;
      sp++;

      sp->dpName = NULL;
      sp->dwSystemID = 0;
      sp->dpString = new WCHAR[dk->Count() + 1];
      for(int j = 0; j < dk->Count(); j++) 
        sp->dpString[j] = dk->GetCombinedCharacter(j);
      sp->dpString[dk->Count()] = 0;
      sp++;

      kkp->Line = 0;
      kkp->ShiftFlags = 0;
      kkp->Key = 0;
      WCHAR *p = kkp->dpContext = new WCHAR[8];
      *p++ = UC_SENTINEL;
      *p++ = CODE_DEADKEY;
      *p++ = DeadKeyMap(dk->DeadCharacter(), &alDead, nDeadkey, FDeadkeys);   // I4353
      // *p++ = nDeadkey+i;
      *p++ = UC_SENTINEL;
      *p++ = CODE_ANY;
      *p++ = nStoreBase + i*2 + 1;
      *p = 0;

      p = kkp->dpOutput = new WCHAR[5];
      *p++ = UC_SENTINEL;
      *p++ = CODE_INDEX;
      *p++ = nStoreBase + i*2 + 2;
      *p++ = 2;
      *p = 0;

      kkp++;
    }
  }
*/
wprintf(L"\n ##### KMX_ImportRules of mc_import_rules ended #####\n");
return true;
}
