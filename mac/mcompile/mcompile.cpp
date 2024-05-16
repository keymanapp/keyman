/*
  Name:             mcompile
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      24 Apr 2014

  Modified Date:    8 Apr 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          24 Apr 2014 - mcdurdin - I4174 - V9 - mcompile logs should be stored in diag folder
                    16 Jun 2014 - mcdurdin - I4273 - V9.0 - Convert keyboards to Unicode before installing
                    23 Jun 2014 - mcdurdin - I4279 - V9.0 - mcompile fails to start when converting keyboard to Unicode
                    03 Aug 2014 - mcdurdin - I4353 - V9.0 - mnemonic layout recompiler mixes up deadkey rules
                    03 Aug 2014 - mcdurdin - I4327 - V9.0 - Mnemonic layout compiler follow-up
                    31 Dec 2014 - mcdurdin - I4549 - V9.0 - Mnemonic layout recompiler does not translate Lctrl Ralt for deadkeys correctly
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
                    08 Apr 2015 - mcdurdin - I4651 - V9.0 - Mnemonic layout recompiler maps AltGr+VK_BKSLASH rather than VK_OEM_102
*/

#include <stdio.h>
#include <wchar.h>
#include "mcompile.h"
#include "u16.h"


KMX_BOOL mac_KMX_DoConvert(LPKMX_KEYBOARD kbd, KMX_BOOL bDeadkeyConversion, int argc, char *argv[]);

bool mac_KMX_ImportRules( LPKMX_KEYBOARD kp,v_dw_3D &All_Vector, const UCKeyboardLayout **keyboard_layout,std::vector<KMX_DeadkeyMapping> *KMX_FDeadkeys, KMX_BOOL bDeadkeyConversion); // I4353   // I4327

std::vector<KMX_DeadkeyMapping> KMX_FDeadkeys; // I4353

// Note: max is not a standard c api function or macro
#ifndef max
#define max(a,b)            (((a) > (b)) ? (a) : (b))
#endif

#define _countof(a) (sizeof(a)/sizeof(*(a)))

#if defined(_WIN32) || defined(_WIN64)
  int wmain(int argc, wchar_t* argv[]) {
    std::vector<std::u16string> str_argv_16 = convert_argvW_to_Vector_u16str( argc, argv);
    run(argc, str_argv_16);

#elif ((__linux__) ||  (__unix__ )) // LINUX, UNIX
  int main(int argc, char* argv[]) {
    std::vector<std::u16string> str_argv_16 = convert_argv_to_Vector_u16str(argc, argv);
    run(argc, str_argv_16, argv);

#elif (__APPLE__ && TARGET_OS_MAC)
  #include <TargetConditionals.h>

  int main(int argc, char* argv[]) {
  std::vector<std::u16string> str_argv_16 = convert_argv_to_Vector_u16str(argc, argv);


  mac_run(argc, str_argv_16, argv);
  printf("\n................................ END ..............................\n");


#endif
}

int mac_run(int argc, std::vector<std::u16string> str_argv, char* argv_ch[] = NULL){

  std::vector<const char16_t*> argv;
  for (int i = 0; i < argc; i++) {
    const char16_t* cmdl_par = str_argv[i].c_str();
    argv.push_back(cmdl_par);
  }

  if(argc < 3 || (argc > 4)) {   // I4273// I4273
    wprintf(
        L"Usage: mcompile [-d] infile.kmx outfile.kmx\n"
        L"       mmcompile -u ...  (not available for mac)\n "
        L"      mcompile converts a Keyman mnemonic layout to a\n"
        L"       positional one based on the mac keyboard\n"
        L"       layout on top position\n"
        L"       (-d   convert deadkeys to plain keys) not available yet \n\n"
        );   // I4552

    return 1;
  }
  // -u option is not available for Linux and macOS

  int bDeadkeyConversion = u16cmp(argv[1], u"-d") == 0; // I4552
  int n = (bDeadkeyConversion ? 2 : 1);

  char16_t* infile = (char16_t*) argv[n], *outfile = (char16_t*) argv[n+1];

  wprintf(L"mcompile%ls \"%ls\" \"%ls\"\n", bDeadkeyConversion ? L" -d" : L"", u16fmt((const char16_t*) infile).c_str(), u16fmt((const char16_t*) outfile).c_str() ); // I4174

  // 1. Load the keyman keyboard file

  // 2. For each key on the system layout, determine its output character and perform a
  //    1-1 replacement on the keyman keyboard of that character with the base VK + shift
  //    state.  This fixup will transform the char to a vk, which will avoid any issues
  //    with the key.
  //
  //  --> deadkeys we will attack after the POC
  //
  //  For each deadkey, we need to determine its possible outputs.  Then we generate a VK
  //  rule for that deadkey, e.g. [K_LBRKT] > dk(c101)
  //
  //  Next, update each rule that references the output from that deadkey to add an extra
  //  context deadkey at the end of the context match, e.g. 'a' dk(c101) + [K_SPACE] > 'b'.
  //  This will require a memory layout change for the .kmx file, plus fixups on the
  //  context+output index offsets
  //
  //  --> virtual character keys
  //
  //  [CTRL ' '] : we look at the character, and replace it in the same way, but merely
  //  switch the shift state from the VIRTUALCHARKEY to VIRTUALKEY, without changing any
  //  other properties of the key.
  //
  // 3. Write the new keyman keyboard file


  LPKMX_KEYBOARD kmxfile;

  if(!KMX_LoadKeyboard(infile, &kmxfile)) {
    mac_KMX_LogError(L"Failed to load keyboard (%d)\n", errno );
    return 3;
  }


  #if defined(_WIN32) || defined(_WIN64)  //Windows
    if(DoConvert(kmxfile, kbid, bDeadkeyConversion)) {   // I4552F
      KMX_SaveKeyboard(kmxfile, outfile);
    }

  #elif ((__linux__) ||  (__unix__ )) // LINUX, UNIX
    if(mac_KMX_DoConvert( kmxfile, bDeadkeyConversion, argc, (gchar**) argv_ch)) { // I4552F
      KMX_SaveKeyboard(kmxfile, outfile);
  }

  #elif (__APPLE__ && TARGET_OS_MAC  ) //macOS
    if(mac_KMX_DoConvert( kmxfile, bDeadkeyConversion, argc, (char**) argv_ch)) { // I4552F
      KMX_SaveKeyboard(kmxfile, outfile);
  }
  #endif

  //DeleteReallocatedPointers(kmxfile); :TODO   // _S2 not my ToDo :-)
  delete kmxfile;
  return 0;
}


//################################################################################################################################################
//################################# Code beyond these lines needs to be included in mcompile #####################################################
//################################################################################################################################################

// Map of all shift states that we will work with
const UINT VKShiftState[] = {0, K_SHIFTFLAG, LCTRLFLAG|RALTFLAG, K_SHIFTFLAG|LCTRLFLAG|RALTFLAG, 0xFFFF};

//
// TranslateKey
//
// For each key rule on the keyboard, remap its key to the
// correct shift state and key.  Adjust the LCTRL+RALT -> RALT if necessary
//
void mac_KMX_TranslateKey(LPKMX_KEY key, KMX_WORD vk, UINT shift, KMX_WCHAR ch) {
  //_S2 do we translate here or is this the reason for Shift + K_A  <-> Shift 'a' ???
  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
   if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))
    shift &= ~LCTRLFLAG;

  if(key->ShiftFlags == 0 && key->Key == ch) {
    // Key is a mnemonic key with no shift state defined.
    // Remap the key according to the character on the key cap.
    mac_KMX_LogError(L"Converted mnemonic rule on line %d, + '%c' TO + [%x K_%d]", key->Line, key->Key, shift, vk);// 1
    key->ShiftFlags = ISVIRTUALKEY | shift;
    key->Key = vk;
  } else if(key->ShiftFlags & VIRTUALCHARKEY && key->Key == ch) {
    // Key is a virtual character key with a hard-coded shift state.
    // Do not remap the shift state, just move the key.
    // This will not result in 100% wonderful mappings as there could
    // be overlap, depending on how keys are arranged on the target layout.
    // But that is up to the designer.
    mac_KMX_LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
    key->ShiftFlags &= ~VIRTUALCHARKEY;
    key->Key = vk;
  }
}

void mac_KMX_TranslateGroup(LPKMX_GROUP group, KMX_WORD vk, UINT shift, KMX_WCHAR ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    mac_KMX_TranslateKey(&group->dpKeyArray[i], vk, shift, ch);
  }
}

void mac_KMX_TranslateKeyboard(LPKMX_KEYBOARD kbd, KMX_WORD vk, UINT shift, KMX_WCHAR ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      mac_KMX_TranslateGroup(&kbd->dpGroupArray[i], vk, shift, ch);
    }
  }
}

void mac_KMX_ReportUnconvertedKeyRule(LPKMX_KEY key) {
  if(key->ShiftFlags == 0) {
    mac_KMX_LogError(L"Did not find a match for mnemonic rule on line %d, + '%c' > ...", key->Line, key->Key);
  } else if(key->ShiftFlags & VIRTUALCHARKEY) {
    mac_KMX_LogError(L"Did not find a match for mnemonic virtual character key rule on line %d, + [%x '%c'] > ...", key->Line, key->ShiftFlags, key->Key);
  }
}

void mac_KMX_ReportUnconvertedGroupRules(LPKMX_GROUP group) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    mac_KMX_ReportUnconvertedKeyRule(&group->dpKeyArray[i]);
  }
}

void mac_KMX_ReportUnconvertedKeyboardRules(LPKMX_KEYBOARD kbd) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      mac_KMX_ReportUnconvertedGroupRules(&kbd->dpGroupArray[i]);
    }
  }
}

void mac_KMX_TranslateDeadkeyKey(LPKMX_KEY key, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch) {
// _S2 TOP_2 INFO this produces  a different output due to different layouts for Lin<-> win ( for the same language!!)

   if((key->ShiftFlags == 0 || key->ShiftFlags & VIRTUALCHARKEY) && key->Key == ch) {
    // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
    // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
    // to provide an alternate..
    if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))   // I4327
      shift &= ~LCTRLFLAG;

    if(key->ShiftFlags == 0) {
      mac_KMX_LogError(L"Converted mnemonic rule on line %d, + '%c' TO dk(%d) + [%x K_%d]", key->Line, key->Key, deadkey, shift, vk);
      key->ShiftFlags = ISVIRTUALKEY | shift;
    } else {
      mac_KMX_LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO dk(%d) + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, deadkey, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
      key->ShiftFlags &= ~VIRTUALCHARKEY;
    }

    int len = u16len(key->dpContext);

    PKMX_WCHAR context = new KMX_WCHAR[len + 4];
    memcpy(context, key->dpContext, len * sizeof(KMX_WCHAR));
    context[len] = UC_SENTINEL;
    context[len+1] = CODE_DEADKEY;
    context[len+2] = deadkey;
    context[len+3] = 0;
    key->dpContext = context;
    key->Key = vk;
  }
}

void mac_KMX_TranslateDeadkeyGroup(LPKMX_GROUP group,KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    mac_KMX_TranslateDeadkeyKey(&group->dpKeyArray[i], deadkey, vk, shift, ch);
  }
}

void mac_KMX_TranslateDeadkeyKeyboard(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      mac_KMX_TranslateDeadkeyGroup(&kbd->dpGroupArray[i], deadkey, vk, shift, ch);
    }
  }
}

void mac_KMX_AddDeadkeyRule(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift) {
  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG)) // I4549
    shift &= ~LCTRLFLAG;
  // If the first group is not a matching-keys group, then we need to add into
  // each subgroup, otherwise just the match group
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      LPKMX_KEY keys = new KMX_KEY[kbd->dpGroupArray[i].cxKeyArray + 1];
      memcpy(keys+1, kbd->dpGroupArray[i].dpKeyArray, kbd->dpGroupArray[i].cxKeyArray * sizeof(KMX_KEY));
      keys[0].dpContext = new KMX_WCHAR[1];
      keys[0].dpContext[0] = 0;
      keys[0].dpOutput = new KMX_WCHAR[4];
      keys[0].dpOutput[0] = UC_SENTINEL;
      keys[0].dpOutput[1] = CODE_DEADKEY;
      keys[0].dpOutput[2] = deadkey; // TODO: translate to unique index
      keys[0].dpOutput[3] = 0;
      keys[0].Key = vk;
      keys[0].Line = 0;
      keys[0].ShiftFlags = shift | ISVIRTUALKEY;
      kbd->dpGroupArray[i].dpKeyArray = keys;
      kbd->dpGroupArray[i].cxKeyArray++;
      mac_KMX_LogError(L"Add deadkey rule:  + [%d K_%d] > dk(%d)", shift, vk, deadkey);
      if(i == kbd->StartGroup[1]) break;  // If this is the initial group, that's all we need to do.
    }
  }
}

KMX_WCHAR mac_KMX_ScanXStringForMaxDeadkeyID(PKMX_WCHAR str) {
  KMX_WCHAR dkid = 0;
  while(str && *str) {
    if(*str == UC_SENTINEL) {
      switch(*(str+1)) {
      case CODE_DEADKEY:
        dkid = max(dkid, *(str+2));
      }
    }
    str = KMX_incxstr(str);
  }
  return dkid;
}

struct KMX_dkidmap {
  KMX_WCHAR src_deadkey, dst_deadkey;
};

KMX_WCHAR mac_KMX_GetUniqueDeadkeyID(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey) {
  LPKMX_GROUP gp;
  LPKMX_KEY kp;
  LPKMX_STORE sp;
  UINT i, j;
  KMX_WCHAR dkid = 0;
  static KMX_WCHAR s_next_dkid = 0;
  static KMX_dkidmap *s_dkids = NULL;
  static int s_ndkids = 0;

  if(!kbd) {
    if(s_dkids) {
      delete s_dkids;
    }
    s_dkids = NULL;
    s_ndkids = 0;
    s_next_dkid = 0;
    return 0;
  }

  for(int i = 0; i < s_ndkids; i++) {
    if(s_dkids[i].src_deadkey == deadkey) {
      return s_dkids[i].dst_deadkey;
    }
  }

  if(s_next_dkid != 0) {
    s_dkids = (KMX_dkidmap*) realloc(s_dkids, sizeof(KMX_dkidmap) * (s_ndkids+1));
    s_dkids[s_ndkids].src_deadkey = deadkey;
    return s_dkids[s_ndkids++].dst_deadkey = ++s_next_dkid;
  }

  for(i = 0, gp = kbd->dpGroupArray; i < kbd->cxGroupArray; i++, gp++) {
    for(j = 0, kp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kp++) {
      dkid = max(dkid, mac_KMX_ScanXStringForMaxDeadkeyID(kp->dpContext));
      dkid = max(dkid, mac_KMX_ScanXStringForMaxDeadkeyID(kp->dpOutput));
    }
    dkid = max(dkid, mac_KMX_ScanXStringForMaxDeadkeyID(gp->dpMatch));
    dkid = max(dkid, mac_KMX_ScanXStringForMaxDeadkeyID(gp->dpNoMatch));
  }

  for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    dkid = max(dkid, mac_KMX_ScanXStringForMaxDeadkeyID(sp->dpString));
  }

  s_dkids = (KMX_dkidmap*) realloc(s_dkids, sizeof(KMX_dkidmap) * (s_ndkids+1));
  s_dkids[s_ndkids].src_deadkey = deadkey;
  return s_dkids[s_ndkids++].dst_deadkey = s_next_dkid = ++dkid;
}

void mac_KMX_ConvertDeadkey(LPKMX_KEYBOARD kbd, KMX_WORD vk_US, UINT shift, KMX_WCHAR deadkey, v_dw_3D &All_Vector, const UCKeyboardLayout * keyboard_layout,v_dw_2D dk_Table) {
  KMX_WORD deadkeys[512]={0};
  KMX_WORD *pdk;

  // Lookup the deadkey table for the deadkey in the physical keyboard
  // Then for each character, go through and map it through
  KMX_WCHAR dkid = mac_KMX_GetUniqueDeadkeyID(kbd, deadkey);

  // Add the deadkey to the mapping table for use in the import rules phase
  KMX_DeadkeyMapping KMX_deadkeyMapping = { deadkey, dkid, shift, vk_US};    // I4353

  KMX_FDeadkeys.push_back(KMX_deadkeyMapping); //dkid, vk, shift);   // I4353
  mac_KMX_AddDeadkeyRule(kbd, dkid, vk_US, shift);

  // creates array [a,e,i,o,u][shiftstate][combined for a+dk e.g. â  ]
  mac_KMX_GetDeadkeys( deadkey, shift, pdk = deadkeys, All_Vector, keyboard_layout);  // returns array of [usvk, ch_out] pairs

  //KMX_WORD deadkeys1[512]={0};
  //KMX_WORD *pdk1;
  //bool tt_S2= test_dk_S2(deadkeys, deadkeys1);
  /*bool uu_S2= test_dk_find_entries_S2(deadkeys, 75);
  bool uu_S3= test_dk_find_entries_S2(deadkeys, 226);
  bool uu_S4= test_dk_find_entries_S2(deadkeys, 8);
  bool uu_S5= test_dk_find_entries_S2(deadkeys, 214);*/
  bool uu_S6= test_dk_write_entries_S2(deadkeys);

  while(*pdk) {
    // Look up the ch
    UINT KeyValUnderlying = mac_KMX_get_KeyValUnderlying_From_KeyValUS(All_Vector, *pdk);
    mac_KMX_TranslateDeadkeyKeyboard(kbd, dkid, KeyValUnderlying, *(pdk+1), *(pdk+2));
    pdk+=3;
  }
}

KMX_BOOL mac_KMX_SetKeyboardToPositional(LPKMX_KEYBOARD kbd) {
  LPKMX_STORE sp;
  UINT i;
  for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    if(sp->dwSystemID == TSS_MNEMONIC) {
      if(!sp->dpString) {
        mac_KMX_LogError(L"Invalid &mnemoniclayout system store");
        return FALSE;
      }
      if(u16cmp((const KMX_WCHAR*)sp->dpString, u"1") != 0) {
        mac_KMX_LogError(L"Keyboard is not a mnemonic layout keyboard");
        return FALSE;
      }
      *sp->dpString = '0';
      return TRUE;
    }
  }
  mac_KMX_LogError(L"Keyboard is not a mnemonic layout keyboard");
  return FALSE;
}

KMX_BOOL mac_KMX_DoConvert(LPKMX_KEYBOARD kbd, KMX_BOOL bDeadkeyConversion, int argc, char *argv[]) {

  KMX_WCHAR DeadKey=0;

  if(!mac_KMX_SetKeyboardToPositional(kbd)) return FALSE;

  // Go through each of the shift states - base, shift, ctrl+alt, ctrl+alt+shift, [caps vs ncaps?]
  // Currently, we go in this order so the 102nd key works. But this is not ideal for keyboards without 102nd key:   // I4651
  // it catches only the first key that matches a given rule, but multiple keys may match that rule. This is particularly
  // evident for the 102nd key on UK, for example, where \ can be generated with VK_OEM_102 or AltGr+VK_QUOTE.
  // For now, we get the least shifted version, which is hopefully adequate.

  // _S2 : Sadly it`s not: on a german WINDOWS keyboard one will get '~' with  ALTGR + K_221(+) only.
  //                       on a german MAC keyboard one will get '~' with either OPT + K_221(+) or OPT + K_84(T).
  // K_84 will be caught first, so the least obvious version for creating the '~' is found and processed.

  const UCKeyboardLayout* keyboard_layout;
  if(mac_InitializeUCHR(&keyboard_layout)) {
      wprintf(L"ERROR: can't Initialize GDK\n");
      return FALSE;
  }
  // create vector that contains Keycode, base, shift for US-Keyboard and underlying keyboard
  v_dw_3D All_Vector;
  if(mac_createOneVectorFromBothKeyboards(All_Vector, keyboard_layout)){
    wprintf(L"ERROR: can't create one vector from both keyboards\n");
    return FALSE;
  }

  // _S2 printoutKeyboards(All_Vector);

  // _S2 ToDo
  v_dw_2D  dk_Table;
  mac_create_DKTable(dk_Table);

  // _S2 which shiftstates??
  for (int j = 0; VKShiftState[j] != 0xFFFF; j++) { // I4651

    // Loop through each possible key on the keyboard
    for (int i = 0;KMX_VKMap[i]; i++) { // I4651

      // windows uses VK, Linux and macOS use SC/Keycode
      // _S2 ToDo unused vk`s ??
      UINT scUnderlying =  mac_KMX_get_KeyCodeUnderlying_From_VKUS(KMX_VKMap[i]);
      KMX_WCHAR ch = mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keyboard_layout, VKShiftState[j], scUnderlying, &DeadKey);

      //wprintf(L"--- VK_%d -> SC_ [%c] dk=%d  ( ss %i) \n", KMX_VKMap[i], ch == 0 ? 32 : ch, DeadKey, VKShiftState[j]);

      if(bDeadkeyConversion) {   // I4552
        if(ch == 0xFFFF) {
          ch = DeadKey;
        }
      }
      switch(ch) {
        case 0x0000: break;
        case 0xFFFF: mac_KMX_ConvertDeadkey(kbd, KMX_VKMap[i], VKShiftState[j], DeadKey, All_Vector, keyboard_layout , dk_Table); break;
        default: mac_KMX_TranslateKeyboard(kbd, KMX_VKMap[i], VKShiftState[j], ch);
      }
    }
  }
  // _S2 ToDo non-dk OK,shifted dk are not
  mac_KMX_ReportUnconvertedKeyboardRules(kbd);

  /*// _S2 has to go later
  KMX_DWORD out = X_find_Shiftstates_S2(0, keyboard_layout,12) ;
  //KMX_DWORD out2= X_find_Shiftstates_S2(0, keyboard_layout,0) ;
  //KMX_DWORD out4= X_compare_Shiftstates_S2(0, keyboard_layout,0) ;
  //KMX_DWORD out3 =  printout_dk(keyboard_layout);
  //KMX_DWORD  out5= X_playWithDK_S2(0, keyboard_layout,0) ;
  KMX_DWORD  out6;
  for ( int i= 0; i<maxKeyCodeMac;i++) {
    out6= X_playWithDK_S2_one(i, keyboard_layout,8) ;
    printf( " key-nr : %i gives char: %i(%c)\n", i, out6,out6);
  }
  for ( int i= 0; i<16;i++) {
    out6= X_playWithDK_S2_one(24, keyboard_layout,i) ;
    printf( " key-nr : 24 ss %i gives char: %i(%c)\n",i,  out6,out6);
  }*/

  if(!mac_KMX_ImportRules(kbd, All_Vector, &keyboard_layout, &KMX_FDeadkeys, bDeadkeyConversion)) {   // I4353   // I4552
    return FALSE;
  }

  return TRUE;
}

int mac_KMX_GetDeadkeys( KMX_WCHAR DeadKey, UINT shift_dk, KMX_WORD *OutputPairs, v_dw_3D &All_Vector, const UCKeyboardLayout * keyboard_layout) {

  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  UniChar unicodeString[maxStringlength];
  KMX_WORD *p = OutputPairs;
  UInt32 deadkeystate = 0;
  OSStatus status;
  KMX_WORD shift[4] ={0,2,8,10};

  KMX_DWORD sc_dk = mac_KMX_get_KeyCodeUnderlying_From_KeyValUnderlying( All_Vector, (KMX_DWORD) DeadKey);

  /*// _S2
  // we go in this order because we need to make sure that the most obvious keycombination is taken into account for deadkeys.
  // This is important since it catches only the first key that matches a given rule, but multiple keys may match that rule.
  // (see explanation in mcompile.cpp DoConvert() )
  // space is the most important one for deadkeys, so we start with Space (Keycode 49)
  // then character keys in alphabetical order, then numbers, then punctuation keys

  // SPACE, A_Z, 0-9, VK_ACCENT,HYPHEN,EQUAL,LBRKT,RBRKT,BKSLASH,COLON,QUOTE,COMMA,PERIOD,SLASH,xDF,OEM_102
  int keyarray[] ={
      49,
      0,  11,   8,   2,  14,   3,   5,   4,  34,  38,  40,  37,  46,  45,  31,  35,  12,  15,   1,  17,  32,   9,  13,   7,  16,   6,
      29,  18,  19,  20,  21,  23,  22,  26,  28,  25,
      42,  27,  24,  33,  30,  42,  41,  39,  43,  47,  44, 223, 226, 0xFFFF
  };*/

  for ( int j=0; j < sizeof(shift)/sizeof(shift[0]); j++) {
    // we start calculating SPACE(49) since most obvious deadkeys combinations use space.
    for ( int i=maxKeyCodeMac-1; i >=0; i--) {
    // _S2 for (int i = 0; keyarray[i] != 0xFFFF; i++) {
      status = UCKeyTranslate(keyboard_layout, sc_dk ,kUCKeyActionDown, mac_map_VKShiftState_to_MacModifier(shift_dk), LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
      if(deadkeystate !=0) {
        status = UCKeyTranslate(keyboard_layout, i ,kUCKeyActionDown, shift[j], LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
        // _S2 status = UCKeyTranslate(keyboard_layout, keyarray[i] ,kUCKeyActionDown, shift[j], LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
        if(deadkeystate !=0) {
          KMX_WORD vk = mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keyboard_layout, i, 1);
          // _S2 KMX_WORD vk = mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keyboard_layout, keyarray[i], 1);
          // _S2 to not get combinations like ^a but only combined characters like â ( exception for ^+space)
          if((unicodeString[0] != DeadKey) || (vk == 32)) {
            *p++ = vk;
            *p++ = shift[j];
            *p++ =unicodeString[0];
          }
        }
      }
    }
  }
  *p = 0;
  return (p-OutputPairs);
}

void mac_KMX_LogError(const wchar_t* fmt, ...) {
  WCHAR fmtbuf[256];
  const wchar_t* end = L"\0";
  const wchar_t* nl  = L"\n";
	va_list vars;
  int j=0;

	va_start(vars, fmt);
  vswprintf (fmtbuf,_countof(fmtbuf), fmt,  vars );
	fmtbuf[255] = 0;

  do {
    putwchar(fmtbuf[j]);
    j++;
  }
  while(fmtbuf[j] != *end);
  putwchar(*nl);

}





//################################################################################################################################################
//################################################################################################################################################


bool test_dk_S2(KMX_WORD deadkeys[512], KMX_WORD deadkeys1[512]) {
  bool tt= true;
  KMX_DWORD val,  val1;
  for ( int i=0; i<512;i++) {
    val = deadkeys[i];
    val1 = deadkeys1[i];
    tt=  ( (deadkeys[i]   ==  deadkeys1[i])   && tt);
    if (!(tt))
    printf("wrong from %i \n",i);
  }
  return tt;
}

bool test_dk_find_entries_S2(KMX_WORD deadkeys[512], int search) {
  for ( int i=0; i<512;i++) {
    if (deadkeys[i] == search) {
      printf("found value %i (first occurance) in deadkeys[%i]  ",search, i);
      if( i%3 ==0 )           printf("as character \n");
      if( i%3 ==1 )           printf("as shiftstate \n");
      if( i%3 ==2 )           printf("as combined character \n");
      return true;
    }
  }
  return false; 
}

bool test_dk_write_entries_S2(KMX_WORD deadkeys[512]) {
  for ( int i=0; i< 512/3;i++) {
   if ( deadkeys[3*i] !=0)
      printf(" %i set nr \t%i:  %i\t-\t%i(%c)\t-\t%i(%c)  \n",deadkeys[2] ,i,deadkeys[3*i+1], deadkeys[3*i],deadkeys[3*i],deadkeys[3*i+2],deadkeys[3*i+2]);
  }
  return true;
}

//--------------------------
/*void fun2() {  std::cout << "Hier ist fun2 von mcompile.cpp ...\n";}

void testmyFunctions_S2() {
  std::u16string character;
  KMX_DWORD val;

  for (int i=0; i<8; i++) {
    character  = mac_get_character_From_Keycode(24,14,i );  // all Shiftstates for ´` + e
    val  = mac_get_keyval_From_Keycode(24,14,i );  // all Shiftstates for ´` + e
    wprintf( L" ` + e   for SHIFTSTATE %i -> %i \n", i, val);
  }
}*/

