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
//_S2
// in /Projects/keyman/keyman/linux/mcompile/keymap/build_mcompile
// run with:
//./mcompile -d in.kmx bla.dll 0407 out.kmx
//./mcompile -d /Projects/keyman/keyman/linux/mcompile/keymap/anii.kmx bla.dll 0407 /Projects/keyman/keyman/linux/mcompile/keymap/anii_out.kmx
//./mcompile -d /Projects/keyman/keyman/linux/mcompile/keymap/sil_ipa_o.kmx bla.dll 0407 /Projects/keyman/keyman/linux/mcompile/keymap/sil_ipa_o_out2.kmx
//./mcompile -d /Projects/keyman/keyman/linux/mcompile/keymap/mcompile_test.kmx bla.dll 0407 /Projects/keyman/keyman/linux/mcompile/keymap/mcompile_test_out.kmx

#include "mcompile.h"

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

// _S2 can go later
void  test_keyboard_S2(LPKMX_KEYBOARD kmxfile);
void TestKey_S2(LPKMX_KEY key) ;
void TestGroup_S2(LPKMX_GROUP group) ;
void TestKeyboard_S2(LPKMX_KEYBOARD kbd) ;


KMX_BOOL KMX_DoConvert(LPKMX_KEYBOARD kbd, KMX_BOOL bDeadkeyConversion, gint argc, gchar *argv[]);

bool KMX_ImportRules( LPKMX_KEYBOARD kp,v_dw_3D &All_Vector, GdkKeymap **keymap,std::vector<KMX_DeadkeyMapping> *KMX_FDeadkeys, KMX_BOOL bDeadkeyConversion); // I4353   // I4327

std::vector<KMX_DeadkeyMapping> KMX_FDeadkeys; // I4353

// Note: max is not a standard c api function or macro
#ifndef max
#define max(a,b)            (((a) > (b)) ? (a) : (b))
#endif

#if defined(_WIN32) || defined(_WIN64)
  int wmain(int argc, wchar_t* argv[]) {
    std::vector<std::u16string> str_argv_16 = convert_argvW_to_Vector_u16str( argc, argv);
    run(argc, str_argv_16);

#else  // LINUX
  int main(int argc, char* argv[]) {
    std::vector<std::u16string> str_argv_16 = convert_argv_to_Vector_u16str(argc, argv);
    run(argc, str_argv_16, argv);
#endif

}

int run(int argc, std::vector<std::u16string> str_argv, char* argv_ch[] = NULL){

  // convert std::vector<std::u16string> to std::vector<const char16_t*>
  std::vector<const char16_t*> argv;
  for (int i = 0; i < argc; i++) {
    const char16_t* cmdl_par = str_argv[i].c_str();
    argv.push_back(cmdl_par);
  }

  if(argc < 3 || (argc > 4)) {   // I4273// I4273
    wprintf(
        L"Usage: mcompile [-d] infile.kmx outfile.kmx\n"
        L"       mmcompile -u ...  (not available for Linux)\n "
        L"      mcompile converts a Keyman mnemonic layout to a\n"
        L"       positional one based on the Linux keyboard\n"
        L"       layout on top position\n"
        L"       (-d   convert deadkeys to plain keys) not available yet \n\n"

        );   // I4552

    return 1;
  }
  //  -u option was removed for Linux

  int bDeadkeyConversion = u16cmp(argv[1], u"-d") == 0; // I4552
  int n = (bDeadkeyConversion ? 2 : 1);

  char16_t* infile = (char16_t*) argv[n], *outfile = (char16_t*) argv[n+1];

  wprintf(L"mcompile%ls \"%ls\" \"%ls\"\n", bDeadkeyConversion ? L" -d" : L"", u16fmt((const char16_t*) infile).c_str(),   u16fmt((const char16_t*) outfile).c_str() ); // I4174

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
    KMX_LogError(L"Failed to load keyboard (%d)\n", errno );
    return 3;
  }


#if defined(_WIN32) || defined(_WIN64)
  // _S2 DoConvert for windows needs to be done later ( can it be copied from engine/mcompile ?)
  /*if(DoConvert(kmxfile, kbid, bDeadkeyConversion)) {   // I4552F
      KMX_SaveKeyboard(kmxfile, outfile);
  }*/

#else  // LINUX
  if(KMX_DoConvert( kmxfile, bDeadkeyConversion, argc, (gchar**) argv_ch)) { // I4552F
    KMX_SaveKeyboard(kmxfile, outfile);
}

#endif

  //DeleteReallocatedPointers(kmxfile); :TODO   // _S2 not my ToDo :-)
  delete kmxfile;

  wprintf(L"\nmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm end\n");
  wprintf(L"\n");
  return 0;
}

// Map of all shift states that we will work with
const UINT VKShiftState[] = {0, K_SHIFTFLAG, LCTRLFLAG|RALTFLAG, K_SHIFTFLAG|LCTRLFLAG|RALTFLAG, 0xFFFF};

// my comment for Lin version
// Ubuntu:  Each of the 4 columns specifies a different modifier:  unmodified,  shift,   right alt (altgr),     shift+right alt(altgr)
// some hold up to 8 what are those ???
// we have assigned these to columns 1-4  ( column o holds the keycode)
//const UINT VKShiftState[] = {0, 1,  2,  3, 0xFFFF};

//
// TranslateKey
//
// For each key rule on the keyboard, remap its key to the
// correct shift state and key.  Adjust the LCTRL+RALT -> RALT if necessary
//
void KMX_TranslateKey(LPKMX_KEY key, KMX_WORD vk, KMX_UINT shift, KMX_WCHAR ch) {
  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))
    shift &= ~LCTRLFLAG;

  if(key->ShiftFlags == 0 && key->Key == ch) {
    // Key is a mnemonic key with no shift state defined.
    // Remap the key according to the character on the key cap.
    //LogError(L"Converted mnemonic rule on line %d, + '%c' TO + [%x K_%d]", key->Line, key->Key, shift, vk);
    key->ShiftFlags = ISVIRTUALKEY | shift;
    key->Key = vk;
    //wprintf(L"ISVIRTUALKEY: %i , shift: %i, key->ShiftFlags for mnemonic=  %i\n", ISVIRTUALKEY,shift, key->ShiftFlags);
    //wprintf(L"     1 and changed, %i (%c)  %i (%c) ", ch,ch, vk,vk);
  } else if(key->ShiftFlags & VIRTUALCHARKEY && key->Key == ch) {
    // Key is a virtual character key with a hard-coded shift state.
    // Do not remap the shift state, just move the key.
    // This will not result in 100% wonderful mappings as there could
    // be overlap, depending on how keys are arranged on the target layout.
    // But that is up to the designer.
    //LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
    key->ShiftFlags &= ~VIRTUALCHARKEY;
    //wprintf(L" i am processing vk%i (%c) char %i (%c) \n", vk,vk, ch,ch);
    key->Key = vk;
    //wprintf(L"key->ShiftFlags for VIRTUALCHARKEY=  %i", key->ShiftFlags);
    //wprintf(L"     2 and changed, vk: %i (%c) --->  %i (%c) ", vk,vk, ch,ch);
  }
}

void KMX_TranslateGroup(LPKMX_GROUP group, KMX_WORD vk, KMX_UINT shift, KMX_WCHAR ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    KMX_TranslateKey(&group->dpKeyArray[i], vk, shift, ch);
  }
}

void KMX_TranslateKeyboard(LPKMX_KEYBOARD kbd, KMX_WORD vk, KMX_UINT shift, KMX_WCHAR ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      KMX_TranslateGroup(&kbd->dpGroupArray[i], vk, shift, ch);
    }
  }
}

void KMX_ReportUnconvertedKeyRule(LPKMX_KEY key) {
  if(key->ShiftFlags == 0) {
    //KMX_LogError(L"Did not find a match for mnemonic rule on line %d, + '%c' > ...", key->Line, key->Key);
    //wprintf(L" _S2 Did not find a match for mnemonic rule on line %d, + '%c' > ...\n", key->Line, key->Key);
  } else if(key->ShiftFlags & VIRTUALCHARKEY) {
    //KMX_LogError(L"Did not find a match for mnemonic virtual character key rule on line %d, + [%x '%c'] > ...", key->Line, key->ShiftFlags, key->Key);
    //wprintf(L"_S2 Did not find a match for mnemonic virtual character key rule on line %d, + [%x '%c'] > ...\n", key->Line, key->ShiftFlags, key->Key);
  }
}

void KMX_ReportUnconvertedGroupRules(LPKMX_GROUP group) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    KMX_ReportUnconvertedKeyRule(&group->dpKeyArray[i]);
  }
}

void KMX_ReportUnconvertedKeyboardRules(LPKMX_KEYBOARD kbd) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      KMX_ReportUnconvertedGroupRules(&kbd->dpGroupArray[i]);
    }
  }
}

void KMX_TranslateDeadkeyKey(LPKMX_KEY key, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch, int abcd) {
PKMX_WCHAR PP = key->dpOutput;
  //if (key->Key == 94)
  //  wprintf(L"key->ShiftFlags: %i  isEqual: %i  %i(%c)   %i(%c) --->  %i \n", key->ShiftFlags, key->ShiftFlags & VIRTUALCHARKEY , key->Key , key->Key, ch,ch , ((key->ShiftFlags == 0 || key->ShiftFlags & VIRTUALCHARKEY) && key->Key == ch));
//wprintf(L"key->Key %i\n",key->Key);
/*if(abcd >1 && abcd < 815 ) {

wprintf(L"key->Key %i\tkey->dpContext: %i  %i %i %i %i %i %i %i %i    \tkey->dpoutput: %i  %i %i %i %i %i\n", key->Key,
*key->dpContext, *(key->dpContext+1),*(key->dpContext+2),*(key->dpContext+3),*(key->dpContext+4),*(key->dpContext+5),*(key->dpContext+6),*(key->dpContext+7),*(key->dpContext+8),
*key->dpOutput, *(key->dpOutput+1),*(key->dpOutput+2),*(key->dpOutput+3),*(key->dpOutput+4),*(key->dpOutput+5) );
}*/
   if((key->ShiftFlags == 0 || key->ShiftFlags & VIRTUALCHARKEY) && key->Key == ch) {
    // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
    // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
    // to provide an alternate..
    if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))   // I4327
      shift &= ~LCTRLFLAG;

    if(key->ShiftFlags == 0) {
      //LogError("Converted mnemonic rule on line %d, + '%c' TO dk(%d) + [%x K_%d]", key->Line, key->Key, deadkey, shift, vk);
      //wprintf(L"DK Converted mnemonic rule on line %d, + '%c' TO dk(%d) + [%x K_%d], %i(%c)\n", key->Line, key->Key, deadkey, shift, vk, ch, ch);
      key->ShiftFlags = ISVIRTUALKEY | shift;
    } else {
      //LogError("Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO dk(%d) + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, deadkey, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
      //wprintf(L"DK Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO dk(%d) + [%x K_%d], %i(%c)\n", key->Line, key->ShiftFlags, key->Key, deadkey, key->ShiftFlags & ~VIRTUALCHARKEY, vk, ch, ch);
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
  int sdfgh=0;
}

void KMX_TranslateDeadkeyGroup(LPKMX_GROUP group,KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    //wprintf(L"      key: %i\n",i );
    KMX_TranslateDeadkeyKey(&group->dpKeyArray[i], deadkey, vk, shift, ch, i);
  }
}

void KMX_TranslateDeadkeyKeyboard(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {

    //wprintf(L" group: %i\n",i );
      KMX_TranslateDeadkeyGroup(&kbd->dpGroupArray[i], deadkey, vk, shift, ch);
    }
  }
}

// _S2 this does not work OK
void KMX_AddDeadkeyRule(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift) {
  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG)) // I4549
    shift &= ~LCTRLFLAG;
    // _s2 idee spanish keyboard has dk on altgr !!

  // If the first group is not a matching-keys group, then we need to add into
  // each subgroup, otherwise just the match group
  int zaehler=0;
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      LPKMX_KEY keys = new KMX_KEY[kbd->dpGroupArray[i].cxKeyArray + 1];
      memcpy(keys+1, kbd->dpGroupArray[i].dpKeyArray, kbd->dpGroupArray[i].cxKeyArray * sizeof(KMX_KEY));
      keys[0].dpContext = new KMX_WCHAR[1];
      keys[0].dpContext[0] = 0;
      keys[0].dpOutput = new KMX_WCHAR[4]; // UC_SENTINEL, CODE_DEADKEY, deadkey_value, 0
      keys[0].dpOutput[0] = UC_SENTINEL;
      keys[0].dpOutput[1] = CODE_DEADKEY;
      keys[0].dpOutput[2] = deadkey; // TODO: translate to unique index
      keys[0].dpOutput[3] = 0;
      keys[0].Key = vk;
      keys[0].Line = 0;
      keys[0].ShiftFlags = shift | ISVIRTUALKEY;
      kbd->dpGroupArray[i].dpKeyArray = keys;
      kbd->dpGroupArray[i].cxKeyArray++;
      //LogError("Add deadkey rule:  + [%d K_%d] > dk(%d)", shift, vk, deadkey);
      zaehler++;
      //wprintf(L"Add deadkey rule:  + [%d K_%d] > dk(%d)  %i\n", shift, vk, deadkey,zaehler);

      if(i == kbd->StartGroup[1]) break;  // If this is the initial group, that's all we need to do.
    }
  }
}

KMX_WCHAR KMX_ScanXStringForMaxDeadkeyID(PKMX_WCHAR str) {
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

KMX_WCHAR KMX_GetUniqueDeadkeyID(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey) {
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
      dkid = max(dkid, KMX_ScanXStringForMaxDeadkeyID(kp->dpContext));
      dkid = max(dkid, KMX_ScanXStringForMaxDeadkeyID(kp->dpOutput));
    }
    dkid = max(dkid, KMX_ScanXStringForMaxDeadkeyID(gp->dpMatch));
    dkid = max(dkid, KMX_ScanXStringForMaxDeadkeyID(gp->dpNoMatch));
  }

  for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    dkid = max(dkid, KMX_ScanXStringForMaxDeadkeyID(sp->dpString));
  }

  s_dkids = (KMX_dkidmap*) realloc(s_dkids, sizeof(KMX_dkidmap) * (s_ndkids+1));
  s_dkids[s_ndkids].src_deadkey = deadkey;
  return s_dkids[s_ndkids++].dst_deadkey = s_next_dkid = ++dkid;
}

void KMX_ConvertDeadkey(LPKMX_KEYBOARD kbd, KMX_WORD vk, UINT shift, KMX_WCHAR deadkey, v_dw_3D &All_Vector, GdkKeymap* keymap) {
  KMX_WORD deadkeys[512], *pdk;

  // _S2 create dkTable once only and use as static/ define before func
  // create dk_createDK_ComposeTable
  v_dw_2D  dk_Table;
  create_DKTable(dk_Table);

  // Lookup the deadkey table for the deadkey in the physical keyboard
  // Then for each character, go through and map it through
  KMX_WCHAR dkid = KMX_GetUniqueDeadkeyID(kbd, deadkey);

  // Add the deadkey to the mapping table for use in the import rules phase
  KMX_DeadkeyMapping KMX_deadkeyMapping = { deadkey, dkid, shift, vk};    // I4353

  //__S2 NEEDS to be here later
  KMX_FDeadkeys.push_back(KMX_deadkeyMapping); //dkid, vk, shift);   // I4353
  KMX_AddDeadkeyRule(kbd, dkid, vk, shift);

  // _S2 NEEDS to go! --------
  /*// _S2 is only for testing &to compare 3 dk of windows
  if(KMX_FDeadkeys.size()<3 )
  KMX_FDeadkeys.push_back(KMX_deadkeyMapping); //dkid, vk, shift);   // I4353
  if(shift== 0 || shift== 16  )
    KMX_AddDeadkeyRule(kbd, dkid, vk, shift);
  else
    return;;*/
  // _S2 -----------------------------

  KMX_GetDeadkeys(dk_Table, deadkey, pdk = deadkeys, keymap);  // returns array of [usvk, ch_out] pairs

  while(*pdk) {

    // Look up the ch
    UINT vkUnderlying = KMX_get_VKUS_From_VKUnderlying_VEC(All_Vector, *pdk);

    //wprintf(L" vkUnderlying   %i    *pdk %i  %i %ixxx", vkUnderlying, *pdk, *(pdk+1), *(pdk+2));
     KMX_TranslateDeadkeyKeyboard(kbd, dkid, vkUnderlying, *(pdk+1), *(pdk+2));

    if(vkUnderlying != (UINT) *pdk) {
      //wprintf(L" TRANSLATED \n");
    }
    //else
      //wprintf(L" \n");
    pdk+=3;
  }
}

KMX_BOOL KMX_SetKeyboardToPositional(LPKMX_KEYBOARD kbd) {
  LPKMX_STORE sp;
  KMX_UINT i;
  for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    if(sp->dwSystemID == TSS_MNEMONIC) {
      if(!sp->dpString) {
        KMX_LogError(L"Invalid &mnemoniclayout system store");
        return FALSE;
      }
      if(u16cmp((const KMX_WCHAR*)sp->dpString, u"1") != 0) {
        KMX_LogError(L"Keyboard is not a mnemonic layout keyboard");
        return FALSE;
      }
      *sp->dpString = '0';
      return TRUE;
    }
  }

  KMX_LogError(L"Keyboard is not a mnemonic layout keyboard");
  return FALSE;
}

KMX_BOOL KMX_DoConvert(LPKMX_KEYBOARD kbd, KMX_BOOL bDeadkeyConversion, gint argc, gchar *argv[]) {

  KMX_WCHAR DeadKey;

  if(!KMX_SetKeyboardToPositional(kbd)) return FALSE;

  // Go through each of the shift states - base, shift, ctrl+alt, ctrl+alt+shift, [caps vs ncaps?]
  // Currently, we go in this order so the 102nd key works. But this is not ideal for keyboards without 102nd key:   // I4651
  // it catches only the first key that matches a given rule, but multiple keys may match that rule. This is particularly
  // evident for the 102nd key on UK, for example, where \ can be generated with VK_OEM_102 or AltGr+VK_QUOTE.
  // For now, we get the least shifted version, which is hopefully adequate.

  // _S2 TODO first version with GTK - maybe change later to  XklGetGroupNames  und XklGetCurrentState  as Eberhard suggested
  //_ init gdk
  GdkKeymap *keymap;
  if(InitializeGDK(&keymap , argc,  argv)) {
      wprintf(L"ERROR: can't Initialize GDK\n");
      return FALSE;
  }

  // create vector
  v_dw_3D All_Vector;
  if(createOneVectorFromBothKeyboards(All_Vector, keymap)){
    wprintf(L"ERROR: can't create one vector from both keyboards\n");
    return FALSE;
  }

  for (int j = 0; VKShiftState[j] != 0xFFFF; j++) { // I4651

    // Loop through each possible key on the keyboard
    for (int i = 0;KMX_VKMap[i]; i++) { // I4651

      // win goes via VK, Lin goes via SC
      UINT scUnderlying =  KMX_get_SCUnderlying_From_VKUS(KMX_VKMap[i]);
      KMX_WCHAR ch = KMX_get_CharUnderlying_From_SCUnderlying_GDK(keymap, VKShiftState[j], scUnderlying, &DeadKey);

      //LogError("--- VK_%d -> VK_%d [%c] dk=%d", VKMap[i], vkUnderlying, ch == 0 ? 32 : ch, DeadKey);
      //wprintf(L"--- VK_%d -> SC_%d [%c] dk=%d  ( ss %i) \n", KMX_VKMap[i], KMX_get_KeyCodeUnderlying_From_VKUS(KMX_VKMap[i]), ch == 0 ? 32 : ch, DeadKey,VKShiftState[j]);

      if(bDeadkeyConversion) {   // I4552
        if(ch == 0xFFFF) {
          ch = DeadKey;
        }
      }

//wprintf(L" scunderlying:  SS: %i Nr: %i VKMap[i] %i ch:%i (%c)\n", VKShiftState[j],i, KMX_VKMap[i],ch,ch );
      switch(ch) {
        case 0x0000: break;
        case 0xFFFF: KMX_ConvertDeadkey(kbd, KMX_VKMap[i], VKShiftState[j], DeadKey, All_Vector, keymap); break;   //test_keyboard_S2(kbd);    //-> mit Convert DK FALSCH; Ohne ConvertDK the same 
        default:     KMX_TranslateKeyboard(kbd, KMX_VKMap[i], VKShiftState[j], ch);
      }
    }
  }


  KMX_ReportUnconvertedKeyboardRules(kbd);
  if(!KMX_ImportRules(kbd, All_Vector, &keymap, &KMX_FDeadkeys, bDeadkeyConversion)) {   // I4353   // I4552
    return FALSE;
  }
  return TRUE;
}

// _S2 TODO
void KMX_LogError(const KMX_WCHART* m1,int m2) {
  wprintf((PWSTR)m1, m2);
}
/*
void KMX_LogError(const KMX_WCHART* m1,int m2, LPKMX_KEY key) {
  wprintf((PWSTR)m1, m2);
}
*/

// takes VK of US keyboard and returns SC of OTHER keyboard
UINT  KMX_get_SCUnderlying_From_VKUS(KMX_DWORD VirtualKeyUS) {
  UINT SC_US = 8 + USVirtualKeyToScanCode[VirtualKeyUS];
  UINT SC_OTHER = SC_US;  // not neccessary but to understand what we do
  return  SC_OTHER;
}

KMX_WCHAR KMX_get_VKUS_From_VKUnderlying_VEC(v_dw_3D All_Vector, KMX_DWORD VK_OTHER) {

  KMX_DWORD VK_US;
  for( int i=0; i< (int)All_Vector[0].size()-1 ;i++) {
    for( int j=1; j< (int)All_Vector[0][0].size();j++) {
      if ( ( All_Vector[0][i][j] == VK_OTHER ) ) {
        VK_US = All_Vector[1][i][j];;
        return VK_US;
      }
    }
  }
  return VK_OTHER;
}

// takes SC of underlying keyboard and returns character of underlying keyboard with shiftstate VKShiftState[j] or deadkey
KMX_WCHAR  KMX_get_CharUnderlying_From_SCUnderlying_GDK(GdkKeymap *keymap, KMX_UINT VKShiftState, UINT SC_OTHER, PKMX_WCHAR DeadKey) {

  PKMX_WCHAR dky;
  int VKShiftState_lin = map_VKShiftState_to_Lin(VKShiftState);
  KMX_DWORD KeyvalOther = KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK(keymap,SC_OTHER, VKShiftState_lin, dky);

  if ((KeyvalOther >= deadkey_min) && (KeyvalOther >=  deadkey_max)){
    *DeadKey = *dky;
    return 0xFFFF;
  }

  return (KMX_WCHAR) KeyvalOther;
}

int createOneVectorFromBothKeyboards(v_dw_3D &All_Vector,GdkKeymap *keymap){
  std::string US_language    = "us";
  const char* text_us        = "xkb_symbols \"basic\"";
  //const char* text_us        = "xkb_symbols \"intl\"";

  if(write_US_ToVector(All_Vector,US_language, text_us)) {
    wprintf(L"ERROR: can't write US to Vector \n");
    return 1;
  }

  // _S2 is it OK to have 65535 instead of 94, 96 and 180 as a value for deadkeys in Vector
  // add contents of other keyboard to All_Vector
  if( append_other_ToVector(All_Vector,keymap)) {
    wprintf(L"ERROR: can't append Other ToVector \n");
    return 2;
  }
  return 0;
}

int KMX_GetDeadkeys(v_dw_2D & dk_Table, KMX_WORD DeadKey, KMX_WORD *OutputPairs, GdkKeymap* keymap) {

  KMX_WORD *p = OutputPairs;
  KMX_DWORD shift;

  v_dw_2D  dk_SingleTable;
  find_dk_combinations_for_single_dk(&dk_Table, dk_SingleTable, DeadKey);
  for ( int i=0; i< (int) dk_SingleTable.size();i++) {
    KMX_WORD vk = KMX_changeKeynameToCapital(dk_SingleTable[i][1], shift, keymap);
    if(vk != 0) {
          *p++ = vk;
          *p++ = shift;
          *p++ = dk_SingleTable[i][2];
        }
        // _S2 TODO
        //else {
        //  LogError(L"Warning: complex deadkey not supported.");
      // }
  }
  *p = 0;
  return (p-OutputPairs);
}


// _S2 can go later
void test_keyboard_S2(LPKMX_KEYBOARD kmxfile){
  TestKeyboard_S2(kmxfile);
}

void TestKey_S2(LPKMX_KEY key, int iii, int gr) {

  KMX_WCHAR* PP= key->dpOutput;
  int z=0;

  if( *(key->dpOutput+1) != 0) {
    wprintf(L"\n     group[%i]      dpKeyArray[%i]  ",gr, iii);
    int tzuiop=0;
    do {
      wprintf(L"%i\t",  *(PP+z ));
      z++;
    } while (*(PP+z) !=0);
  }
  //if ((*(PP+z) !=0)) wprintf(L" _\n");
}

void TestGroup_S2(LPKMX_GROUP group ,int gr) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    TestKey_S2(&group->dpKeyArray[i],i,gr);
  }
}

void TestKeyboard_S2(LPKMX_KEYBOARD kbd) {
 for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      wprintf(L"\nkbd->dpGroupArray[%i]  \n",i);
      TestGroup_S2(&kbd->dpGroupArray[i], i);
    }
  }
}


// ---- old copy code from here ----------------------------------------------------------

/*
BOOL ConvertKeyboardToUnicode(LPKEYBOARD kbd); // I4273


#define KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_MCOMPILE KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE ".mcompile"

//
// Map of all shift states that we will work with
//
const UINT VKShiftState[] = {0, K_SHIFTFLAG, LCTRLFLAG|RALTFLAG, K_SHIFTFLAG|LCTRLFLAG|RALTFLAG, 0xFFFF};


void ReportUnconvertedKeyRule(LPKEY key) {
  if(key->ShiftFlags == 0) {
    LogError(L"Did not find a match for mnemonic rule on line %d, + '%c' > ...", key->Line, key->Key);
  } else if(key->ShiftFlags & VIRTUALCHARKEY) {
    LogError(L"Did not find a match for mnemonic virtual character key rule on line %d, + [%x '%c'] > ...", key->Line, key->ShiftFlags, key->Key);
  }
}

void ReportUnconvertedGroupRules(LPGROUP group) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    ReportUnconvertedKeyRule(&group->dpKeyArray[i]);
  }
}

void ReportUnconvertedKeyboardRules(LPKEYBOARD kbd) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      ReportUnconvertedGroupRules(&kbd->dpGroupArray[i]);
    }
  }
}

void TranslateDeadkeyKey(LPKEY key, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  if((key->ShiftFlags == 0 || key->ShiftFlags & VIRTUALCHARKEY) && key->Key == ch) {

    // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
    // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
    // to provide an alternate..
    if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))   // I4327
      shift &= ~LCTRLFLAG;

    if(key->ShiftFlags == 0) {
      //LogError("Converted mnemonic rule on line %d, + '%c' TO dk(%d) + [%x K_%d]", key->Line, key->Key, deadkey, shift, vk);
      key->ShiftFlags = ISVIRTUALKEY | shift;
    } else {
      //LogError("Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO dk(%d) + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, deadkey, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
      key->ShiftFlags &= ~VIRTUALCHARKEY;
    }

    int len = wcslen(key->dpContext);
    PWSTR context = new WCHAR[len + 4];
    memcpy(context, key->dpContext, len * sizeof(WCHAR));
    context[len] = UC_SENTINEL;
    context[len+1] = CODE_DEADKEY;
    context[len+2] = deadkey;
    context[len+3] = 0;
    key->dpContext = context;
    key->Key = vk;
  }
}

void TranslateDeadkeyGroup(LPGROUP group, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    TranslateDeadkeyKey(&group->dpKeyArray[i], deadkey, vk, shift, ch);
  }
}

void TranslateDeadkeyKeyboard(LPKEYBOARD kbd, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      TranslateDeadkeyGroup(&kbd->dpGroupArray[i], deadkey, vk, shift, ch);
    }
  }
}


*/



//---------old- - original code------------------------------------------
/*#include "pch.h"
#include <vector>

#include <stdio.h>
#include <stdarg.h>
#include <varargs.h>

BOOL DoConvert(LPKEYBOARD kbd, PWSTR kbid, BOOL bDeadkeyConversion);
BOOL SaveKeyboard(LPKEYBOARD kbd, PWSTR filename);
bool ImportRules(WCHAR *kbid, LPKEYBOARD kp, std::vector<DeadkeyMapping> *FDeadkeys, BOOL bDeadkeyConversion);   // I4353   // I4327
BOOL ConvertKeyboardToUnicode(LPKEYBOARD kbd);   // I4273
int run(int argc, wchar_t * argv[]);

std::vector<DeadkeyMapping> FDeadkeys;   // I4353

#define KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_MCOMPILE KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE ".mcompile"

int wmain(int argc, wchar_t * argv[])
{
  return keyman_sentry_wmain(false, KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_MCOMPILE, argc, argv, run);
}

int run(int argc, wchar_t * argv[])
{
  if(argc < 3 || (argc < 5 && wcscmp(argv[1], L"-u") != 0)) {   // I4273
    printf(
         "Usage: mcompile -u infile.kmx outfile.kmx\n"
         "       mcompile [-d] infile.kmx kbdfile.dll kbid outfile.kmx\n"
         "  With -u parameter, converts keyboard from ANSI to Unicode\n"
         "  Otherwise, mcompile converts a Keyman mnemonic layout to a\n"
         "  positional one based on the Windows keyboard\n"
         "  layout file given by kbdfile.dll\n\n"
         "  kbid should be a hexadecimal number e.g. 409 for US English\n"
         "  -d   convert deadkeys to plain keys\n");   // I4552

    return 1;
  }

  if(wcscmp(argv[1], L"-u") == 0) {   // I4273
    wchar_t *infile = argv[2], *outfile = argv[3];

    LPKEYBOARD kmxfile;

    if(!LoadKeyboard(infile, &kmxfile)) {
      LogError(L"Failed to load keyboard (%d)", GetLastError());
      return 3;
    }

    if(ConvertKeyboardToUnicode(kmxfile)) {
      SaveKeyboard(kmxfile, outfile);
    }

    //DeleteReallocatedPointers(kmxfile); :TODO
    delete[] kmxfile;

    return 0;   // I4279
  }

  int bDeadkeyConversion = wcscmp(argv[1], L"-d") == 0;   // I4552
  int n = (bDeadkeyConversion ? 2 : 1);

  wchar_t *infile = argv[n], *indll = argv[n+1], *kbid = argv[n+2], *outfile = argv[n+3];

  wprintf(L"mcompile%ls \"%ls\" \"%ls\" \"%ls\" \"%ls\"\n", bDeadkeyConversion ? L" -d":L"", infile, indll, kbid, outfile);   // I4174

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

  if(!LoadNewLibrary(indll)) {
    LogError(L"Failed to load keyboard DLL (%d)", GetLastError());
    return 2;
  }

  LPKEYBOARD kmxfile;

  if(!LoadKeyboard(infile, &kmxfile)) {
    LogError(L"Failed to load keyboard (%d)", GetLastError());
    return 3;
  }

  if(DoConvert(kmxfile, kbid, bDeadkeyConversion)) {   // I4552
    SaveKeyboard(kmxfile, outfile);
  }

  //DeleteReallocatedPointers(kmxfile); :TODO
  delete kmxfile;

	return 0;
}


//
// Map of all US English virtual key codes that we can translate
//
const WORD VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  VK_SPACE,
  VK_ACCENT, VK_HYPHEN, VK_EQUAL,
  VK_LBRKT, VK_RBRKT, VK_BKSLASH,
  VK_COLON, VK_QUOTE,
  VK_COMMA, VK_PERIOD, VK_SLASH,
  VK_xDF, VK_OEM_102,
  0
};


//
// Map of all shift states that we will work with
//
const UINT VKShiftState[] = {0, K_SHIFTFLAG, LCTRLFLAG|RALTFLAG, K_SHIFTFLAG|LCTRLFLAG|RALTFLAG, 0xFFFF};

//
// TranslateKey
//
// For each key rule on the keyboard, remap its key to the
// correct shift state and key.  Adjust the LCTRL+RALT -> RALT if necessary
//
void TranslateKey(LPKEY key, WORD vk, UINT shift, WCHAR ch) {

  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))
    shift &= ~LCTRLFLAG;

  if(key->ShiftFlags == 0 && key->Key == ch) {
    // Key is a mnemonic key with no shift state defined.
    // Remap the key according to the character on the key cap.
    //LogError(L"Converted mnemonic rule on line %d, + '%c' TO + [%x K_%d]", key->Line, key->Key, shift, vk);
    key->ShiftFlags = ISVIRTUALKEY | shift;
    key->Key = vk;
  } else if(key->ShiftFlags & VIRTUALCHARKEY && key->Key == ch) {
    // Key is a virtual character key with a hard-coded shift state.
    // Do not remap the shift state, just move the key.
    // This will not result in 100% wonderful mappings as there could
    // be overlap, depending on how keys are arranged on the target layout.
    // But that is up to the designer.
    //LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
    key->ShiftFlags &= ~VIRTUALCHARKEY;
    key->Key = vk;
  }
}

void TranslateGroup(LPGROUP group, WORD vk, UINT shift, WCHAR ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    TranslateKey(&group->dpKeyArray[i], vk, shift, ch);
  }
}

void TranslateKeyboard(LPKEYBOARD kbd, WORD vk, UINT shift, WCHAR ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      TranslateGroup(&kbd->dpGroupArray[i], vk, shift, ch);
    }
  }
}

void ReportUnconvertedKeyRule(LPKEY key) {
  if(key->ShiftFlags == 0) {
    LogError(L"Did not find a match for mnemonic rule on line %d, + '%c' > ...", key->Line, key->Key);
  } else if(key->ShiftFlags & VIRTUALCHARKEY) {
    LogError(L"Did not find a match for mnemonic virtual character key rule on line %d, + [%x '%c'] > ...", key->Line, key->ShiftFlags, key->Key);
  }
}

void ReportUnconvertedGroupRules(LPGROUP group) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    ReportUnconvertedKeyRule(&group->dpKeyArray[i]);
  }
}

void ReportUnconvertedKeyboardRules(LPKEYBOARD kbd) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      ReportUnconvertedGroupRules(&kbd->dpGroupArray[i]);
    }
  }
}

void TranslateDeadkeyKey(LPKEY key, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  if((key->ShiftFlags == 0 || key->ShiftFlags & VIRTUALCHARKEY) && key->Key == ch) {

    // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
    // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
    // to provide an alternate..
    if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))   // I4327
      shift &= ~LCTRLFLAG;

    if(key->ShiftFlags == 0) {
      //LogError("Converted mnemonic rule on line %d, + '%c' TO dk(%d) + [%x K_%d]", key->Line, key->Key, deadkey, shift, vk);
      key->ShiftFlags = ISVIRTUALKEY | shift;
    } else {
      //LogError("Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO dk(%d) + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, deadkey, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
      key->ShiftFlags &= ~VIRTUALCHARKEY;
    }

    int len = wcslen(key->dpContext);
    PWSTR context = new WCHAR[len + 4];
    memcpy(context, key->dpContext, len * sizeof(WCHAR));
    context[len] = UC_SENTINEL;
    context[len+1] = CODE_DEADKEY;
    context[len+2] = deadkey;
    context[len+3] = 0;
    key->dpContext = context;
    key->Key = vk;
  }
}

void TranslateDeadkeyGroup(LPGROUP group, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    TranslateDeadkeyKey(&group->dpKeyArray[i], deadkey, vk, shift, ch);
  }
}

void TranslateDeadkeyKeyboard(LPKEYBOARD kbd, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      TranslateDeadkeyGroup(&kbd->dpGroupArray[i], deadkey, vk, shift, ch);
    }
  }
}

void AddDeadkeyRule(LPKEYBOARD kbd, WCHAR deadkey, WORD vk, UINT shift) {
  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))   // I4549
    shift &= ~LCTRLFLAG;

  // If the first group is not a matching-keys group, then we need to add into
  // each subgroup, otherwise just the match group
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      LPKEY keys = new KEY[kbd->dpGroupArray[i].cxKeyArray + 1];
      memcpy(keys+1, kbd->dpGroupArray[i].dpKeyArray, kbd->dpGroupArray[i].cxKeyArray * sizeof(KEY));
      keys[0].dpContext = new WCHAR[1];
      keys[0].dpContext[0] = 0;
      keys[0].dpOutput = new WCHAR[4]; // UC_SENTINEL, CODE_DEADKEY, deadkey_value, 0
      keys[0].dpOutput[0] = UC_SENTINEL;
      keys[0].dpOutput[1] = CODE_DEADKEY;
      keys[0].dpOutput[2] = deadkey; // TODO: translate to unique index
      keys[0].dpOutput[3] = 0;
      keys[0].Key = vk;
      keys[0].Line = 0;
      keys[0].ShiftFlags = shift | ISVIRTUALKEY;
      kbd->dpGroupArray[i].dpKeyArray = keys;
      kbd->dpGroupArray[i].cxKeyArray++;
      //LogError("Add deadkey rule:  + [%d K_%d] > dk(%d)", shift, vk, deadkey);
      if(i == kbd->StartGroup[1]) break;  // If this is the initial group, that's all we need to do.
    }
  }
}

WCHAR ScanXStringForMaxDeadkeyID(LPWSTR str) {
  WCHAR dkid = 0;
  while(str && *str) {
    if(*str == UC_SENTINEL) {
      switch(*(str+1)) {
      case CODE_DEADKEY:
        dkid = max(dkid, *(str+2));
      }
    }
    str = incxstr(str);
  }
  return dkid;
}

struct dkidmap {
  WCHAR src_deadkey, dst_deadkey;
};

WCHAR GetUniqueDeadkeyID(LPKEYBOARD kbd, WCHAR deadkey) {
  LPGROUP gp;
  LPKEY kp;
  LPSTORE sp;
  UINT i, j;
  WCHAR dkid = 0;
  static WCHAR s_next_dkid = 0;
  static dkidmap *s_dkids = NULL;
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
    s_dkids = (dkidmap*) realloc(s_dkids, sizeof(dkidmap) * (s_ndkids+1));
    s_dkids[s_ndkids].src_deadkey = deadkey;
    return s_dkids[s_ndkids++].dst_deadkey = ++s_next_dkid;
  }

  for(i = 0, gp = kbd->dpGroupArray; i < kbd->cxGroupArray; i++, gp++) {
    for(j = 0, kp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kp++) {
      dkid = max(dkid, ScanXStringForMaxDeadkeyID(kp->dpContext));
      dkid = max(dkid, ScanXStringForMaxDeadkeyID(kp->dpOutput));
    }
    dkid = max(dkid, ScanXStringForMaxDeadkeyID(gp->dpMatch));
    dkid = max(dkid, ScanXStringForMaxDeadkeyID(gp->dpNoMatch));
  }

  for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    dkid = max(dkid, ScanXStringForMaxDeadkeyID(sp->dpString));
  }

  s_dkids = (dkidmap*) realloc(s_dkids, sizeof(dkidmap) * (s_ndkids+1));
  s_dkids[s_ndkids].src_deadkey = deadkey;
  return s_dkids[s_ndkids++].dst_deadkey = s_next_dkid = ++dkid;
}


void ConvertDeadkey(LPKEYBOARD kbd, WORD vk, UINT shift, WCHAR deadkey) {
  WORD deadkeys[512], *pdk;

  // Lookup the deadkey table for the deadkey in the physical keyboard
  // Then for each character, go through and map it through

  WCHAR dkid = GetUniqueDeadkeyID(kbd, deadkey);

  // Add the deadkey to the mapping table for use in the import rules phase
  DeadkeyMapping deadkeyMapping = { deadkey, dkid, shift, vk };   // I4353
  FDeadkeys.push_back(deadkeyMapping); //dkid, vk, shift);   // I4353

  AddDeadkeyRule(kbd, dkid, vk, shift);

  GetDeadkeys(deadkey, pdk = deadkeys);  // returns array of [usvk, ch_out] pairs
  while(*pdk) {
    // Look up the ch
    UINT vkUnderlying = VKUnderlyingLayoutToVKUS(*pdk);
    TranslateDeadkeyKeyboard(kbd, dkid, vkUnderlying, *(pdk+1), *(pdk+2));
    pdk+=3;
  }
}

BOOL SetKeyboardToPositional(LPKEYBOARD kbd) {
  LPSTORE sp;
  UINT i;
  for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    if(sp->dwSystemID == TSS_MNEMONIC) {
      if(!sp->dpString) {
        LogError(L"Invalid &mnemoniclayout system store");
        return FALSE;
      }
      if(wcscmp(sp->dpString, L"1") != 0) {
        LogError(L"Keyboard is not a mnemonic layout keyboard");
        return FALSE;
      }
      *sp->dpString = '0';
      return TRUE;
    }
  }

  LogError(L"Keyboard is not a mnemonic layout keyboard");
  return FALSE;
}

BOOL DoConvert(LPKEYBOARD kbd, LPWSTR kbid, BOOL bDeadkeyConversion) {   // I4552
  WCHAR DeadKey;

  if(!SetKeyboardToPositional(kbd)) return FALSE;

  // Go through each of the shift states - base, shift, ctrl+alt, ctrl+alt+shift, [caps vs ncaps?]
  // Currently, we go in this order so the 102nd key works. But this is not ideal for keyboards without 102nd key:   // I4651
  // it catches only the first key that matches a given rule, but multiple keys may match that rule. This is particularly
  // evident for the 102nd key on UK, for example, where \ can be generated with VK_OEM_102 or AltGr+VK_QUOTE.
  // For now, we get the least shifted version, which is hopefully adequate.

  for(int j = 0; VKShiftState[j] != 0xFFFF; j++) {   // I4651
    // Go through each possible key on the keyboard
    for(int i = 0; VKMap[i]; i++) {   // I4651
      UINT vkUnderlying = VKUSToVKUnderlyingLayout(VKMap[i]);

      WCHAR ch = CharFromVK(vkUnderlying, VKShiftState[j], &DeadKey);

      //LogError("--- VK_%d -> VK_%d [%c] dk=%d", VKMap[i], vkUnderlying, ch == 0 ? 32 : ch, DeadKey);

      if(bDeadkeyConversion) {   // I4552
        if(ch == 0xFFFF) {
          ch = DeadKey;
        }
      }

      switch(ch) {
        case 0x0000: break;
        case 0xFFFF: ConvertDeadkey(kbd, VKMap[i], VKShiftState[j], DeadKey); break;
        default:     TranslateKeyboard(kbd, VKMap[i], VKShiftState[j], ch);
      }

      //
    }
  }

  ReportUnconvertedKeyboardRules(kbd);

  if(!ImportRules(kbid, kbd, &FDeadkeys, bDeadkeyConversion)) {   // I4353   // I4552
    return FALSE;
  }

  return TRUE;
}

void LogError(PWSTR fmt, ...) {
	WCHAR fmtbuf[256];

	va_list vars;
	va_start(vars, fmt);
	_vsnwprintf_s(fmtbuf, _countof(fmtbuf), _TRUNCATE, fmt, vars);  // I2248   // I3547
	fmtbuf[255] = 0;
  _putws(fmtbuf);
}
*/
