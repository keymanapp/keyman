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

#include "mcompile.h"

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------


KMX_BOOL KMX_DoConvert(LPKMX_KEYBOARD kbd, KMX_BOOL bDeadkeyConversion, gint argc, gchar *argv[]);

bool KMX_ImportRules( LPKMX_KEYBOARD kp,vec_dword_3D& all_vector, GdkKeymap **keymap,std::vector<KMX_DeadkeyMapping> *KMX_FDeadkeys, KMX_BOOL bDeadkeyConversion); // I4353   // I4327

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

#else  // LINUX
  int main(int argc, char* argv[]) {
    std::vector<std::u16string> str_argv_16 = convert_argv_to_Vector_u16str(argc, argv);
    run(argc, str_argv_16, argv);
#endif

}

int run(int argc, std::vector<std::u16string> str_argv, char* argv_ch[] = NULL){

  std::vector<const char16_t*> argv;
  for (int i = 0; i < argc; i++) {
    const char16_t* cmdl_par = str_argv[i].c_str();
    argv.push_back(cmdl_par);
  }

  if(argc < 3 || (argc > 4)) {   // I4273// I4273
    wprintf(
        L"Usage: mcompile [-d] infile.kmx outfile.kmx\n"
        L"       mmcompile -u ...  (not available for Linux)\n "
        L"       mcompile converts a Keyman mnemonic layout to a\n"
        L"       positional one based on the Linux keyboard\n"
        L"       layout on top position\n"
        L"       (-d   convert deadkeys to plain keys) not available yet \n\n"
        );   // I4552

    return 1;
  }
  // -u option is not available for Linux


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
    KMX_LogError(L"Failed to load keyboard (%d)\n", errno );
    return 3;
  }


#if defined(_WIN32) || defined(_WIN64)
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
  return 0;
}

// Map of all shift states that we will work with
const UINT VKShiftState[] = {0, K_SHIFTFLAG, LCTRLFLAG|RALTFLAG, K_SHIFTFLAG|LCTRLFLAG|RALTFLAG, 0xFFFF};

//
// TranslateKey
//
// For each key rule on the keyboard, remap its key to the
// correct shift state and key.  Adjust the LCTRL+RALT -> RALT if necessary
//
void KMX_TranslateKey(LPKMX_KEY key, KMX_WORD vk, UINT shift, KMX_WCHAR ch) {
  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))
    shift &= ~LCTRLFLAG;

  if(key->ShiftFlags == 0 && key->Key == ch) {
    // Key is a mnemonic key with no shift state defined.
    // Remap the key according to the character on the key cap.
    //KMX_LogError(L"Converted mnemonic rule on line %d, + '%c' TO + [%x K_%d]", key->Line, key->Key, shift, vk);
    key->ShiftFlags = ISVIRTUALKEY | shift;
    key->Key = vk;
  } else if(key->ShiftFlags & VIRTUALCHARKEY && key->Key == ch) {
    // Key is a virtual character key with a hard-coded shift state.
    // Do not remap the shift state, just move the key.
    // This will not result in 100% wonderful mappings as there could
    // be overlap, depending on how keys are arranged on the target layout.
    // But that is up to the designer.
    //KMX_LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
    key->ShiftFlags &= ~VIRTUALCHARKEY;
    key->Key = vk;
  }
}

void KMX_TranslateGroup(LPKMX_GROUP group, KMX_WORD vk, UINT shift, KMX_WCHAR ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    KMX_TranslateKey(&group->dpKeyArray[i], vk, shift, ch);
  }
}

void KMX_TranslateKeyboard(LPKMX_KEYBOARD kbd, KMX_WORD vk, UINT shift, KMX_WCHAR ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      KMX_TranslateGroup(&kbd->dpGroupArray[i], vk, shift, ch);
    }
  }
}

void KMX_ReportUnconvertedKeyRule(LPKMX_KEY key) {
  if(key->ShiftFlags == 0) {
    //KMX_LogError(L"Did not find a match for mnemonic rule on line %d, + '%c' > ...", key->Line, key->Key);
  } else if(key->ShiftFlags & VIRTUALCHARKEY) {
    KMX_LogError(L"Did not find a match for mnemonic virtual character key rule on line %d, + [%x '%c'] > ...", key->Line, key->ShiftFlags, key->Key);
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

void KMX_TranslateDeadkeyKey(LPKMX_KEY key, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch) {
// _S2 TOP_2 INFO this produces  a different output due to different layouts for Lin<-> win ( for the same language!!)

   if((key->ShiftFlags == 0 || key->ShiftFlags & VIRTUALCHARKEY) && key->Key == ch) {
    // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
    // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
    // to provide an alternate..
    if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))   // I4327
      shift &= ~LCTRLFLAG;

    if(key->ShiftFlags == 0) {
      //KMX_LogError(L"Converted mnemonic rule on line %d, + '%c' TO dk(%d) + [%x K_%d]", key->Line, key->Key, deadkey, shift, vk);
      key->ShiftFlags = ISVIRTUALKEY | shift;
    } else {
      //KMX_LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO dk(%d) + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, deadkey, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
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

void KMX_TranslateDeadkeyGroup(LPKMX_GROUP group,KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    KMX_TranslateDeadkeyKey(&group->dpKeyArray[i], deadkey, vk, shift, ch);
  }
}

void KMX_TranslateDeadkeyKeyboard(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      KMX_TranslateDeadkeyGroup(&kbd->dpGroupArray[i], deadkey, vk, shift, ch);
    }
  }
}

void KMX_AddDeadkeyRule(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift) {
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
      KMX_LogError(L"Add deadkey rule:  + [%d K_%d] > dk(%d)", shift, vk, deadkey);
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

void KMX_ConvertDeadkey(LPKMX_KEYBOARD kbd, KMX_WORD vk_US, UINT shift, KMX_WCHAR deadkey, vec_dword_3D& all_vector, GdkKeymap* keymap,vec_dword_2D dk_Table) {
  KMX_WORD deadkeys[512], *pdk;

  // Lookup the deadkey table for the deadkey in the physical keyboard
  // Then for each character, go through and map it through
  KMX_WCHAR dkid = KMX_GetUniqueDeadkeyID(kbd, deadkey);

  // Add the deadkey to the mapping table for use in the import rules phase
  KMX_DeadkeyMapping KMX_deadkeyMapping = { deadkey, dkid, shift, vk_US};    // I4353

  KMX_FDeadkeys.push_back(KMX_deadkeyMapping); //dkid, vk, shift);   // I4353
  KMX_AddDeadkeyRule(kbd, dkid, vk_US, shift);

  KMX_GetDeadkeys(dk_Table, deadkey, pdk = deadkeys, keymap);  // returns array of [usvk, ch_out] pairs

  while(*pdk) {
    // Look up the ch
    UINT KeyValUnderlying = KMX_get_KeyValUnderlying_From_KeyValUS(all_vector, *pdk);
    KMX_TranslateDeadkeyKeyboard(kbd, dkid, KeyValUnderlying, *(pdk+1), *(pdk+2));
    pdk+=3;
  }
}

KMX_BOOL KMX_SetKeyboardToPositional(LPKMX_KEYBOARD kbd) {
  LPKMX_STORE sp;
  UINT i;
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

  KMX_WCHAR DeadKey=0;

  if(!KMX_SetKeyboardToPositional(kbd)) return FALSE;

  // Go through each of the shift states - base, shift, ctrl+alt, ctrl+alt+shift, [caps vs ncaps?]
  // Currently, we go in this order so the 102nd key works. But this is not ideal for keyboards without 102nd key:   // I4651
  // it catches only the first key that matches a given rule, but multiple keys may match that rule. This is particularly
  // evident for the 102nd key on UK, for example, where \ can be generated with VK_OEM_102 or AltGr+VK_QUOTE.
  // For now, we get the least shifted version, which is hopefully adequate.

  GdkKeymap *keymap;
  if(InitializeGDK(&keymap,  argc, argv)) {
      wprintf(L"ERROR: can't Initialize GDK\n");
      return FALSE;
  }

  // create vector that contains Keycode, base, shift for US-KEyboard and underlying keyboard
  vec_dword_3D all_vector;
  if(createOneVectorFromBothKeyboards(all_vector, keymap)){
    wprintf(L"ERROR: can't create one vector from both keyboards\n");
    return FALSE;
  }

  vec_dword_2D  dk_Table;
  create_DKTable(dk_Table);

  for (int j = 0; VKShiftState[j] != 0xFFFF; j++) { // I4651

    // Loop through each possible key on the keyboard
    for (int i = 0;KMX_VKMap[i]; i++) { // I4651

      // windows uses  VK, Linux uses SC/Keycode
      UINT scUnderlying =  KMX_get_KeyCodeUnderlying_From_VKUS(KMX_VKMap[i]);
      KMX_WCHAR ch = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keymap, VKShiftState[j], scUnderlying, &DeadKey);

      //wprintf(L"--- VK_%d -> SC_ [%c] dk=%d  ( ss %i) \n", KMX_VKMap[i], ch == 0 ? 32 : ch, DeadKey, VKShiftState[j]);

      if(bDeadkeyConversion) {   // I4552
        if(ch == 0xFFFF) {
          ch = DeadKey;
        }
      }

      switch(ch) {
        case 0x0000: break;
        case 0xFFFF: KMX_ConvertDeadkey(kbd, KMX_VKMap[i], VKShiftState[j], DeadKey, all_vector, keymap,  dk_Table); break;
        default: KMX_TranslateKeyboard(kbd, KMX_VKMap[i], VKShiftState[j], ch);
      }
    }
  }

  KMX_ReportUnconvertedKeyboardRules(kbd);

  if(!KMX_ImportRules(kbd, all_vector, &keymap, &KMX_FDeadkeys, bDeadkeyConversion)) {   // I4353   // I4552
    return FALSE;
  }
  return TRUE;
}

int KMX_GetDeadkeys(vec_dword_2D & dk_Table, KMX_WORD deadkey, KMX_WORD *OutputPairs, GdkKeymap* keymap) {

  KMX_WORD *p = OutputPairs;
  KMX_DWORD shift;

  vec_dword_2D  dk_SingleTable;
  query_dk_combinations_for_specific_dk(&dk_Table, dk_SingleTable, deadkey);
  for ( int i=0; i< (int) dk_SingleTable.size();i++) {
    KMX_WORD vk = KMX_change_keyname_to_capital(dk_SingleTable[i][1], shift, keymap);
    if(vk != 0) {
      *p++ = vk;
      *p++ = shift;
      *p++ = dk_SingleTable[i][2];
    }
    else {
      KMX_LogError(L"Warning: complex deadkey not supported.");
    }
  }
  *p = 0;
  return (p-OutputPairs);
}

void KMX_LogError(const wchar_t* fmt, ...) {
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

