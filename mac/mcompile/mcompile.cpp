/*
 * Keyman is copyright (C) 2004 SIL International. MIT License.
 *
 * Mnemonic layout support for Linux
 */

//
// Defines the entry point for the console application.
//
// Note: this program deliberately leaks memory as it has a very short life cycle and managing the memory allocations
// for the subcomponents of the compiled keyboard is an unnecessary optimisation. Just so you know.
//

#include <stdio.h>
#include <wchar.h>
#include "mcompile.h"
#include "u16.h"
/**
 * @brief  convert mnemonic keyboard layout to positional keyboard layout and translate keyboard
 * @param  kbd pointer to US keyboard
 * @param  bDeadkeyConversion option for converting a deadkey to a character: 1=conversion; 0= no conversion
 * @param  argc number of commandline arguments
 * @return TRUE if conversion was successful;  FALSE if not
 */
KMX_BOOL mac_KMX_DoConvert(LPKMX_KEYBOARD kbd, KMX_BOOL bDeadkeyConversion, int argc);

/** @brief  Collect the key data, translate it to kmx and append to the existing keyboard */
bool mac_KMX_ImportRules(LPKMX_KEYBOARD kp, vec_dword_3D &all_vector, const UCKeyboardLayout** keyboard_layout, std::vector<KMX_DeadkeyMapping>* KMX_FDeadkeys, KMX_BOOL bDeadkeyConversion);  // I4353   // I4327

std::vector<KMX_DeadkeyMapping> KMX_FDeadkeys; // I4353

// Note: max is not a standard c api function or macro
#ifndef max
#define max(a,b)            (((a) > (b)) ? (a) : (b))
#endif

// _S2 run and getdeadkeys to here
#define _countof(a) (sizeof(a) / sizeof(*(a)))

   /**
 * @brief  main function for mcompile for Windows, Linux, Mac
 * @param  argc number of commandline arguments
 * @param  argv commandline arguments
 * @return 0 on success
 */
#if defined(_WIN32) || defined(_WIN64)
  int wmain(int argc, wchar_t* argv[]) {
    std::vector<std::u16string> str_argv_16 = convert_argvW_to_Vector_u16str(argc, argv);
    run(argc, str_argv_16);

#elif ((__linux__) || (__unix__))  // LINUX, UNIX
  int main(int argc, char* argv[]) {
    std::vector<std::u16string> str_argv_16 = convert_argv_to_Vector_u16str(argc, argv);
    run(argc, str_argv_16, argv);

#elif (__APPLE__ && TARGET_OS_MAC)
  #include <TargetConditionals.h>

  int main(int argc, char* argv[]) {
  std::vector<std::u16string> str_argv_16 = convert_argv_to_Vector_u16str(argc, argv);

  mac_run(argc, str_argv_16, argv);
  printf("\n................................ END .............................. _S2 \n");

#endif
}
 
/** @brief  start of mcompile; load, convert and save keyboard */
int mac_run(int argc, std::vector<std::u16string> str_argv, char* argv_ch[] = NULL) {
  std::vector<const char16_t*> argv;
  for (int i = 0; i < argc; i++) {
    const char16_t* cmdl_par = str_argv[i].c_str();
    argv.push_back(cmdl_par);
  }

  if (argc < 3 || (argc > 4)) {  // I4273// I4273
    wprintf(
        L"Usage: mcompile [-d] infile.kmx outfile.kmx\n"
        L"       mmcompile -u ...  (not available for mac)\n "
        L"      mcompile converts a Keyman mnemonic layout to a\n"
        L"       positional one based on the mac keyboard\n"
        L"       layout on top position\n"
        L"       (-d   convert deadkeys to plain keys) not available yet \n\n");  // I4552

    return 1;
  }
  // -u option is not available for Linux and macOS

  int bDeadkeyConversion = u16cmp(argv[1], u"-d") == 0;  // I4552
  int n = (bDeadkeyConversion ? 2 : 1);

  char16_t *infile = (char16_t*)argv[n], *outfile = (char16_t*)argv[n + 1];

  // _S2 TODO open again!!
  //wprintf(L"mcompile%ls \"%ls\" \"%ls\"\n", bDeadkeyConversion ? L" -d" : L"", u16fmt((const char16_t*) infile).c_str(), u16fmt((const char16_t*) outfile).c_str() ); // I4174

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

  if (!KMX_LoadKeyboard(infile, &kmxfile)) {
    mac_KMX_LogError(L"Failed to load keyboard (%d)\n", errno);
    return 3;
  }

  #if defined(_WIN32) || defined(_WIN64)  // Windows
    if (DoConvert(kmxfile, kbid, bDeadkeyConversion)) {  // I4552F
      KMX_SaveKeyboard(kmxfile, outfile);
    }

  #elif ((__linux__) || (__unix__))  // LINUX, UNIX
    if (mac_KMX_DoConvert(kmxfile, bDeadkeyConversion, argc)) {  // I4552F
      KMX_SaveKeyboard(kmxfile, outfile);
  }

  #elif (__APPLE__ && TARGET_OS_MAC)  // macOS
    if (mac_KMX_DoConvert(kmxfile, bDeadkeyConversion, argc)) {  // I4552F
      KMX_SaveKeyboard(kmxfile, outfile);
  }
  #endif

  // DeleteReallocatedPointers(kmxfile); :TODO   // _S2 not my ToDo :-)
  delete kmxfile;
  return 0;
}

// Map of all shift states that we will work with
const UINT VKShiftState[] = {0, K_SHIFTFLAG, LCTRLFLAG | RALTFLAG, K_SHIFTFLAG | LCTRLFLAG | RALTFLAG, 0xFFFF};

//
// TranslateKey
//
// For each key rule on the keyboard, remap its key to the
// correct shift state and key.  Adjust the LCTRL+RALT -> RALT if necessary
//

/**
 * @brief  translate each key of a group: remap the content of a key (key->Key) of the US keyboard to a character (ch)
 * @param  key pointer to a key
 * @param  vk a keyvalue of the US keyboard
 * @param  shift shiftstate
 * @param  ch character of the underlying keyboard to be remapped
 * @return void
 */
void mac_KMX_TranslateKey(LPKMX_KEY key, KMX_WORD vk, UINT shift, KMX_WCHAR ch) {
  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if ((shift & (LCTRLFLAG | RALTFLAG)) == (LCTRLFLAG | RALTFLAG))
    shift &= ~LCTRLFLAG;

  if (key->ShiftFlags == 0 && key->Key == ch) {
    // Key is a mnemonic key with no shift state defined.
    // Remap the key according to the character on the key cap.
    // mac_KMX_LogError(L"Converted mnemonic rule on line %d, + '%c' TO + [%x K_%d]", key->Line, key->Key, shift, vk);// 1
    key->ShiftFlags = ISVIRTUALKEY | shift;
    key->Key = vk;
  } else if (key->ShiftFlags & VIRTUALCHARKEY && key->Key == ch) {
    // Key is a virtual character key with a hard-coded shift state.
    // Do not remap the shift state, just move the key.
    // This will not result in 100% wonderful mappings as there could
    // be overlap, depending on how keys are arranged on the target layout.
    // But that is up to the designer.
    // mac_KMX_LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
    key->ShiftFlags &= ~VIRTUALCHARKEY;
    key->Key = vk;
  }
}

/**
 * @brief  translate a group of a keyboard
 * @param  group pointer to a keyboard group
 * @param  vk a keyvalue of the US keyboard
 * @param  shift shiftstate
 * @param  ch character of the underlying keyboard to be remapped
 * @return void
 */
void mac_KMX_TranslateGroup(LPKMX_GROUP group, KMX_WORD vk, UINT shift, KMX_WCHAR ch) {
  for (unsigned int i = 0; i < group->cxKeyArray; i++) {
    mac_KMX_TranslateKey(&group->dpKeyArray[i], vk, shift, ch);
  }
}

/**
 * @brief  translate  a keyboard
 * @param  kbd pointer to the US keyboard
 * @param  vk a keyvalue of the US keyboard
 * @param  shift shiftstate
 * @param  ch character of the underlying keyboard to be remapped
 * @return void
 */
void mac_KMX_TranslateKeyboard(LPKMX_KEYBOARD kbd, KMX_WORD vk, UINT shift, KMX_WCHAR ch) {
  for (unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if (kbd->dpGroupArray[i].fUsingKeys) {
      mac_KMX_TranslateGroup(&kbd->dpGroupArray[i], vk, shift, ch);
    }
  }
}

 /**
 * @brief  check key for unconverted key rules
 * @param  key pointer to a key
 * @return void
 */
void mac_KMX_ReportUnconvertedKeyRule(LPKMX_KEY key) {
  if (key->ShiftFlags == 0) {
    mac_KMX_LogError(L"Did not find a match for mnemonic rule on line %d, + '%c' > ...", key->Line, key->Key);
  } else if (key->ShiftFlags & VIRTUALCHARKEY) {
    mac_KMX_LogError(L"Did not find a match for mnemonic virtual character key rule on line %d, + [%x '%c'] > ...", key->Line, key->ShiftFlags, key->Key);
  }
}
 
/**
 * @brief  check a group for unconverted rules
 * @param  group pointer to a keyboard group
 * @return void
 */
void mac_KMX_ReportUnconvertedGroupRules(LPKMX_GROUP group) {
  for (unsigned int i = 0; i < group->cxKeyArray; i++) {
    mac_KMX_ReportUnconvertedKeyRule(&group->dpKeyArray[i]);
  }
}
 
/**
 * @brief  check a keyboard for unconverted rules
 * @param  kbd pointer to the US keyboard
 * @return void
 */
void mac_KMX_ReportUnconvertedKeyboardRules(LPKMX_KEYBOARD kbd) {
  for (unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if (kbd->dpGroupArray[i].fUsingKeys) {
      mac_KMX_ReportUnconvertedGroupRules(&kbd->dpGroupArray[i]);
    }
  }
}

/**
 * @brief  remap the content of a key (key->dpContext) of the US keyboard to a deadkey sequence
 * @param  key pointer to a key
 * @param  deadkey a deadkey to be remapped
 * @param  vk a keyvalue of the US keyboard
 * @param  shift shiftstate
 * @param  ch character of the underlying keyboard
 * @return void
 */
void mac_KMX_TranslateDeadkeyKey(LPKMX_KEY key, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch) {
   if ((key->ShiftFlags == 0 || key->ShiftFlags & VIRTUALCHARKEY) && key->Key == ch) {
    // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
    // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
    // to provide an alternate..
    if ((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))   // I4327
      shift &= ~LCTRLFLAG;

    if (key->ShiftFlags == 0) {
      //mac_KMX_LogError(L"Converted mnemonic rule on line %d, + '%c' TO dk(%d) + [%x K_%d]", key->Line, key->Key, deadkey, shift, vk);
      key->ShiftFlags = ISVIRTUALKEY | shift;
    } else {
      //mac_KMX_LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO dk(%d) + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, deadkey, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
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

/**
 * @brief  translate a group
 * @param  group pointer to a keyboard group
 * @param  a deadkey to be remapped
 * @param  vk a keyvalue of the US keyboard
 * @param  shift shiftstate
 * @param  character of the underlying keyboard
 * @return void
 */
void mac_KMX_TranslateDeadkeyGroup(LPKMX_GROUP group, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch) {
  for (unsigned int i = 0; i < group->cxKeyArray; i++) {
    mac_KMX_TranslateDeadkeyKey(&group->dpKeyArray[i], deadkey, vk, shift, ch);
  }
}

/**
 * @brief  translate a keyboard
 * @param  kbd pointer to the US keyboard
 * @param  a deadkey to be remapped
 * @param  vk a keyvalue of the US keyboard
 * @param  shift shiftstate
 * @param  character of the underlying keyboard
 * @return void
 */
void mac_KMX_TranslateDeadkeyKeyboard(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift, KMX_WORD ch) {
  for (unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if (kbd->dpGroupArray[i].fUsingKeys) {
      mac_KMX_TranslateDeadkeyGroup(&kbd->dpGroupArray[i], deadkey, vk, shift, ch);
    }
  }
}

/**
 * @brief  add a deadkey rule
 * @param  kbd pointer to the US keyboard
 * @param  deadkey a deadkey to be added
 * @param  vk a keyvalue of the US keyboard
 * @param  shift shiftstate
 * @return void
 */
void mac_KMX_AddDeadkeyRule(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey, KMX_WORD vk, UINT shift) {
  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if ((shift & (LCTRLFLAG | RALTFLAG)) == (LCTRLFLAG | RALTFLAG))  // I4549
    shift &= ~LCTRLFLAG;
  // If the first group is not a matching-keys group, then we need to add into
  // each subgroup, otherwise just the match group
  for (unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if (kbd->dpGroupArray[i].fUsingKeys) {
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
      if (i == kbd->StartGroup[1])
       break;  // If this is the initial group, that's all we need to do.
    }
  }
}

/**
 * @brief  find the maximal deadkey id
 * @param  str the deadkey
 * @return the maximum deadkey id
 */
KMX_WCHAR mac_KMX_ScanXStringForMaxDeadkeyID(PKMX_WCHAR str) {
  KMX_WCHAR dkid = 0;
  while(str && *str) {
    if (*str == UC_SENTINEL) {
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

/**
 * @brief  find the deadkey id for a given deadkey
 * @param  kbd pointer to the keyboard
 * @param  deadkey for which an id is to be found
 * @return 0 if failed; otherwise a deadkey-id
 */
KMX_WCHAR mac_KMX_GetUniqueDeadkeyID(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey) {
  LPKMX_GROUP gp;
  LPKMX_KEY kp;
  LPKMX_STORE sp;
  UINT i, j;
  KMX_WCHAR dkid = 0;
  static KMX_WCHAR s_next_dkid = 0;
  static KMX_dkidmap* s_dkids  = NULL;
  static int s_ndkids = 0;

  if (!kbd) {
    if (s_dkids) {
      delete s_dkids;
    }
    s_dkids = NULL;
    s_ndkids = 0;
    s_next_dkid = 0;
    return 0;
  }

  for (int i = 0; i < s_ndkids; i++) {
    if (s_dkids[i].src_deadkey == deadkey) {
      return s_dkids[i].dst_deadkey;
    }
  }

  if (s_next_dkid != 0) {
    s_dkids = (KMX_dkidmap*)realloc(s_dkids, sizeof(KMX_dkidmap) * (s_ndkids + 1));
    s_dkids[s_ndkids].src_deadkey = deadkey;
    return s_dkids[s_ndkids++].dst_deadkey = ++s_next_dkid;
  }

  for (i = 0, gp = kbd->dpGroupArray; i < kbd->cxGroupArray; i++, gp++) {
    for (j = 0, kp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kp++) {
      dkid = max(dkid, mac_KMX_ScanXStringForMaxDeadkeyID(kp->dpContext));
      dkid = max(dkid, mac_KMX_ScanXStringForMaxDeadkeyID(kp->dpOutput));
    }
    dkid = max(dkid, mac_KMX_ScanXStringForMaxDeadkeyID(gp->dpMatch));
    dkid = max(dkid, mac_KMX_ScanXStringForMaxDeadkeyID(gp->dpNoMatch));
  }

  for (i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    dkid = max(dkid, mac_KMX_ScanXStringForMaxDeadkeyID(sp->dpString));
  }

  s_dkids = (KMX_dkidmap*)realloc(s_dkids, sizeof(KMX_dkidmap) * (s_ndkids + 1));
  s_dkids[s_ndkids].src_deadkey = deadkey;
  return s_dkids[s_ndkids++].dst_deadkey = s_next_dkid = ++dkid;
}

/**
 * @brief  Lookup the deadkey table for the deadkey in the physical keyboard. Then for each character, go through and map it through
 * @param  kbd pointer to the keyboard
 * @param  vk_US virtual key of the us keyboard
 * @param  shift shiftstate
 * @param  deadkey character produced by a deadkey
 * @param  all_vector vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param  keymap pointer to the currently used (underlying) keyboard Layout
 * @param  dk_Table a vector of all possible deadkey combinations for all Linux keyboards
 * @return void
 */
void mac_KMX_ConvertDeadkey(LPKMX_KEYBOARD kbd, KMX_WORD vk_US, UINT shift, KMX_WCHAR deadkey, vec_dword_3D& all_vector, const UCKeyboardLayout* keyboard_layout) {
  KMX_WORD deadkeys[512] = {0};
  KMX_WORD* pdk;

  // Lookup the deadkey table for the deadkey in the physical keyboard
  // Then for each character, go through and map it through
  KMX_WCHAR dkid = mac_KMX_GetUniqueDeadkeyID(kbd, deadkey);
  // Add the deadkey to the mapping table for use in the import rules phase
  KMX_DeadkeyMapping KMX_deadkeyMapping = {deadkey, dkid, shift, vk_US};  // I4353

  KMX_FDeadkeys.push_back(KMX_deadkeyMapping);  // dkid, vk, shift);   // I4353
  mac_KMX_AddDeadkeyRule(kbd, dkid, vk_US, shift);

  mac_KMX_GetDeadkeys(keyboard_layout, all_vector, deadkey, shift, pdk = deadkeys);  // returns array of [usvk, ch_out] pairs

  while (*pdk) {
    // Look up the ch
    UINT KeyValUnderlying = mac_KMX_get_KeyValUnderlying_From_KeyValUS(all_vector, *pdk);
     mac_KMX_TranslateDeadkeyKeyboard(kbd, dkid, KeyValUnderlying, *(pdk + 1), *(pdk + 2));
    pdk += 3;
  }
}

 /**
 * @brief  convert a mnemonic keyboard to a positional keyboard
 *         (i.e. setting *sp->dpString = '0' / TSS_MNEMONIC=0)
 * @param  kbd pointer to keyboard
 * @return TRUE if conversion was successful; FALSE otherwise
 */
KMX_BOOL mac_KMX_SetKeyboardToPositional(LPKMX_KEYBOARD kbd) {
  LPKMX_STORE sp;
  UINT i;
  for (i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    if (sp->dwSystemID == TSS_MNEMONIC) {
      if (!sp->dpString) {
        mac_KMX_LogError(L"Invalid &mnemoniclayout system store");
        return FALSE;
      }
      if (u16cmp((const KMX_WCHAR*)sp->dpString, u"1") != 0) {
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

/** @brief  convert mnemonic keyboard layout to positional keyboard layout and translate keyboard */
KMX_BOOL mac_KMX_DoConvert(LPKMX_KEYBOARD kbd, KMX_BOOL bDeadkeyConversion, int argc) {

  KMX_WCHAR DeadKey=0;
  if (!mac_KMX_SetKeyboardToPositional(kbd))
    return FALSE;

  // Go through each of the shift states - base, shift, ctrl+alt, ctrl+alt+shift, [caps vs ncaps?]
  // Currently, we go in this order so the 102nd key works. But this is not ideal for keyboards without 102nd key:   // I4651
  // it catches only the first key that matches a given rule, but multiple keys may match that rule. This is particularly
  // evident for the 102nd key on UK, for example, where \ can be generated with VK_OEM_102 or AltGr+VK_QUOTE.
  // For now, we get the least shifted version, which is hopefully adequate.

  // Sadly it`s not e.g.:   on a German WINDOWS keyboard one will get '~' with  ALTGR + K_221(+) only which is the usual combination to get ~.
  //                        on a German MAC keyboard one will get '~' with either   OPT + K_221(+)    or     OPT + K_84(T)    or    CAPS + OPT + K_78(N)
  // K_84(T) will be caught first, so one of the the least obvious version for creating the '~' is found and processed.
  // -> meeting with Marc May 21 2024: We leave it as it is; it is OK if different combinations are found.

  const UCKeyboardLayout* keyboard_layout;
  if (mac_InitializeUCHR(&keyboard_layout)) {
      wprintf(L"ERROR: can't Initialize GDK\n");
      return FALSE;
  }
  // create vector that contains Keycode, Base, Shift for US-Keyboard and underlying keyboard
  vec_dword_3D all_vector;
  if (mac_createOneVectorFromBothKeyboards(all_vector, keyboard_layout)) {
    wprintf(L"ERROR: can't create one vector from both keyboards\n");
    return FALSE;
  }

 //printoutKeyboards(all_vector);
find_character_S2(keyboard_layout, 180);
printf("-------\n");
//print_dublicates_S2(keyboard_layout);
//find_double_keys(keyboard_layout,1);

  for (int j = 0; VKShiftState[j] != 0xFFFF; j++) {  // I4651

    // Loop through each possible key on the keyboard
    for (int i = 0; KMX_VKMap[i]; i++) {  // I4651

      // windows uses VK as sorting order in rgkey[], Linux and macOS use SC/Keycode as sorting order
      UINT scUnderlying = mac_KMX_get_KeyCodeUnderlying_From_VKUS(KMX_VKMap[i]);
      KMX_WCHAR ch = mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keyboard_layout, scUnderlying, VKShiftState[j], &DeadKey);

      // wprintf(L"--- VK_%d -> SC_ [%c] dk=%d  ( ss %i) \n", KMX_VKMap[i], ch == 0 ? 32 : ch, DeadKey, VKShiftState[j]);

      if (bDeadkeyConversion) {  // I4552
        if (ch == 0xFFFF) {
          ch = DeadKey;
        }
      }

      switch(ch) {
        case 0x0000: break;
        case 0xFFFF: mac_KMX_ConvertDeadkey(kbd, KMX_VKMap[i], VKShiftState[j], DeadKey, all_vector, keyboard_layout); break;
        default: mac_KMX_TranslateKeyboard(kbd, KMX_VKMap[i], VKShiftState[j], ch);
      }
    }
  }

  mac_KMX_ReportUnconvertedKeyboardRules(kbd);

  if (!mac_KMX_ImportRules(kbd, all_vector, &keyboard_layout, &KMX_FDeadkeys, bDeadkeyConversion)) {  // I4353   // I4552
    return FALSE;
  }

  return TRUE;
}


  /**
   * @brief  return an array of [usvk, ch_out] pairs: all existing combinations of a deadkey + character for the underlying keyboard
   * @param  keyboard_layout the currently used (underlying) keyboard Layout
   * @param  all_vector Vector that holds the data of the US keyboard and the currently used (underlying) keyboard
   * @param  deadkey deadkey character
   * @param  shift_dk shiftstate of the deadkey
   * @param  OutputPairs ptr to array of deadkeys
   * @return size of array of deadkeys
   */
 /** _S2 TODO thids is from linux: 
 * @brief  return an array of [usvk, ch_out] pairs: all existing combinations of a deadkey + character for the underlying keyboard
 * @param       deadkey deadkey character
 * @param [out] OutputPairs pointer to array of [usvk, ch_out] pairs
 * @param       keymap pointer to  the currently used (underlying) keyboard Layout
 * @param       dk_Table shiftstate of the deadkey
 * @return      size of array of [usvk, ch_out] pairs
 */
int mac_KMX_GetDeadkeys( const UCKeyboardLayout* keyboard_layout, vec_dword_3D& all_vector, KMX_WCHAR deadkey, UINT shift_dk, KMX_WORD* OutputPairs) {
  UInt32 deadkeystate;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  OptionBits keyTranslateOptions  = 0;
  UniChar unicodeString[maxStringlength];
  OSStatus status;
  unicodeString[0] = 0;

  KMX_WORD* p     = OutputPairs;
  KMX_DWORD sc_dk = mac_KMX_get_KeyCodeUnderlying_From_KeyValUnderlying(all_vector, deadkey);

  for (int j = 0; j < sizeof(ss_mac) / sizeof(ss_mac[0]); j++) {
    // we start with SPACE (keycode_spacebar=49) since all deadkeys occur in combinations with space.
    // Since mcompile finds only the first occurance of a dk combination, this makes sure that we always find the dk+SPACE combinations for a deadkey.
    // If there are more combinations to create a specific character they will not be found. (See comment at the beginning of mac_KMX_DoConvert())
    for (int i = keycode_spacebar; i >= 0; i--) {
      status = UCKeyTranslate(keyboard_layout, sc_dk, kUCKeyActionDown, mac_convert_Shiftstate_to_MacShiftstate(shift_dk), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

      if (deadkeystate != 0) {
        status = UCKeyTranslate(keyboard_layout, i, kUCKeyActionDown, ss_mac[j], LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

        if (deadkeystate != 0) {
          KMX_WORD vk = mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout, i, ss_mac[1], 0);

          // ensure to not get key combinations like ^a but only combined characters like â ( exception for ^+space)
          if ((unicodeString[0] != deadkey) || (vk == 32)) {
            *p++ = vk;
            *p++ = ss_mac[j];
            *p++ = unicodeString[0];
          }
        }
      }
    }
  }
  *p = 0;
  return (p - OutputPairs);
}

  /** @brief  print (error) messages */
  void mac_KMX_LogError(const wchar_t* fmt, ...) {
  WCHAR fmtbuf[256];
  const wchar_t* end = L"\0";
  const wchar_t* nl  = L"\n";
	va_list vars;
  int j = 0;

	va_start(vars, fmt);
  vswprintf (fmtbuf, _countof(fmtbuf), fmt, vars);
	fmtbuf[255] = 0;

  do {
    putwchar(fmtbuf[j]);
    j++;
  } while (fmtbuf[j] != *end);
  putwchar(*nl);
}




//################################################################################################################################################
//################################# Code beyond these lines needs to be included in mcompile #####################################################
//################################################################################################################################################

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
      if ( i%3 ==0 )           printf("as character \n");
      if ( i%3 ==1 )           printf("as shiftstate \n");
      if ( i%3 ==2 )           printf("as combined character \n");
      return true;
    }
  }
  return false;
}
bool test_dk_write_entries_S2(KMX_WORD deadkeys[512]) {
  for ( int i=0; i< 512/3;i++) {
   if ( deadkeys[3*i] !=0)
      printf(" %i set nr \t%i:  %i\t-\t%i(%c)\t-\t%i(%c)  \n", deadkeys[2], i,deadkeys[3*i+1], deadkeys[3*i], deadkeys[3*i], deadkeys[3*i+2], deadkeys[3*i+2]);
  }
  return true;
}

bool find_print_character_S2(const UCKeyboardLayout * keyboard_layout, int keyval, int k, int c, int s) {
  int ss_rgkey;
int count =0;
bool moreval=false;
  std::vector<std::string> res_vec;

  for ( int key=0; key< 50;key++) {
    for ( int caps=0; caps< 2;caps++) {
      for ( int ss=0; ss< 10;ss++) {

        if ( ss == 0 )   ss_rgkey= 0;
        else if ( ss == 2 )   ss_rgkey= 1;
        else if ( ss == 8 )   ss_rgkey= 6;
        else if ( ss == 10 )   ss_rgkey= 7;
        else  ss_rgkey= 999;
if ( (ss==1) || (ss==3) || (ss==5) || (ss==7) || (ss==9) )
continue;

          int keyvalsearch= mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout,  key,  ss,  caps);

          count++;
          if ((ss_rgkey!= 999 )&& ( keyval!= 0)&& ( keyval== keyvalsearch)&& ( count>1)&& ( key!=k)) {
          printf( "                                        key: %i, ss_mac: %i ( ss_rgkey:%i),  caps:%i  \n",key, ss, ss_rgkey, caps);
          moreval=true;
          }
      }
    }
  }
  return moreval;
}
bool print_dublicates_S2(const UCKeyboardLayout * keyboard_layout){
  for ( int key=0; key< 50;key++) {
      for ( int caps=0; caps< 2;caps++) {
        for ( int ss=0; ss< 6;ss++) {
          int keyval= mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout,  key,  2*ss,  caps);
          if (find_print_character_S2(keyboard_layout, keyval, key,caps,ss) && (keyval>0)) {
          printf( "Keyval %i(%c): can be found on :  \tkey: %i, ss-mac: %i                 caps:%i\t:\n---------------------------------------------------------\n",keyval,keyval, key, 2*ss,caps);
          }
        }
      }
    }


  return true;
}
bool print_character_of_key_S2(const UCKeyboardLayout * keyboard_layout, int key) {
int ss_rgkey;
  for ( int ss=0; ss< 6;ss++) {
    for ( int caps=0; caps< 2;caps++) {

          int keyvalsearch= mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout,  key,  2*ss,  caps);
          printf( " key %i has values : %i (%c) ( ss:%i, caps:%i) \n", key,  keyvalsearch,keyvalsearch, 2*ss, caps);
    }
  }
  return true;
}
bool find_character_S2(const UCKeyboardLayout * keyboard_layout, int keyval) {
int ss_rgkey;
  for ( int ss=0; ss< 10;ss++) {

if ( ss == 0 )   ss_rgkey= 0;
else if ( ss == 2 )   ss_rgkey= 1;
else if ( ss == 8 )   ss_rgkey= 6;
else if ( ss == 10 )   ss_rgkey= 7;
else  ss_rgkey= 999;

if ( (ss==1) || (ss==3) || (ss==5) || (ss==7) || (ss==9)  )
continue;

    for ( int caps=0; caps< 2;caps++) {
      for ( int key=0; key< 50;key++) {
          int keyvalsearch= mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout,  key,  mac_map_rgkey_ShiftState_to_Shiftstate(ss),  caps);
          if ((ss_rgkey!= 999 )&& ( keyval== keyvalsearch))
          printf( " found keyval: key: %i, ss_mac:%i ( ss_rgkey:%i),  caps:%i  -> character: %i (%c)  \n", key, ss, ss_rgkey, caps, keyvalsearch,keyvalsearch);
      }
    }
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

