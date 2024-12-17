/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Mnemonic layout support for linux
 *
 * Defines the entry point for the console application.
 *
 * Note: this program deliberately leaks memory as it has a very short life cycle and managing the memory allocations
 * for the subcomponents of the compiled keyboard is an unnecessary optimisation. Just so you know.
*/

#include "mcompile.h"

const int nr_DK_pairs = 1000;
static const int size_DK_array = (nr_DK_pairs + 1) *3;

/** @brief convert mnemonic keyboard layout to positional keyboard layout and translate keyboard */
KMX_BOOL KMX_DoConvert(LPKMX_KEYBOARD kbd, KMX_BOOL bDeadkeyConversion, gint argc, gchar* argv[]);

/** @brief Collect the key data, translate it to kmx and append to the existing keyboard */
bool KMX_ImportRules(LPKMX_KEYBOARD kp, vec_dword_3D& all_vector, GdkKeymap** keymap, std::vector<KMX_DeadkeyMapping>* KMX_FDeadkeys, KMX_BOOL bDeadkeyConversion);  // I4353   // I4327

/** @brief  return a vector of [usvk, ch_out] pairs: all existing combinations of a deadkey + character for the underlying keyboard */
int KMX_GetDeadkeys(vec_dword_2D& dk_Table, KMX_WORD deadkey, std::vector<KMX_WORD> &dk_vec, GdkKeymap* keymap);

std::vector<KMX_DeadkeyMapping> KMX_FDeadkeys;  // I4353

#define _countof(a) (sizeof(a) / sizeof(*(a)))

/**
 * @brief  main function for mcompile for Linux
 * @param  argc number of commandline arguments
 * @param  argv pointer to commandline arguments: executable, inputfile, outputfile
 * @return 0 on success
 */

  int main(int argc, char* argv[]) {


  int bDeadkeyConversion = 0;

  if (argc > 1)
    bDeadkeyConversion = (strcmp(argv[1], "-d") == 0);  // I4552

  int n = (bDeadkeyConversion ? 2 : 1);

  if (argc < 3 || argc > 4 || (argc - n) != 2) {  // I4273// I4273
    printf(
        "Usage:  \tmcompile [-d] infile.kmx outfile.kmx\n"
        "        \tmcompile converts a Keyman mnemonic layout to\n"
        "        \ta positional one based on the currently used \n"
        "        \tLinux keyboard layout\n"
        "        \t(-d convert deadkeys to plain keys) \n \n");  // I4552

    return 1;
  }

  // -u option is not available for Linux and macOS

  KMX_CHAR* infile = argv[n];
  KMX_CHAR* outfile = argv[n + 1];

  printf("mcompile%s \"%s\" \"%s\"\n", bDeadkeyConversion ? " -d" : "", infile, outfile);  // I4174

  // 1. Load the keyman keyboard file

  // 2. For each key on the system layout, determine its output character and perform a
  //    1-1 replacement on the keyman keyboard of that character with the base VK + shift
  //    state.  This fixup will transform the char to a vk, which will avoid any issues
  //    with the key.
  //
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
    KMX_LogError(L"Failed to load keyboard (%d)\n", errno);
    return 3;
  }

  if (KMX_DoConvert(kmxfile, bDeadkeyConversion, argc, (gchar**)argv)) {  // I4552F
    if(!KMX_SaveKeyboard(kmxfile, outfile)) {
      KMX_LogError(L"Failed to save keyboard (%d)\n", errno);
      return 3;
    }
  }

  delete kmxfile;
  return 0;
}

// Map of all shift states that we will work with
const KMX_DWORD VKShiftState[] = {0, K_SHIFTFLAG, LCTRLFLAG | RALTFLAG, K_SHIFTFLAG | LCTRLFLAG | RALTFLAG, 0xFFFF};

//
// TranslateKey
//
// For each key rule on the keyboard, remap its key to the
// correct shift state and key.  Adjust the LCTRL+RALT -> RALT if necessary
//

/**
 * @brief  translate each key of a group: remap the content of a key (key->Key) of the US keyboard to a character (ch)
 * @param  key   pointer to a key
 * @param  vk    a keyvalue of the US keyboard
 * @param  shift shiftstate
 * @param  ch    character of the underlying keyboard to be remapped
 */
void KMX_TranslateKey(LPKMX_KEY key, KMX_WORD vk, KMX_DWORD shift, KMX_WCHAR ch) {
  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if ((shift & (LCTRLFLAG | RALTFLAG)) == (LCTRLFLAG | RALTFLAG))
    shift &= ~LCTRLFLAG;

  if (key->ShiftFlags == 0 && key->Key == ch) {
    // Key is a mnemonic key with no shift state defined.
    // Remap the key according to the character on the key cap.
    // KMX_LogError(L"Converted mnemonic rule on line %d, + '%c' TO + [%x K_%d]", key->Line, key->Key, shift, vk);
    key->ShiftFlags = ISVIRTUALKEY | shift;
    key->Key = vk;
  } else if (key->ShiftFlags & VIRTUALCHARKEY && key->Key == ch) {
    // Key is a virtual character key with a hard-coded shift state.
    // Do not remap the shift state, just move the key.
    // This will not result in 100% wonderful mappings as there could
    // be overlap, depending on how keys are arranged on the target layout.
    // But that is up to the designer.
    // KMX_LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
    key->ShiftFlags &= ~VIRTUALCHARKEY;
    key->Key = vk;
  }
}

/**
 * @brief  translate a group of a keyboard
 * @param  group pointer to a keyboard group
 * @param  vk    a keyvalue of the US keyboard
 * @param  shift shiftstate
 * @param  ch    character of the underlying keyboard to be remapped
 */
void KMX_TranslateGroup(LPKMX_GROUP group, KMX_WORD vk, KMX_DWORD shift, KMX_WCHAR ch) {
  for (unsigned int i = 0; i < group->cxKeyArray; i++) {
    KMX_TranslateKey(&group->dpKeyArray[i], vk, shift, ch);
  }
}

/**
 * @brief  translate  a keyboard
 * @param  kbd   pointer to the US keyboard
 * @param  vk    a keyvalue of the US keyboard
 * @param  shift shiftstate
 * @param  ch    character of the underlying keyboard to be remapped
 */
void KMX_TranslateKeyboard(LPKMX_KEYBOARD kbd, KMX_WORD vk, KMX_DWORD shift, KMX_WCHAR ch) {
  for (unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if (kbd->dpGroupArray[i].fUsingKeys) {
      KMX_TranslateGroup(&kbd->dpGroupArray[i], vk, shift, ch);
    }
  }
}

/**
 * @brief  check key for unconverted key rules
 * @param  key pointer to a key
 */
void KMX_ReportUnconvertedKeyRule(LPKMX_KEY key) {
  if (key->ShiftFlags == 0) {
    // KMX_LogError(L"Did not find a match for mnemonic rule on line %d, + '%c' > ...", key->Line, key->Key);
  } else if (key->ShiftFlags & VIRTUALCHARKEY) {
    KMX_LogError(L"Did not find a match for mnemonic virtual character key rule on line %d, + [%x '%c'] > ...", key->Line, key->ShiftFlags, key->Key);
  }
}

/**
 * @brief  check a group for unconverted rules
 * @param  group pointer to a keyboard group
 */
void KMX_ReportUnconvertedGroupRules(LPKMX_GROUP group) {
  for (unsigned int i = 0; i < group->cxKeyArray; i++) {
    KMX_ReportUnconvertedKeyRule(&group->dpKeyArray[i]);
  }
}

/**
 * @brief  check a keyboard for unconverted rules
 * @param  kbd pointer to the US keyboard
 */
void KMX_ReportUnconvertedKeyboardRules(LPKMX_KEYBOARD kbd) {
  for (unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if (kbd->dpGroupArray[i].fUsingKeys) {
      KMX_ReportUnconvertedGroupRules(&kbd->dpGroupArray[i]);
    }
  }
}

/**
 * @brief  remap the content of a key (key->dpContext) of the US keyboard to a deadkey sequence
 * @param  key     pointer to a key
 * @param  deadkey a deadkey to be remapped
 * @param  vk      a keyvalue of the US keyboard
 * @param  shift   shiftstate
 * @param  ch      character of the underlying keyboard
 */
void KMX_TranslateDeadkeyKey(LPKMX_KEY key, KMX_WCHAR deadkey, KMX_WORD vk, KMX_DWORD shift, KMX_WORD ch) {

  if ((key->ShiftFlags == 0 || key->ShiftFlags & VIRTUALCHARKEY) && key->Key == ch) {
    // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
    // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
    // to provide an alternate..
    if ((shift & (LCTRLFLAG | RALTFLAG)) == (LCTRLFLAG | RALTFLAG))  // I4327
      shift &= ~LCTRLFLAG;

    if (key->ShiftFlags == 0) {
      // KMX_LogError(L"Converted mnemonic rule on line %d, + '%c' TO dk(%d) + [%x K_%d]", key->Line, key->Key, deadkey, shift, vk);
      key->ShiftFlags = ISVIRTUALKEY | shift;
    } else {
      // KMX_LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO dk(%d) + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, deadkey, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
      key->ShiftFlags &= ~VIRTUALCHARKEY;
    }

    int len = u16len(key->dpContext);

    PKMX_WCHAR context = new KMX_WCHAR[len + 4];
    memcpy(context, key->dpContext, len * sizeof(KMX_WCHAR));
    context[len] = UC_SENTINEL;
    context[len + 1] = CODE_DEADKEY;
    context[len + 2] = deadkey;
    context[len + 3] = 0;
    key->dpContext = context;
    key->Key = vk;
  }
}

/**
 * @brief  translate a group
 * @param  group   pointer to a keyboard group
 * @param  deadkey deadkey to be remapped
 * @param  vk      a keyvalue of the US keyboard
 * @param  shift   shiftstate
 * @param  ch      character of the underlying keyboard
 */
void KMX_TranslateDeadkeyGroup(LPKMX_GROUP group, KMX_WCHAR deadkey, KMX_WORD vk, KMX_DWORD shift, KMX_WORD ch) {
  for (unsigned int i = 0; i < group->cxKeyArray; i++) {
    KMX_TranslateDeadkeyKey(&group->dpKeyArray[i], deadkey, vk, shift, ch);
  }
}

/**
 * @brief  translate a keyboard
 * @param  kbd     pointer to the US keyboard
 * @param  deadkey a deadkey to be remapped
 * @param  vk      a keyvalue of the US keyboard
 * @param  shift   shiftstate
 * @param  ch      character of the underlying keyboard
 */
void KMX_TranslateDeadkeyKeyboard(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey, KMX_WORD vk, KMX_DWORD shift, KMX_WORD ch) {
  for (unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if (kbd->dpGroupArray[i].fUsingKeys) {
      KMX_TranslateDeadkeyGroup(&kbd->dpGroupArray[i], deadkey, vk, shift, ch);
    }
  }
}

/**
 * @brief  add a deadkey rule
 * @param  kbd     pointer to the US keyboard
 * @param  deadkey a deadkey to be added
 * @param  vk      a keyvalue of the US keyboard
 * @param  shift   shiftstate
 */
void KMX_AddDeadkeyRule(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey, KMX_WORD vk, KMX_DWORD shift) {
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
      memcpy(keys + 1, kbd->dpGroupArray[i].dpKeyArray, kbd->dpGroupArray[i].cxKeyArray * sizeof(KMX_KEY));
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
KMX_WCHAR KMX_ScanXStringForMaxDeadkeyID(PKMX_WCHAR str) {
  KMX_WCHAR dkid = 0;
  while (str && *str) {
    if (*str == UC_SENTINEL && *(str + 1) == CODE_DEADKEY) {
      dkid = std::max(dkid, *(str + 2));
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
 * @param  kbd     pointer to the keyboard
 * @param  deadkey for which an id is to be found
 * @return 0 if failed;
 *         otherwise a deadkey-id
 */
KMX_WCHAR KMX_GetUniqueDeadkeyID(LPKMX_KEYBOARD kbd, KMX_WCHAR deadkey) {
  LPKMX_GROUP gp;
  LPKMX_KEY kp;
  LPKMX_STORE sp;
  KMX_DWORD i, j;
  KMX_WCHAR dkid = 0;
  static KMX_WCHAR s_next_dkid = 0;
  static KMX_dkidmap* s_dkids = NULL;
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

  for (int xi = 0; xi < s_ndkids; xi++) {
    if (s_dkids[xi].src_deadkey == deadkey) {
      return s_dkids[xi].dst_deadkey;
    }
  }

  if (s_next_dkid != 0) {
    s_dkids = (KMX_dkidmap*)realloc(s_dkids, sizeof(KMX_dkidmap) * (s_ndkids + 1));
    s_dkids[s_ndkids].src_deadkey = deadkey;
    return s_dkids[s_ndkids++].dst_deadkey = ++s_next_dkid;
  }

  for (i = 0, gp = kbd->dpGroupArray; i < kbd->cxGroupArray; i++, gp++) {
    for (j = 0, kp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kp++) {
      dkid = std::max(dkid, KMX_ScanXStringForMaxDeadkeyID(kp->dpContext));
      dkid = std::max(dkid, KMX_ScanXStringForMaxDeadkeyID(kp->dpOutput));
    }
    dkid = std::max(dkid, KMX_ScanXStringForMaxDeadkeyID(gp->dpMatch));
    dkid = std::max(dkid, KMX_ScanXStringForMaxDeadkeyID(gp->dpNoMatch));
  }

  for (i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    dkid = std::max(dkid, KMX_ScanXStringForMaxDeadkeyID(sp->dpString));
  }

  s_dkids = (KMX_dkidmap*)realloc(s_dkids, sizeof(KMX_dkidmap) * (s_ndkids + 1));
  s_dkids[s_ndkids].src_deadkey = deadkey;
  return s_dkids[s_ndkids++].dst_deadkey = s_next_dkid = ++dkid;
}

/**
 * @brief  Lookup the deadkey table for the deadkey in the physical keyboard. Then for each character, go through and map it through
 * @param  kbd        pointer to the keyboard
 * @param  vk_US      virtual key of the us keyboard
 * @param  shift      shiftstate
 * @param  deadkey    character produced by a deadkey
 * @param  all_vector vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param  keymap     pointer to the currently used (underlying) keyboard Layout
 * @param  dk_Table   a vector of all possible deadkey combinations for all Linux keyboards
 */
void KMX_ConvertDeadkey(LPKMX_KEYBOARD kbd, KMX_WORD vk_US, KMX_DWORD shift, KMX_WCHAR deadkey, vec_dword_3D& all_vector, GdkKeymap* keymap, vec_dword_2D dk_Table) {

  std::vector<KMX_WORD> vec_deadkeys;

  // Lookup the deadkey table for the deadkey in the physical keyboard
  // Then for each character, go through and map it through
  KMX_WCHAR dkid = KMX_GetUniqueDeadkeyID(kbd, deadkey);

  // Add the deadkey to the mapping table for use in the import rules phase
  KMX_DeadkeyMapping KMX_deadkeyMapping = {deadkey, dkid, shift, vk_US};  // I4353

  KMX_FDeadkeys.push_back(KMX_deadkeyMapping);  // dkid, vk, shift);   // I4353
  KMX_AddDeadkeyRule(kbd, dkid, vk_US, shift);

  KMX_GetDeadkeys(dk_Table, deadkey, vec_deadkeys, keymap);  // returns vector of [usvk, ch_out] pairs

  int n=0;
  while (n < (int)vec_deadkeys.size()) {
    // Look up the ch
    KMX_DWORD KeyValUnderlying = (KMX_DWORD) KMX_get_KeyValUnderlying_From_KeyValUS(all_vector, vec_deadkeys[n]);
    KMX_TranslateDeadkeyKeyboard(kbd, dkid, KeyValUnderlying, vec_deadkeys[n + 1], vec_deadkeys[n + 2]);
    n += 3;
  }
}

/**
 * @brief  convert a mnemonic keyboard to a positional keyboard
 *         (i.e. setting *sp->dpString = '0' / TSS_MNEMONIC=0)
 * @param  kbd pointer to keyboard
 * @return TRUE if conversion was successful;
 *         FALSE otherwise
 */
KMX_BOOL KMX_SetKeyboardToPositional(LPKMX_KEYBOARD kbd) {
  LPKMX_STORE sp;
  KMX_DWORD i;
  for (i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    if (sp->dwSystemID == TSS_MNEMONIC) {
      if (!sp->dpString) {
        KMX_LogError(L"Invalid &mnemoniclayout system store");
        return FALSE;
      }
      if (u16cmp((const KMX_WCHAR*)sp->dpString, u"1") != 0) {
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

/**
 * @brief  convert mnemonic keyboard layout to positional keyboard layout and translate keyboard
 * @param  kbd                pointer to US keyboard
 * @param  bDeadkeyConversion option for converting a deadkey to a character: 1 = dk conversion; 0 = no dk conversion
 * @param  argc               number of command line arguments
 * @param  argv               pointer to command line arguments
 * @return TRUE if conversion was successful;
 *         FALSE if not
 */
KMX_BOOL KMX_DoConvert(LPKMX_KEYBOARD kbd, KMX_BOOL bDeadkeyConversion, gint argc, gchar* argv[]) {
  KMX_WCHAR DeadKey = 0;

  if (!KMX_SetKeyboardToPositional(kbd))
    return FALSE;

  // Go through each of the shift states - base, shift, ctrl+alt, ctrl+alt+shift, [caps vs ncaps?]
  // Currently, we go in this order so the 102nd key works. But this is not ideal for keyboards without 102nd key:   // I4651
  // it catches only the first key that matches a given rule, but multiple keys may match that rule. This is particularly
  // evident for the 102nd key on UK, for example, where \ can be generated with VK_OEM_102 or AltGr+VK_QUOTE.
  // For now, we get the least shifted version, which is hopefully adequate.

  GdkKeymap* keymap;
  if (InitializeGDK(&keymap, argc, argv)) {
    printf("ERROR: can't Initialize GDK\n");
    return FALSE;
  }

  // create vector that contains Keycode, base, shift for US-KEyboard and underlying keyboard
  vec_dword_3D all_vector;
  if (createOneVectorFromBothKeyboards(all_vector, keymap)) {
    printf("ERROR: can't create one vector from both keyboards\n");
    return FALSE;
  }

  vec_dword_2D dk_Table;
  create_DKTable(dk_Table);

  for (int j = 0; VKShiftState[j] != 0xFFFF; j++) {  // I4651

    // Loop through each possible key on the keyboard
    for (int i = 0; KMX_VKMap[i]; i++) {  // I4651

      // windows uses  VK, Linux uses SC/Keycode
      KMX_DWORD scUnderlying = (KMX_DWORD)KMX_get_KeyCodeUnderlying_From_VKUS(KMX_VKMap[i]);
      KMX_WCHAR ch = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keymap, scUnderlying, VKShiftState[j], &DeadKey);

      // printf("--- VK_%d -> SC_ [%c] dk=%d  ( ss %i) \n", KMX_VKMap[i], ch == 0 ? 32 : ch, DeadKey, VKShiftState[j]);

      if (bDeadkeyConversion) {  // I4552
        if (ch == 0xFFFF) {
          ch = DeadKey;
        }
      }

      switch (ch) {
        case 0x0000: break;
        case 0xFFFF: KMX_ConvertDeadkey(kbd, KMX_VKMap[i], VKShiftState[j], DeadKey, all_vector, keymap, dk_Table); break;
        default: KMX_TranslateKeyboard(kbd, KMX_VKMap[i], VKShiftState[j], ch);
      }
    }
  }

  KMX_ReportUnconvertedKeyboardRules(kbd);

  if (!KMX_ImportRules(kbd, all_vector, &keymap, &KMX_FDeadkeys, bDeadkeyConversion)) {  // I4353   // I4552
    return FALSE;
  }
  return TRUE;
}

/**
 * @brief  return a vector of [usvk, ch_out] pairs: all existing combinations of a deadkey + character for the underlying keyboard
 * @param      dk_Table    shiftstate of the deadkey
 * @param      deadkey     deadkey character
 * @param[out] dk_vec vector of [usvk, ch_out] pairs
 * @param      keymap      pointer to  the currently used (underlying) keyboard Layout
 * @return size of array of [usvk, ch_out] pairs
 */

int KMX_GetDeadkeys(vec_dword_2D& dk_Table, KMX_WORD deadkey, std::vector<KMX_WORD> &dk_vec, GdkKeymap* keymap) {

  KMX_DWORD shift;
  vec_dword_2D dk_SingleTable;

  query_dk_combinations_for_specific_dk(dk_Table, deadkey, dk_SingleTable);
  for (int i = 0; i < (int)dk_SingleTable.size(); i++) {
    KMX_WORD vk = KMX_change_keyname_to_capital(dk_SingleTable[i][1], shift, keymap);
    if (vk != 0) {

        dk_vec.push_back(vk);
        dk_vec.push_back(shift);
        dk_vec.push_back(dk_SingleTable[i][2]);

    } else {
      KMX_LogError(L"Warning: complex deadkey not supported.");
    }
  }

  return dk_vec.size();
}

/**
 * @brief  print (error) messages
 * @param  fmt text to print
 */
void KMX_LogError(const wchar_t* fmt, ...) {
  wchar_t fmtbuf[256];
  const wchar_t* end = L"\0";
  const wchar_t* nl  = L"\n";
  va_list vars;
  int j = 0;

  va_start(vars, fmt);
  vswprintf(fmtbuf, _countof(fmtbuf), fmt, vars);
  fmtbuf[255] = 0;

  do {
    putwchar(fmtbuf[j]);
    j++;
  } while (fmtbuf[j] != *end);
  putwchar(*nl);
}
