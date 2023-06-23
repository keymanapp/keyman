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
//
// m-to-p.cpp : Defines the entry point for the console application.
//
// Note: this program deliberately leaks memory as it has a very short life cycle and managing the memory allocations
// for the subcomponents of the compiled keyboard is an unnecessary optimisation. Just so you know.
//

#include "pch.h"
#include <vector>

#include <stdio.h>
#include <stdarg.h>
#include <varargs.h>

BOOL DoConvert(LPKEYBOARD kbd, PWSTR kbid, BOOL bDeadkeyConversion);
BOOL SaveKeyboard(LPKEYBOARD kbd, PWSTR filename);
BOOL CopyKeyboard(PWSTR source, PWSTR target);
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

    if (kmxfile->dwFlags & KF_KMXPLUS) {
      LogError(L"Error: Don't use the -u option with KMXPlus files.");
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

  if (kmxfile->dwFlags & KF_KMXPLUS) {
    if(!CopyKeyboard(infile, outfile)) {
      LogError(L"Failed to copy KMXPlus keyboard file (%d)", GetLastError());
      return 3;
    }
  } else {
    if(DoConvert(kmxfile, kbid, bDeadkeyConversion)) {   // I4552
      SaveKeyboard(kmxfile, outfile);
    }
  }

  //DeleteReallocatedPointers(kmxfile); :TODO
  delete kmxfile;

	return 0;
}


//
// Map of all US English virtual key codes that we can translate
//
const WORD VKMap[] = {
  'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
  '0','1','2','3','4','5','6','7','8','9',
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

BOOL CopyKeyboard(PWSTR source, PWSTR target)
{
  return CopyFile(source, target, FALSE /* bFailIfExists */); // overwrite file if it exists
}
