/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "pch.h"
//#include <io.h>
//#include <fcntl.h>
#include <stdarg.h>
#include <string>
#include "kmxtest.h"

struct KMXTest_KeyEvent {
  KMX_UINT vkey;
  KMX_UINT modifiers;
};

struct KMXTest_ModifierNames {
  char *name;
  KMX_UINT flag;
};

struct KMXTest_ChToVKey {
  KMX_UINT vkey;
  KMX_BOOL shifted;
};

/* Globals */

int g_nKeyEvents = 0;
KMXTest_KeyEvent g_keyEvents[1024] = { 0 };
KMX_BOOL g_debug_ToConsole = TRUE, g_debug_KeymanLog = TRUE;
KMX_BOOL g_silent = FALSE;

/* Context - to refactor */
KMX_WCHAR g_context[512] = u"";
int g_contextLength = 0;

/* Expected output */
KMX_WCHAR g_expectedOutput[512] = u"";
int g_expectedOutputLength = 0;

/* Keyboard options - to refactor */
KMXTest_KeyboardOption g_keyboardOption[1024] = { 0 };
int g_keyboardOptionCount = 0;

/* Environment - to refactor */
KMX_Environment g_environment = {
  FALSE, // simulateAltGr
  FALSE, // baseLayoutGivesCtrlRAltForRAlt
  u"kbdus.dll", // baseLayout
  u"en-US", // baseLayoutAlt
  FALSE, // capsLock
  u"windows desktop hardware native" // platform
};

/* Constants - to refactor */
extern const struct KMXTest_ModifierNames s_modifierNames[];
extern const char *VKeyNames[];
extern const struct KMXTest_ChToVKey chToVKey[];

void print_default_environment() {
  wprintf(L"  env.simulate_altgr=%d\n", g_environment.simulateAltGr);
  wprintf(L"  env.base_layout_gives_ctrl_ralt_for_ralt=%d\n", g_environment.baseLayoutGivesCtrlRAltForRAlt);
  wprintf(L"  env.base_layout=%s\n", g_environment.baseLayout.c_str());
  wprintf(L"  env.base_layout_alt=%s\n", g_environment.baseLayoutAlt.c_str());
  wprintf(L"  env.platform=%hs\n", g_environment.platform.c_str());
  wprintf(L"  env.caps_lock=%d\n", g_environment.capsLock);
}

void ValidateOptions();

KMX_BOOL addKeyboardOption(char *storeName, char *value) {
  PKMX_WCHAR p = strtowstr(storeName);
  std::u16string ps(p);
  size_t sz = ps.copy(g_keyboardOption[g_keyboardOptionCount].name, sizeof(g_keyboardOption[g_keyboardOptionCount].name) / sizeof(g_keyboardOption[g_keyboardOptionCount].name[0]));
  g_keyboardOption[g_keyboardOptionCount].name[sz] = 0;
  //wcscpy_s(g_keyboardOption[g_keyboardOptionCount].name, p);
  delete p;
  
  p = strtowstr(value);
  ps.assign(p);
  sz = ps.copy(g_keyboardOption[g_keyboardOptionCount].value, sizeof(g_keyboardOption[g_keyboardOptionCount].value) / sizeof(g_keyboardOption[g_keyboardOptionCount].value[0]));
  g_keyboardOption[g_keyboardOptionCount].value[sz] = 0;
  //wcscpy_s(g_keyboardOption[g_keyboardOptionCount++].value, p);
  delete p;
  return TRUE;
}

KMX_BOOL addOption(char *val) {
  char *p = strchr(val, '=');
  if (!p) {
    return FALSE;
  }
  int len = (int)(p - val);
  p++;
  PKMX_WCHAR wp = strtowstr(p);

  if (!strncasecmp(val, "env.simulate_altgr", len)) {
    g_environment.simulateAltGr = !!atoi(p);
  }
  else if (!strncasecmp(val, "env.base_layout_gives_ctrl_ralt_for_ralt", len)) {
    g_environment.baseLayoutGivesCtrlRAltForRAlt = !!atoi(p);
  }
  else if (!strncasecmp(val, "env.base_layout", len)) {
    //std::u16string c(wp);
    g_environment.baseLayout.assign(wp);
    //c.copy(g_environment.baseLayout,  wcscpy_s(g_environment.baseLayout, wp);
  }
  else if (!strncasecmp(val, "env.base_layout_alt", len)) {
    g_environment.baseLayoutAlt.assign(wp);
    //wcscpy_s(g_environment.baseLayoutAlt, wp);
  }
  else if (!strncasecmp(val, "env.platform", len)) {
    g_environment.platform.assign(wp);
    //strcpy_s(g_environment.platform, p);
  }
  else if (!strncasecmp(val, "env.caps_lock", len)) {
    g_environment.capsLock = !!atoi(p);
  }
  else if (!strncasecmp(val, "opt.", 4)) {
    *(p - 1) = 0;
    if (!addKeyboardOption(val + 4, p)) {
      *(p - 1) = '=';
      return FALSE;
    }
    *(p - 1) = '=';
  }
  else {
    return FALSE;
  }
  delete wp;
  return TRUE;
}

KMX_BOOL addKey(char *val, int len) {
  char *buf = new char[len + 1];
  strncpy(buf, val, len);
  buf[len] = 0;
  val = buf;

  KMX_DWORD ShiftState = 0;
  char *p = strchr(val, ' ');
  while (p && *p) {
    int i;
    for (i = 0; s_modifierNames[i].name; i++) {
      size_t len = strlen(s_modifierNames[i].name);
      if (len < (size_t)(p - val)) len = (size_t)(p - val);
      if (!strncasecmp(val, s_modifierNames[i].name, len)) {
        ShiftState |= s_modifierNames[i].flag;
        break;
      }
    }
    if (!s_modifierNames[i].name) {
      console_error(L"invalid modifier: [%hs]\n", buf);
      delete buf;
      return FALSE;
    }
    val = p + 1;
    p = strchr(val, ' ');
  }
  for (int i = 0; i < 256; i++) {
    if (!strcasecmp(val, VKeyNames[i])) {
      g_keyEvents[g_nKeyEvents].vkey = i;
      g_keyEvents[g_nKeyEvents++].modifiers = ShiftState;
      delete buf;
      return TRUE;
    }
  }
  console_error(L"invalid key: [%hs]\n", buf);
  delete buf;
  return FALSE;
}

char VKeyToChar(KMX_UINT modifiers, KMX_UINT vk) {
  // We only map SHIFT and UNSHIFTED
  // TODO: Map CAPS LOCK correctly
  if (modifiers != 0 && modifiers != K_SHIFTFLAG) {
    return 0;
  }

  KMX_BOOL shifted = modifiers == K_SHIFTFLAG ? 1 : 0;

  if (vk == VK_SPACE) {
    // Override for space because it is the same for
    // shifted and unshifted.
    return 32;
  }

  for (int i = 0; chToVKey[i].vkey; i++) {
    if (chToVKey[i].vkey == vk && chToVKey[i].shifted == shifted) {
      return i + 32;
    }
  }
  return 0;
}

KMX_BOOL addKey(char ch) {
  if (ch > 127 || ch < 32) return FALSE;
  g_keyEvents[g_nKeyEvents].vkey = chToVKey[ch-32].vkey;
  g_keyEvents[g_nKeyEvents++].modifiers = chToVKey[ch-32].shifted ? K_SHIFTFLAG : 0;
  return TRUE;
}

KMX_BOOL setKeys(char *val) {
  while (*val) {
    if (*val == '[') {
      val++;
      if (*val == '[') {
        if (!addKey(*val)) return FALSE;
        val++;
      }
      else {
        char *p = strchr(val, ']');
        if (!p) {
          return FALSE;
        }
        if (!addKey(val, (int)(p - val))) return FALSE;
        val = p + 1;
      }
    }
    else if (*val >= 32 && *val < 127) {
      if (!addKey(*val)) return FALSE;
      val++;
    }
    else {
      return FALSE;
    }
  }
  return TRUE;
}

KMX_BOOL addXStringChar(km_kbp_cp *x, int *xl, KMX_DWORD ch) {
  if (ch > 0x10FFFF) return FALSE;
  if (ch >= 0x10000) {
    x[(*xl)++] = (KMX_WCHAR) Uni_UTF32ToSurrogate1(ch);
    x[(*xl)++] = (KMX_WCHAR) Uni_UTF32ToSurrogate2(ch);
  }
  else {
    x[(*xl)++] = (KMX_WCHAR) ch;
  }
  x[*xl] = 0;
  return TRUE;
}

KMX_BOOL addXStringDeadkey(km_kbp_cp *x, int *xl, KMX_DWORD dk) {
  x[(*xl)++] = UC_SENTINEL;
  x[(*xl)++] = CODE_DEADKEY;
  x[(*xl)++] = (KMX_WCHAR) (dk + 1);
  x[(*xl)] = 0;
  return TRUE;
}

KMX_BOOL setXString(km_kbp_cp *x, int *xl, char *val) {
  while (*val) {
    if (*val == '\\') {
      val++;
      if (*val == '\\') {
        if (!addXStringChar(x, xl, *val)) return FALSE;
      }
      else if (*val == 'u') {
        val++;
        char *p = NULL;
        if (!addXStringChar(x, xl, strtol(val, &p, 16))) return FALSE;
        val = p;
      }
      else if (*val == 'd') {
        val++;
        char *p = NULL;
        if (!addXStringDeadkey(x, xl, strtol(val, &p, 10))) return FALSE;
        val = p;
      }
    }
    else {
      if (!addXStringChar(x, xl, *val)) return FALSE;
      val++;
    }
  }
  return TRUE;
}

KMX_BOOL setExpectedOutput(char *val) {
  return setXString(g_expectedOutput, &g_expectedOutputLength, val);
}

KMX_BOOL setContext(char *val) {
  return setXString(g_context, &g_contextLength, val);
}

int main(int argc, char *argv[]) {
  char *filename = NULL;
  KMX_BOOL invalid = FALSE;

  //_setmode(_fileno(stdout), _O_U16TEXT);

  for (int i = 1; i < argc-1; i += 2) {
    char *arg = argv[i], *val = argv[i + 1];
    if (!strcmp(arg, "-kmx")) {
      filename = val;
    }
    else if (!strcmp(arg, "-expected-output")) {
      invalid = !setExpectedOutput(val);
    }
    else if (!strcmp(arg, "-context")) {
      invalid = !setContext(val);
    } 
    else if (!strcmp(arg, "-keys")) {
      invalid = !setKeys(val);
    }
    else if (!strcmp(arg, "-d")) {
      invalid = !addOption(val);
    }
    else if (!strcmp(arg, "-s")) {
      g_silent = TRUE;
      i--;
    }
    else if (!strcmp(arg, "-q")) {
      i--;
      for (int j = 1; j < argc; j++) {
        console_log(L"\"%hs\" ", argv[j]);
      }
      console_log(L"\n");
    }
    else {
      invalid = TRUE;
    }
    if (invalid) {
      console_error(L"Invalid argument %d: '%hs' = '%hs'\n", i, arg, val);
      break;
    }
  }

  if (invalid || g_nKeyEvents == 0 || filename == NULL) {

    wprintf(L"Usage: kmxtest [-s] [-q] -kmx <file.kmx> [-context <context>] -keys <key-sequence> [-expected-output <output>] [-d a=b]...\n");
    wprintf(L"  -s silent\n");
    wprintf(L"  -q print the command line\n");
    wprintf(L"  file.kmx must exist. No translation is done for mnemonic layout, etc.\n");
    wprintf(L"  context, output should be a string of unicode characters and/or deadkeys:\n");
    wprintf(L"    e.g. \"ABC\\u1234\\dxxxx\\d{name}\" (where xxxx is the integer deadkey value or {name} is the deadkey name if keyboard is compiled with debug)\n");
    wprintf(L"  key-sequence is one or more Keyman keystrokes:\n");
    wprintf(L"    e.g. \"[SHIFT K_A] [K_B]\"\n");
    wprintf(L"  -d allows for definition of environment and keyboard options\n");
    wprintf(L"    keyboard option is opt.store_name=\"value\"\n");
    wprintf(L"    environment is env.name=\"\"\n");
    wprintf(L"    default environment is:\n");

    print_default_environment();
    return 2;
  }

  KMX_Processor kmx;

  // run;

  if (!kmx.Load(filename)) {
    console_error(L"Failed to load %hs\n", filename);
    return 1;
  }

  kmx.GetContext()->Set(g_context);

  console_log(L"============ Starting test ============\n");

  for (int i = 0; i < g_nKeyEvents; i++) {
    km_kbp_cp local_context[512];
    kmx.GetContext()->Get(local_context, 512);
    console_log(L"%d: '%hs' + [%hs %hs]\n", i, Debug_UnicodeString(local_context), Debug_ModifierName(g_keyEvents[i].modifiers), Debug_VirtualKey(g_keyEvents[i].vkey));

    KMX_BOOL outputKeystroke = !kmx.ProcessEvent(g_keyEvents[i].vkey, g_keyEvents[i].modifiers, VKeyToChar(g_keyEvents[i].modifiers, g_keyEvents[i].vkey));
    console_log(L"outputKeystroke = %d\n", outputKeystroke);
  }


  int result = kmx.GetActions()->CheckOutput(g_context, g_expectedOutput) ? 0 : 1;
  if (result == 1) {
    console_error(L"Output did not match expected output\n");
  } else {
    console_log(L"Output matched expected output\n");
  }
  ValidateOptions();

  console_log(L"============ Stopping test ============\n");

  return result;
}

void ValidateOptions() {
}


const char *VKeyNames[256] = {
  // Key Codes
    "K_?00",        // &H0
    "K_LBUTTON",      // &H1
    "K_RBUTTON",      // &H2
    "K_CANCEL",       // &H3
    "K_MBUTTON",      // &H4
    "K_?05",        // &H5
    "K_?06",        // &H6
    "K_?07",        // &H7
    "K_BKSP",         // &H8
    "K_TAB",          // &H9
    "K_?0A",        // &HA
    "K_?0B",        // &HB
    "K_KP5",          // &HC
    "K_ENTER",        // &HD
    "K_?0E",        // &HE
    "K_?0F",        // &HF
    "K_SHIFT",        // &H10
    "K_CONTROL",      // &H11
    "K_ALT",        // &H12
    "K_PAUSE",        // &H13
    "K_CAPS",       // &H14
    "K_KANJI?15",     // &H15
    "K_KANJI?16",     // &H16
    "K_KANJI?17",     // &H17
    "K_KANJI?18",     // &H18
    "K_KANJI?19",     // &H19
    "K_?1A",        // &H1A
    "K_ESC",        // &H1B
    "K_KANJI?1C",     // &H1C
    "K_KANJI?1D",     // &H1D
    "K_KANJI?1E",     // &H1E
    "K_KANJI?1F",     // &H1F
    "K_SPACE",        // &H20
    "K_PGUP",       // &H21
    "K_PGDN",       // &H22
    "K_END",        // &H23
    "K_HOME",       // &H24
    "K_LEFT",       // &H25
    "K_UP",       // &H26
    "K_RIGHT",        // &H27
    "K_DOWN",       // &H28
    "K_SEL",        // &H29
    "K_PRINT",        // &H2A
    "K_EXEC",       // &H2B
    "K_PRTSCN",     // &H2C
    "K_INS",        // &H2D
    "K_DEL",        // &H2E
    "K_HELP",       // &H2F
    "K_0",          // &H30
    "K_1",          // &H31
    "K_2",          // &H32
    "K_3",          // &H33
    "K_4",          // &H34
    "K_5",          // &H35
    "K_6",          // &H36
    "K_7",          // &H37
    "K_8",          // &H38
    "K_9",          // &H39
    "K_?3A",        // &H3A
    "K_?3B",        // &H3B
    "K_?3C",        // &H3C
    "K_?3D",        // &H3D
    "K_?3E",        // &H3E
    "K_?3F",        // &H3F
    "K_?40",        // &H40

    "K_A",          // &H41
    "K_B",          // &H42
    "K_C",          // &H43
    "K_D",          // &H44
    "K_E",          // &H45
    "K_F",          // &H46
    "K_G",          // &H47
    "K_H",          // &H48
    "K_I",          // &H49
    "K_J",          // &H4A
    "K_K",          // &H4B
    "K_L",          // &H4C
    "K_M",          // &H4D
    "K_N",          // &H4E
    "K_O",          // &H4F
    "K_P",          // &H50
    "K_Q",          // &H51
    "K_R",          // &H52
    "K_S",          // &H53
    "K_T",          // &H54
    "K_U",          // &H55
    "K_V",          // &H56
    "K_W",          // &H57
    "K_X",          // &H58
    "K_Y",          // &H59
    "K_Z",          // &H5A
    "K_?5B",        // &H5B
    "K_?5C",        // &H5C
    "K_?5D",        // &H5D
    "K_?5E",        // &H5E
    "K_?5F",        // &H5F
    "K_NP0",        // &H60
    "K_NP1",        // &H61
    "K_NP2",        // &H62
    "K_NP3",        // &H63
    "K_NP4",        // &H64
    "K_NP5",        // &H65
    "K_NP6",        // &H66
    "K_NP7",        // &H67
    "K_NP8",        // &H68
    "K_NP9",        // &H69
    "K_NPSTAR",     // &H6A
    "K_NPPLUS",     // &H6B
    "K_SEPARATOR",      // &H6C
    "K_NPMINUS",      // &H6D
    "K_NPDOT",        // &H6E
    "K_NPSLASH",      // &H6F
    "K_F1",       // &H70
    "K_F2",       // &H71
    "K_F3",       // &H72
    "K_F4",       // &H73
    "K_F5",       // &H74
    "K_F6",       // &H75
    "K_F7",       // &H76
    "K_F8",       // &H77
    "K_F9",       // &H78
    "K_F10",        // &H79
    "K_F11",        // &H7A
    "K_F12",        // &H7B
    "K_F13",        // &H7C
    "K_F14",        // &H7D
    "K_F15",        // &H7E
    "K_F16",        // &H7F
    "K_F17",        // &H80
    "K_F18",        // &H81
    "K_F19",        // &H82
    "K_F20",        // &H83
    "K_F21",        // &H84
    "K_F22",        // &H85
    "K_F23",        // &H86
    "K_F24",        // &H87

    "K_?88",        // &H88
    "K_?89",        // &H89
    "K_?8A",        // &H8A
    "K_?8B",        // &H8B
    "K_?8C",        // &H8C
    "K_?8D",        // &H8D
    "K_?8E",        // &H8E
    "K_?8F",        // &H8F

    "K_NUMLOCK",      // &H90
    "K_SCROLL",     // &H91

    "K_?92",        // &H92
    "K_?93",        // &H93
    "K_?94",        // &H94
    "K_?95",        // &H95
    "K_?96",        // &H96
    "K_?97",        // &H97
    "K_?98",        // &H98
    "K_?99",        // &H99
    "K_?9A",        // &H9A
    "K_?9B",        // &H9B
    "K_?9C",        // &H9C
    "K_?9D",        // &H9D
    "K_?9E",        // &H9E
    "K_?9F",        // &H9F
    "K_?A0",        // &HA0
    "K_?A1",        // &HA1
    "K_?A2",        // &HA2
    "K_?A3",        // &HA3
    "K_?A4",        // &HA4
    "K_?A5",        // &HA5
    "K_?A6",        // &HA6
    "K_?A7",        // &HA7
    "K_?A8",        // &HA8
    "K_?A9",        // &HA9
    "K_?AA",        // &HAA
    "K_?AB",        // &HAB
    "K_?AC",        // &HAC
    "K_?AD",        // &HAD
    "K_?AE",        // &HAE
    "K_?AF",        // &HAF
    "K_?B0",        // &HB0
    "K_?B1",        // &HB1
    "K_?B2",        // &HB2
    "K_?B3",        // &HB3
    "K_?B4",        // &HB4
    "K_?B5",        // &HB5
    "K_?B6",        // &HB6
    "K_?B7",        // &HB7
    "K_?B8",        // &HB8
    "K_?B9",        // &HB9

    "K_COLON",        // &HBA
    "K_EQUAL",        // &HBB
    "K_COMMA",        // &HBC
    "K_HYPHEN",     // &HBD
    "K_PERIOD",     // &HBE
    "K_SLASH",        // &HBF
    "K_BKQUOTE",      // &HC0

    "K_?C1",        // &HC1
    "K_?C2",        // &HC2
    "K_?C3",        // &HC3
    "K_?C4",        // &HC4
    "K_?C5",        // &HC5
    "K_?C6",        // &HC6
    "K_?C7",        // &HC7
    "K_?C8",        // &HC8
    "K_?C9",        // &HC9
    "K_?CA",        // &HCA
    "K_?CB",        // &HCB
    "K_?CC",        // &HCC
    "K_?CD",        // &HCD
    "K_?CE",        // &HCE
    "K_?CF",        // &HCF
    "K_?D0",        // &HD0
    "K_?D1",        // &HD1
    "K_?D2",        // &HD2
    "K_?D3",        // &HD3
    "K_?D4",        // &HD4
    "K_?D5",        // &HD5
    "K_?D6",        // &HD6
    "K_?D7",        // &HD7
    "K_?D8",        // &HD8
    "K_?D9",        // &HD9
    "K_?DA",        // &HDA

    "K_LBRKT",        // &HDB
    "K_BKSLASH",      // &HDC
    "K_RBRKT",        // &HDD
    "K_QUOTE",        // &HDE
    "K_oDF",        // &HDF
    "K_oE0",        // &HE0
    "K_oE1",        // &HE1
    "K_oE2",        // &HE2
    "K_oE3",        // &HE3
    "K_oE4",        // &HE4

    "K_?E5",        // &HE5

    "K_oE6",        // &HE6

    "K_?E7",        // &HE7
    "K_?E8",        // &HE8

    "K_oE9",        // &HE9
    "K_oEA",        // &HEA
    "K_oEB",        // &HEB
    "K_oEC",        // &HEC
    "K_oED",        // &HED
    "K_oEE",        // &HEE
    "K_oEF",        // &HEF
    "K_oF0",        // &HF0
    "K_oF1",        // &HF1
    "K_oF2",        // &HF2
    "K_oF3",        // &HF3
    "K_oF4",        // &HF4
    "K_oF5",        // &HF5

    "K_?F6",        // &HF6
    "K_?F7",        // &HF7
    "K_?F8",        // &HF8
    "K_?F9",        // &HF9
    "K_?FA",        // &HFA
    "K_?FB",        // &HFB
    "K_?FC",        // &HFC
    "K_?FD",        // &HFD
    "K_?FE",        // &HFE
    "K_?FF"       // &HFF
};

const struct KMXTest_ModifierNames s_modifierNames[14] = {
  {"LCTRL", 0x0001},    // Left Control flag
  {"RCTRL", 0x0002},    // Right Control flag
  {"LALT", 0x0004},   // Left Alt flag
  {"RALT", 0x0008},   // Right Alt flag
  {"SHIFT", 0x0010},    // Either shift flag
  {"CTRL-do-not-use", 0x0020},    // Either ctrl flag -- don't use this for inputs
  {"ALT-do-not-use", 0x0040},   // Either alt flag -- don't use this for inputs
  {"CAPS", 0x0100},   // Caps lock on
  {"NCAPS", 0x0200},    // Caps lock NOT on
  {"NUMLOCK", 0x0400},    // Num lock on
  {"NNUMLOCK", 0x0800},   // Num lock NOT on
  {"SCROLL", 0x1000},   // Scroll lock on
  {"NSCROLL", 0x2000},    // Scroll lock NOT on
  {NULL, 0}
};

#define VK_COLON  0xBA
#define VK_EQUAL  0xBB
#define VK_COMMA  0xBC
#define VK_HYPHEN 0xBD
#define VK_PERIOD 0xBE
#define VK_SLASH  0xBF
#define VK_ACCENT 0xC0
#define VK_LBRKT  0xDB
#define VK_BKSLASH  0xDC
#define VK_RBRKT  0xDD
#define VK_QUOTE  0xDE
#define VK_xDF    0xDF

const struct KMXTest_ChToVKey chToVKey[] = {
  {VK_SPACE},     // 
  {'1', 1},       // !
  {VK_QUOTE, 1},  // "
  {'3', 1},       // #
  {'4', 1},       // $
  {'5', 1},       // %
  {'7', 1},       // &
  {VK_QUOTE},     // '
  {'9', 1},       // (
  {'0', 1},       // )
  {'8', 1},       // *
  {VK_EQUAL, 1},  // +
  {VK_COMMA},     // ,
  {VK_HYPHEN},    // -
  {VK_PERIOD},    // .
  {VK_SLASH},     // /
  {'0'},
  {'1'},
  {'2'},
  {'3'},
  {'4'},
  {'5'},
  {'6'},
  {'7'},
  {'8'},
  {'9'},
  {VK_COLON, 1},  // :
  {VK_COLON},     // ;
  {VK_COMMA, 1},  // <
  {VK_EQUAL},     // =
  {VK_PERIOD, 1}, // >
  {VK_SLASH, 1},  // ?
  {'2', 1},       // @
  {'A', 1},
  {'B', 1},
  {'C', 1},
  {'D', 1},
  {'E', 1},
  {'F', 1},
  {'G', 1},
  {'H', 1},
  {'I', 1},
  {'J', 1},
  {'K', 1},
  {'L', 1},
  {'M', 1},
  {'N', 1},
  {'O', 1},
  {'P', 1},
  {'Q', 1},
  {'R', 1},
  {'S', 1},
  {'T', 1},
  {'U', 1},
  {'V', 1},
  {'W', 1},
  {'X', 1},
  {'Y', 1},
  {'Z', 1},
  {VK_LBRKT},
  {VK_BKSLASH},
  {VK_RBRKT},
  {'6', 1},
  {VK_HYPHEN, 1},
  {VK_ACCENT},
  {'A'},
  {'B'},
  {'C'},
  {'D'},
  {'E'},
  {'F'},
  {'G'},
  {'H'},
  {'I'},
  {'J'},
  {'K'},
  {'L'},
  {'M'},
  {'N'},
  {'O'},
  {'P'},
  {'Q'},
  {'R'},
  {'S'},
  {'T'},
  {'U'},
  {'V'},
  {'W'},
  {'X'},
  {'Y'},
  {'Z'},
  {VK_LBRKT, 1},
  {VK_BKSLASH, 1},
  {VK_RBRKT, 1},
  {VK_ACCENT, 1},
  {0, 0}
};
