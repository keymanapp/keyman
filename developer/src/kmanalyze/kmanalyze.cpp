// kmanalyze.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include "../../../common/windows/cpp/include/keymanversion.h"
#include <iostream>
#include <vector>
#include <codecvt>
#include <locale>

BOOL LoadKeyboard(LPSTR fileName, LPKEYBOARD *lpKeyboard);

void Err(const char *p);
int DoKeyboardAnalysis(LPKEYBOARD kbd, char *keyboardID, char *keyboardJSFilename, char *outputfilename);
void MapVirtualKeys(void);

struct TEST {
  UINT key;
  UINT shift;
  std::wstring context;
};

typedef std::vector<TEST> TESTS;

enum GROUPREFTYPE { rule, match, nomatch };

struct GROUPTREE;

struct GROUPREF {
  GROUPREFTYPE type;
  GROUPTREE* target;
  LPKEY rule;
};

struct GROUPTREE {
  LPGROUP group;
  std::vector<GROUPREF> refs;
};

TESTS *DoGroupAnalysis(LPKEYBOARD kbd, LPGROUP gp, std::vector<LPGROUP> & tree, TEST *test);
int run(int argc, char *argv[]);

wchar_t VKToChar[256][2];
UINT CharToVK[256];
UINT CharToShift[256];

#define KEYMAN_SENTRY_LOGGER_DEVELOPER_TOOLS_KMANALYZE KEYMAN_SENTRY_LOGGER_DEVELOPER_TOOLS ".kmanalyze"

int main(int argc, char *argv[])
{
  return keyman_sentry_main(TRUE, KEYMAN_SENTRY_LOGGER_DEVELOPER_TOOLS_KMANALYZE, argc, argv, run);
}

int run(int argc, char *argv[])
{
  LPKEYBOARD kbd;
  char buf[_MAX_PATH], drive[_MAX_DRIVE], dir[_MAX_DIR], filename[_MAX_FNAME], ext[_MAX_EXT], jsfilename[_MAX_PATH];

  if (argc < 2 || !strcmp(argv[1], "--help"))
  {
    printf(
      "KMANALYZE: Extract rules from a Keyman .kmx keyboard to use for building automated tests\n"
      "Version %s, %s\n"
      "Usage: KMANALYZE <filename> [outputfilename]\n\n"
      "Will create a keyboard.tests from keyboard.js; if outputfilename is not specified,\n"
      "then will put the output file in the same folder as filename.",
      KEYMAN_VersionWithTag, KEYMAN_Copyright
    );
    return 1;
  }

  MapVirtualKeys();

  if (!LoadKeyboard(argv[1], &kbd)) return 2;

  _splitpath_s(argv[1], drive, dir, filename, ext);
  if (argc >= 3) {
    strcpy_s(buf, argv[2]);
  }
  else {
    _makepath_s(buf, drive, dir, filename, ".tests");
  }
  _makepath_s(jsfilename, drive, dir, filename, ".js");

  int n = DoKeyboardAnalysis(kbd, filename, jsfilename, buf);

  delete kbd;
  return n;
}


void Err(const char *p)
{
  printf("Fatal Error: %s\n", p);
}

BOOL LoadKeyboard(LPSTR fileName, LPKEYBOARD *lpKeyboard)
{
  DWORD sz, i, j;
  LPBYTE buf;
  HANDLE hFile;
  PCOMP_KEYBOARD ckbp;
  PCOMP_GROUP cgp;
  PCOMP_STORE csp;
  PCOMP_KEY ckp;
  LPKEYBOARD kbp;
  LPGROUP gp;
  LPSTORE sp;
  LPKEY kp;

  if (!lpKeyboard)
  {
    Err("Internal error 001");
    return FALSE;
  }

  if (!fileName)
  {
    Err("Bad Filename");
    return FALSE;
  }

  hFile = CreateFile(fileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
  if (hFile == INVALID_HANDLE_VALUE)
  {
    Err("Could not open file");
    return FALSE;
  }

  sz = GetFileSize(hFile, NULL);

  buf = new BYTE[sz];

  if (!buf)
  {
    CloseHandle(hFile);
    Err("Could not allocate memory");
    return FALSE;
  }

  ReadFile(hFile, buf, sz, &sz, NULL);

  CloseHandle(hFile);

  kbp = (LPKEYBOARD)buf;
  ckbp = (PCOMP_KEYBOARD)buf;

  if (kbp->dwIdentifier != FILEID_COMPILED) { delete buf; Err("errNotFileID"); return FALSE; }

  /* Check file version */

  if (ckbp->dwFileVersion < VERSION_MIN ||
    ckbp->dwFileVersion > VERSION_MAX)
  {
    kbp->dpStoreArray = (LPSTORE)(buf + ckbp->dpStoreArray);
    for (sp = kbp->dpStoreArray, i = 0; i < kbp->cxStoreArray; i++, sp++) {
      if (sp->dwSystemID == TSS_COMPILEDVERSION)
      {
        char buf2[256];
        wsprintf(buf2, "Wrong File Version: file version is %ls", ((PBYTE)kbp) + (INT_PTR)sp->dpString);
        delete buf;
        Err(buf2);
        return FALSE;
      }
    }
    delete buf;
    Err("Unknown File Version: try using the latest version of KMDECOMP");
    return FALSE;
  }

  kbp->dpStoreArray = (LPSTORE)(buf + ckbp->dpStoreArray);
  kbp->dpGroupArray = (LPGROUP)(buf + ckbp->dpGroupArray);

  //kbp->dpName = (PWSTR) (buf + ckbp->dpName);
  //kbp->dpCopyright = (PWSTR) (buf + ckbp->dpCopyright);
  //kbp->dpMessage = (PWSTR) (buf + ckbp->dpMessage);
  //kbp->dpLanguageName = (PWSTR) (buf + ckbp->dpLanguageName);

  for (sp = kbp->dpStoreArray, csp = (PCOMP_STORE)sp, i = 0; i < kbp->cxStoreArray; i++, sp++, csp++)
  {
    if (csp->dpName > 0) sp->dpName = (PWSTR)(buf + csp->dpName); else sp->dpName = NULL;
    sp->dpString = (PWSTR)(buf + csp->dpString);
  }

  for (gp = kbp->dpGroupArray, cgp = (PCOMP_GROUP)gp, i = 0; i < kbp->cxGroupArray; i++, gp++, cgp++)
  {
    if (cgp->dpName > 0) gp->dpName = (PWSTR)(buf + cgp->dpName); else gp->dpName = NULL;
    gp->dpKeyArray = (LPKEY)(buf + cgp->dpKeyArray);
    if (cgp->dpMatch != NULL) gp->dpMatch = (PWSTR)(buf + cgp->dpMatch);
    if (cgp->dpNoMatch != NULL) gp->dpNoMatch = (PWSTR)(buf + cgp->dpNoMatch);

    for (kp = gp->dpKeyArray, ckp = (PCOMP_KEY)kp, j = 0; j < gp->cxKeyArray; j++, kp++, ckp++)
    {
      kp->dpOutput = (PWSTR)(buf + ckp->dpOutput);
      kp->dpContext = (PWSTR)(buf + ckp->dpContext);
    }
  }

  *lpKeyboard = kbp;

  return TRUE;
}


//void DoContextAnalysis(LPKEYBOARD kbd, GROUPTREE *gpref, std::vector<LPGROUP> tree, LPKEY kp, PWCHAR pc, GROUPREFTYPE type) {
//}

TESTS *DoGroupAnalysis(LPKEYBOARD kbd, LPGROUP gp, std::vector<LPGROUP> & tree, TEST *base_test) {

  //
  // Prevent recursion
  //

  std::vector<LPGROUP>::iterator it = std::find(tree.begin(), tree.end(), gp);
  if(it != tree.end()) {
    puts("Recursive groups are not yet supported");
    return NULL;
  }

  tree.push_back(gp);
  //printf("%*.*sEntering group %d [depth %d]\n", tree.size(), tree.size(), " ", gpref->group - kbd->dpGroupArray + 1, tree.size());

  LPKEY kp = gp->dpKeyArray;

  // Set of tests for this group.
  TESTS *tests = new TESTS;

  for (DWORD i = 0; i < gp->cxKeyArray; i++, kp++) {
    // Set of expanding tests for this rule
    TESTS *new_tests = NULL;
    TEST t0;

    if (gp->fUsingKeys && base_test && base_test->key != 0 && (base_test->key != kp->Key || base_test->shift != kp->ShiftFlags)) {
      // We are in a subgroup that uses keys, and this rule's key doesn't match the parent group's test's key.
      continue;
    }

    std::wstring context;
    for (PWCHAR pc = kp->dpContext; pc && *pc; pc = incxstr(pc)) {
      if (*pc == UC_SENTINEL) {
        switch (*(pc + 1)) {
        case CODE_ANY:
        {
          PWCHAR ps = kbd->dpStoreArray[*(pc + 2) - 1].dpString;
          // TODO: Support any character in store
          if (ps[0] == UC_SENTINEL) break;
          context += ps[0];
          if (Uni_IsSurrogate1(ps[0])) context += ps[1];
        }
          break;
        case CODE_NUL:
          // TODO: SUpport nul at start of context
          break;
        case CODE_DEADKEY:
          // TODO: Support deadkeys for testing
          break;
        case CODE_CONTEXTEX:
          break;
        case CODE_NOTANY:
          break;
        }
      }
      else {
        context += *pc;
        if (Uni_IsSurrogate1(*pc)) context += *(pc + 1);
      }
    }

    if (base_test) {
      std::wstring c1 = context.length() > base_test->context.length() ? context : base_test->context;
      //std::wstring c2 = context.length() > base_test->context.length() ? base_test->context : context;

      //if(c1.compare(c1.length() - c2.length(), c2.length(), c2) != 0) {
        // TODO: We are not comparing like with like ... so continue; but this naive test does not take into account
        // stores, which we need to, in order to make this work well.
      //}

      // TODO: For now we are assuming that the contexts are congruent, so we take the longer one for our test.
      // TODO: In reality, we could probably do both the longer and shorter one as separate tests...
      //TODO: merge_context(context);
      context = c1;
    }

    t0.context = context;
    if (!gp->fUsingKeys) {
      if (base_test) {
        t0.key = base_test->key;
        t0.shift = base_test->shift;
      }
    }
    else {
      t0.key = kp->Key;
      t0.shift = kp->ShiftFlags;
    }

    BOOL found = FALSE;
    for (PWCHAR pc = kp->dpOutput; pc && *pc; pc = incxstr(pc)) {
      if (*pc == UC_SENTINEL && *(pc + 1) == CODE_USE) {
        //printf("%*.*s: Matched rule %d of group %d\n", tree.size(), tree.size(), " ", kp ? kp - gpref->group->dpKeyArray + 1 : -1, gpref->group - kbd->dpGroupArray + 1);
        LPGROUP gpTarget = &kbd->dpGroupArray[*(pc + 2) - 1];
        new_tests = DoGroupAnalysis(kbd, gpTarget, tree, &t0);
        if (!new_tests) return NULL;
        // TODO: Then multiplying new_tests for each subsequent group I guess!?
        found = TRUE;
        break;
      }
    }
    if (!found) {
      // TODO: Support use() in rule and use() in match
      // check for use in match or extra
      PWCHAR pc = gp->dpMatch;
      if (pc && *pc == UC_SENTINEL && *(pc + 1) == CODE_USE) {
        new_tests = DoGroupAnalysis(kbd, &kbd->dpGroupArray[*(pc + 2) - 1], tree, &t0);
        if (!new_tests) return NULL;
        // TODO: JUST A SINGLE GROUP FOR NOW
      }
    }

    tests->push_back(t0);

    if (new_tests) {
      for (size_t i = 0; i < new_tests->size(); i++)
        tests->push_back(new_tests->at(i));

      delete new_tests;
    }
    //t0.context = "";
    //PWC
    //t0.context = kp->dpContext;
    //for (int i = 0; i < new_tests.size(); i++) {
    //DoContextAnalysis(kbd, gp, tree, kp, kp->dpOutput, rule, gpref->group->dpMatch);
  }

  PWCHAR pc = gp->dpNoMatch;
  if (pc && *pc == UC_SENTINEL && *(pc + 1) == CODE_USE) {
    TESTS *new_tests = DoGroupAnalysis(kbd, &kbd->dpGroupArray[*(pc+2)-1], tree, base_test);

    if (!new_tests) return NULL;

    for (size_t i = 0; i < new_tests->size(); i++)
      tests->push_back(new_tests->at(i));

    delete new_tests;

    // TODO: JUST A SINGLE GROUP FOR NOW
  }

  //printf("%*.*sExiting group %d [depth %d]\n", tree.size(), tree.size(), " ", gpref->group - kbd->dpGroupArray + 1, tree.size());
  tree.pop_back();

  return tests;
}

void PrintTree(LPKEYBOARD kbd, GROUPTREE *t, int depth);

std::string unicode_escape(std::wstring s) {
  std::string r = "\"";
  for (auto ch = s.begin(); ch != s.end(); ch++) {
    char buf[16];
    sprintf_s(buf, "\\u%04.4x", *ch);
    r += buf;
  }
  r += "\"";
  return r;
}

#include "../../../common/windows/cpp/include/vkeys.h"

std::wstring GetKeyName(UINT key) {
  WCHAR buf[64];

//  if (key < 256) {
//    wsprintfW(buf, L"keyCodes.%s /* 0x%x */", VKeyNames[key], key);
//  }
//  else {
    wsprintfW(buf, L"0x%x", key);
//  }

  std::wstring str(buf);
  return str;
}

struct ModifierNames {
  PCWSTR name;
  UINT modifier;
};

// see kmwosk.ts
ModifierNames modifierNames[] = {
  { L"LCTRL", 0x0001 },
  { L"RCTRL", 0x0002 },
  { L"LALT", 0x0004 },
  { L"RALT", 0x0008 },
  { L"SHIFT", 0x0010 },
  { L"CTRL", 0x0020 },
  { L"ALT", 0x0040 },
  { L"CAPS", 0x0100 },
  { L"NO_CAPS", 0x0200 },
  { L"NUM_LOCK", 0x0400 },
  { L"NO_NUM_LOCK", 0x0800 },
  { L"SCROLL_LOCK", 0x1000 },
  { L"NO_SCROLL_LOCK", 0x2000 },
  { L"VIRTUAL_KEY", 0x4000 },
  { NULL, 0 }
};

std::wstring GetModifierName(UINT modifier) {
  modifier &= ~ISVIRTUALKEY; // we don't need to dump this in every rule
  std::wstring str;
  WCHAR buf[64];
  UINT originalModifier = modifier;

  for (int i = 0; modifierNames[i].name; i++) {
    if (modifier & modifierNames[i].modifier) {
      wsprintfW(buf, L"modCodes.%s", modifierNames[i].name);
      if (str.size()) str += L" | ";
      str += buf;
      modifier &= ~modifierNames[i].modifier;
    }
  }

  if (modifier) {
    wsprintfW(buf, L"%x", modifier);
    if (str.size()) str += L" | ";
    str += buf;
  }

  wsprintfW(buf, L" /* 0x%x */", originalModifier);
  if (!str.size()) str += L"0";
  str += buf;
  return str;
}

void PrintTests(TESTS *tests, char *keyboardID, wchar_t *keyboardName, char *keyboardJSFilename, char *outputfilename) {
  FILE *fp;
  fopen_s(&fp, outputfilename, "wt");

  std::wstring_convert<std::codecvt_utf8<wchar_t>> utf8_conv;
  //std::wstring keyboardName_t = keyboardName;

  fprintf(fp,
    /*"(function(){\n"
    "\"use strict\";\n"
    "var modCodes = testRunner.modCodes;\n"
    "var keyCodes = testRunner.keyCodes;\n"
    "testRunner.register(*/"{\n"
    "  \"keyboard\": {\n"
    "    \"id\": \"%s\",\n"
    //"    \"name\" : \"%s\",\n" //TODO: utf8
    //"    \"filename\" : \"%s\",\n"
    "    \"languages\" : [{\n"
    "      \"id\": \"en\",\n"
    "      \"name\" : \"English\",\n"
    "      \"region\" : \"Europe\"\n"
    "    }]\n"
    "  },\n"
    "  \"inputTests\": {\n",
    keyboardID
    //utf8_conv.to_bytes(keyboardName).c_str(),
    //keyboardJSFilename
  );

  for (size_t i = 0; i < tests->size(); i++) {
    UINT key = tests->at(i).key, shift = tests->at(i).shift;

    if (!(shift & ISVIRTUALKEY)) {
      shift = CharToShift[key] | ISVIRTUALKEY;
      key = CharToVK[key];
    }

    std::wstring keyName = GetKeyName(key);
    std::wstring modifierName = GetModifierName(shift);
    fprintf(fp, "    \"%d\": {\"key\": %d", i, key); // keyName.c_str());

    if (shift != ISVIRTUALKEY && shift != 0)
      fprintf(fp, ", \"modifier\": %d", shift & ~ISVIRTUALKEY); // %ws", modifierName.c_str());

    if (tests->at(i).context.size())
      fprintf(fp, ", \"context\": %s", unicode_escape(tests->at(i).context).c_str());
    fprintf(fp, "}%s\n", (i == tests->size() - 1 ? "" : ","));
  }

  fprintf(fp,
    "  }\n"
    "}"/*);\n"
    "})();\n"*/
  );

  fclose(fp);
}

void RemoveDuplicateTests(TESTS *tests) {
  // TODO: sort tests by key, modifier, context

  // Then remove duplicates
}

int DoKeyboardAnalysis(LPKEYBOARD kbd, char *keyboardID, char *keyboardJSFilename, char *outputfilename) {
  std::vector<std::vector<GROUPREF>> groupPaths;

  //GROUPTREE tree;
  LPGROUP gp = &kbd->dpGroupArray[kbd->StartGroup[1]]; // Unicode start group

  std::vector<LPGROUP> tt;

  // each rule in the base group is a base rule. Then, each
  // rule can be expanded out by the use() tree from the rule.
  TESTS *tests = DoGroupAnalysis(kbd, gp, tt, NULL);
  if (!tests) {
    // Error exit
    return 3;
  }

  wchar_t *keyboardName = NULL, keyboardNameBuf[32];

  for (UINT i = 0; i < kbd->cxStoreArray; i++) {
    if (kbd->dpStoreArray[i].dwSystemID == TSS_NAME) {
      keyboardName = kbd->dpStoreArray[i].dpString;
    }
  }

  if (!keyboardName) {
    wsprintfW(keyboardNameBuf, L"%s", keyboardID);
    keyboardName = keyboardNameBuf;
  }

  RemoveDuplicateTests(tests);

  PrintTests(tests, keyboardID, keyboardName, keyboardJSFilename, outputfilename);

  delete tests;
  // print the tree

  //PrintTree(kbd, &tree, 0);

  return 0;
}

DWORD NextUTF32(PWCHAR pc) {
  if (Uni_IsSurrogate1(*pc)) {
    return Uni_SurrogateToUTF32(*pc, *(pc + 1));
  }
  return *pc;
}
/*
void PrintRule(LPKEYBOARD kbd, LPKEY kp) {
  LPSTORE sp;
  std::vector<DWORD> context, output;

  // Always take first char in each referenced store
  for (PWCHAR pc = kp->dpContext; pc && *pc; pc = incxstr(pc)) {
    if (*pc == UC_SENTINEL) {
      switch (*(pc + 1)) {
      case CODE_ANY:
        sp = &kbd->dpStoreArray[*(pc + 2) - 1];
        context.push_back(NextUTF32(sp->dpString));
        break;
      case CODE_NOTANY:
        assert(FALSE); //TODO
      case CODE_INDEX:
        assert(FALSE); //TODO
      case CODE_DEADKEY:

        case CODE_EXTENDED:		p += 2; while (*p != UC_SENTINEL_EXTENDEDEND) p++; return p + 1;
        case CODE_CLEARCONTEXT: return p + 1;
        case CODE_CALL:			return p + 1;
        case CODE_CONTEXTEX:	return p + 1;
        case CODE_IFOPT:    return p + 3;
        case CODE_IFSYSTEMSTORE: return p + 3;
        case CODE_SETOPT:   return p + 2;
        case CODE_SETSYSTEMSTORE: return p + 2;
        case CODE_RESETOPT: return p + 1;
        case CODE_SAVEOPT:  return p + 1;
      }
    }
    else {
      // Character (either 1 or 2 word)
    }
  }
}
*/

int groupindex(LPKEYBOARD kbd, LPGROUP gp) {
  return gp - kbd->dpGroupArray;
}

void PrintTree(LPKEYBOARD kbd, GROUPTREE *t, int depth) {
  if (t->group->dpName)
    printf("%*.*s%ws\n", depth * 2, depth * 2, " ", t->group->dpName);
  else
    printf("%*.*sgroup%d\n", depth * 2, depth * 2, " ", groupindex(kbd, t->group));

//  LPKEY kp = t->group->dpKeyArray;
//  for (DWORD i = 0; i < t->group->cxKeyArray; i++) {
//    AnalyzeRule(kp);
//  }

  for (size_t i = 0; i < t->refs.size(); i++) {
    PrintTree(kbd, t->refs[i].target, (depth + 1));
  }
}


// MapVirtualKeys is copied from syskbd.cpp (keyman32)
#define VK_COLON	0xBA
#define VK_EQUAL	0xBB
#define VK_COMMA	0xBC
#define VK_HYPHEN	0xBD
#define VK_PERIOD	0xBE
#define	VK_SLASH	0xBF
#define VK_ACCENT	0xC0
#define VK_LBRKT	0xDB
#define VK_BKSLASH	0xDC
#define VK_RBRKT	0xDD
#define VK_QUOTE	0xDE
#define VK_xDF		0xDF

WCHAR MapVirtualKeys(WORD keyCode, UINT shiftFlags)
{
  char shiftedDigit[] = ")!@#$%^&*(";
  int n, Shift;

  if (shiftFlags & (LCTRLFLAG | RCTRLFLAG | LALTFLAG | RALTFLAG)) return 0;

  if (keyCode >= '0' && keyCode <= '9')
  {
    n = keyCode - '0';
    return ((shiftFlags & K_SHIFTFLAG) ? shiftedDigit[n] : keyCode);
  }

  if (keyCode >= 'A' && keyCode <= 'Z')
  {
    Shift = (shiftFlags & K_SHIFTFLAG);
    if (shiftFlags & (CAPITALFLAG)) Shift = !Shift;
    return (Shift ? keyCode : keyCode + 32);
  }

  if (keyCode >= VK_NUMPAD0 && keyCode <= VK_NUMPAD9)
  {
    if (!(shiftFlags & NUMLOCKFLAG)) return 0;
    return keyCode - (VK_NUMPAD0 - '0');
  }

  Shift = (shiftFlags & K_SHIFTFLAG);

  switch (keyCode)
  {
  case VK_ACCENT:
    return Shift ? '~' : '`';
  case VK_HYPHEN:
    return Shift ? '_' : '-';
  case VK_EQUAL:
    return Shift ? '+' : '=';
  case VK_BKSLASH:
  case 0xE2:  // I5332
    return Shift ? '|' : 92;
  case VK_LBRKT:
    return Shift ? '{' : '[';
  case VK_RBRKT:
    return Shift ? '}' : ']';
  case VK_COLON:
    return Shift ? ':' : ';';
  case VK_QUOTE:
    return Shift ? '"' : 39;
  case VK_COMMA:
    return Shift ? '<' : ',';
  case VK_PERIOD:
    return Shift ? '>' : '.';
  case VK_SLASH:
    return Shift ? '?' : '/';
  case VK_SPACE:
    return ' ';
  }
  return 0;
  //keyCode;
}

void MapVirtualKeys(void) {
  for (int i = 0; i < 256; i++) {
    VKToChar[i][0] = MapVirtualKeys(i, 0);
    VKToChar[i][1] = MapVirtualKeys(i, K_SHIFTFLAG);
    CharToVK[VKToChar[i][0]] = i;
    CharToVK[VKToChar[i][1]] = i;
    CharToShift[VKToChar[i][0]] = 0;
    CharToShift[VKToChar[i][1]] = K_SHIFTFLAG;
  }
}
