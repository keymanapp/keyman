/*
  Name:             Compiler
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    23 Aug 2006 - mcdurdin - Add VISUALKEYBOARD, KMW_RTL, KMW_HELPFILE, KMW_HELPTEXT, KMW_EMBEDJS system stores
                    14 Sep 2006 - mcdurdin - Support icons in version 7
                    28 Sep 2006 - mcdurdin - Added product validation
                    28 Sep 2006 - mcdurdin - Added test for version 7.0 icon support
                    06 Oct 2006 - mcdurdin - Fix buffer overflow in UTF8 conversion
                    04 Dec 2006 - mcdurdin - Fix readfile buffer bug
                    04 Jan 2007 - mcdurdin - Add notany support
                    22 Jan 2007 - mcdurdin - Add K_NPENTER reference
                    25 Jan 2007 - mcdurdin - Resize buffers to 4095
                    30 May 2007 - mcdurdin - I786 - Compiler crash if zero length string in keystroke any
                    23 Aug 2007 - mcdurdin - I1011 - Fix buffer clobbering for UTF8 conversion of large files
                    27 Mar 2008 - mcdurdin - I1358 - Support for multiple languages for office config
                    14 Jun 2008 - mcdurdin - Support documenting language id as a single WORD instead of by PRIMARY/SUB
                    14 Jun 2008 - mcdurdin - Support Windows languages list
                    28 Jul 2008 - mcdurdin - I1569 - line prefixes
                    22 Mar 2010 - mcdurdin - Compiler fixup - x64 support
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    24 Jun 2010 - mcdurdin - I2432 - Use local buffers so GetXString can be re-entrant (used by if())
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    18 Mar 2011 - mcdurdin - I2646 - Compiler warning on invalid Ethnologue codes
                    18 Mar 2011 - mcdurdin - I2525 - Unterminated string can crash compiler
                    19 Jul 2011 - mcdurdin - I2993 - Named code constants cause a warning 0x208D to appear
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    17 Aug 2012 - mcdurdin - I3431 - V9.0 - Spaces in values in if comparisons break the compiler parser
                    27 Aug 2012 - mcdurdin - I3439 - V9.0 - Refactor xstring support in C++ code
                    27 Aug 2012 - mcdurdin - I3437 - V9.0 - Add support for set(&layer) and layer()
                    27 Aug 2012 - mcdurdin - I3438 - V9.0 - Add support for custom virtual keys
                    27 Aug 2012 - mcdurdin - I3430 - V9.0 - Add support for if(&platform) and if(&baselayout) to compilers
                    27 Aug 2012 - mcdurdin - I3440 - V9.0 - Tidy up set statement delimiter recognition
                    24 Oct 2012 - mcdurdin - I3483 - V9.0 - Add support for compiling in the layout file
                    24 Oct 2012 - mcdurdin - I3481 - V9.0 - Eliminate unsafe calls in C++
                    24 Jan 2012 - mcdurdin - I3137 - If key part of VK rule is missing, compiler generates invalid file
                    06 Feb 2012 - mcdurdin - I3228 - kmcmpdll sometimes tries to write temp files to Program Files
                    03 Nov 2012 - mcdurdin - I3510 - V9.0 - Merge of I3228 - kmcmpdll sometimes tries to write temp files to Program Files
                    03 Nov 2012 - mcdurdin - I3511 - V9.0 - Merge of I3137 - If key part of VK rule is missing, compiler generates invalid file
                    13 Dec 2012 - mcdurdin - I3654 - V9.0 - Compiler appears to create unregistered keyboards even when registered
                    13 Dec 2012 - mcdurdin - I3641 - V9.0 - compiler dll buffer overrun bugs
                    13 Dec 2012 - mcdurdin - I3681 - V9.0 - KeymanWeb compiler should output formatted js when debug=1
                    13 Dec 2012 - mcdurdin - I3686 - V9.0 - AddStore attaches property flags to wrong store structure
                    19 Mar 2014 - mcdurdin - I4140 - V9.0 - Add keyboard version information to keyboards
                    04 Nov 2014 - mcdurdin - I4504 - V9.0 - Consolidate the compile action into single command
                    02 Jul 2015 - mcdurdin - I4784 - Compiler does not recognise baselayout, layer or platform at the start of a line
                    02 Jul 2015 - mcdurdin - I4785 - baselayout(), layer() and platform() produce incorrect compiled code
                    24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile
                    24 Aug 2015 - mcdurdin - I4867 - Add test for code after use warning to compile
                    06 Nov 2015 - mcdurdin - I4914 - kmcmpdll does not pick an index() statement that has an offset one past the key
                    23 Feb 2016 - mcdurdin - I4982 - Defined character constants cannot be referenced correctly in other stores
                    25 Oct 2016 - mcdurdin - I5135 - Remove product and licensing references from Developer projects
*/
#include "pch.h"

#include <compfile.h>
#include <kmn_compiler_errors.h>
#include "../../../common/windows/cpp/include/vkeys.h"
#include <versioning.h>
#include <kmcmpdll.h>
#include <DeprecationChecks.h>

#include "virtualcharkeys.h"

#include "../../../common/windows/cpp/include/crc32.h"
#include "../../../common/windows/cpp/include/ConvertUTF.h"
#include "debugstore.h"
#include "namedcodeconstants.h"
#include "../../../common/windows/cpp/include/unicode.h"

#include "edition.h"

#include "CharToKeyConversion.h"
#include "CasedKeys.h"
#include "CheckNCapsConsistency.h"
#include "CheckFilenameConsistency.h"
#include "UnreachableRules.h"
#include "CheckForDuplicates.h"

int xatoi(PWSTR *p);
int atoiW(PWSTR p);
void safe_wcsncpy(PWSTR out, PWSTR in, int cbMax);
int UTF32ToUTF16(int n, int *n1, int *n2);
int GetDeadKey(PFILE_KEYBOARD fk, PWSTR p);

BOOL IsValidCallStore(PFILE_STORE fs);
BOOL IsSameToken(PWSTR *p, PWSTR token);
DWORD GetRHS(PFILE_KEYBOARD fk, PWSTR p, PWSTR buf, int bufsize, int offset, int IsUnicode);
PWSTR GetDelimitedString(PWSTR *p, PWSTR Delimiters, WORD Flags);
DWORD GetXString(PFILE_KEYBOARD fk, PWSTR str, PWSTR token, PWSTR output, int max, int offset, PWSTR *newp, int isVKey,
  int isUnicode);

int GetGroupNum(PFILE_KEYBOARD fk, PWSTR p);
int LineTokenType(PWSTR *str);

DWORD ParseLine(PFILE_KEYBOARD fk, PWSTR str);

DWORD ProcessGroupFinish(PFILE_KEYBOARD fk);
DWORD ProcessGroupLine(PFILE_KEYBOARD fk, PWSTR p);
DWORD ProcessStoreLine(PFILE_KEYBOARD fk, PWSTR p);
DWORD AddDebugStore(PFILE_KEYBOARD fk, PWSTR str);
DWORD ProcessKeyLine(PFILE_KEYBOARD fk, PWSTR str, BOOL IsUnicode);
DWORD ProcessEthnologueStore(PWSTR p); // I2646
DWORD ProcessHotKey(PWSTR p, DWORD *hk);
DWORD ImportBitmapFile(PFILE_KEYBOARD fk, PWSTR szName, PDWORD FileSize, PBYTE *Buf);

DWORD ExpandKp(PFILE_KEYBOARD fk, PFILE_KEY kpp, DWORD storeIndex);

DWORD ReadLine(HANDLE hInfile, PWSTR str, BOOL PreProcess);

DWORD WriteCompiledKeyboard(PFILE_KEYBOARD fk, HANDLE hOutfile);
BOOL CompileKeyboardHandle(HANDLE hInfile, PFILE_KEYBOARD fk);

int GetVKCode(PFILE_KEYBOARD fk, PWSTR p);  // I3438  // TODO: Consolidate GetDeadKey and GetVKCode?
DWORD BuildVKDictionary(PFILE_KEYBOARD fk);  // I3438
DWORD AddStore(PFILE_KEYBOARD fk, DWORD SystemID, PWSTR str, DWORD *dwStoreID = NULL);
DWORD ProcessSystemStore(PFILE_KEYBOARD fk, DWORD SystemID, PFILE_STORE sp);
void RecordDeadkeyNames(PFILE_KEYBOARD fk);
DWORD AddCompilerVersionStore(PFILE_KEYBOARD fk);
BOOL CheckStoreUsage(PFILE_KEYBOARD fk, int storeIndex, BOOL fIsStore, BOOL fIsOption, BOOL fIsCall);

DWORD process_if(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx);
DWORD process_reset(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx);
DWORD process_set(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx);
DWORD process_save(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx);
DWORD process_platform(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx);  // I3430
DWORD process_baselayout(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx);  // I3430
DWORD process_set_synonym(DWORD dwSystemID, PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx);  // I3437
DWORD process_expansion(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx, int max);

BOOL IsValidKeyboardVersion(WCHAR *dpString);   // I4140

HANDLE UTF16TempFromUTF8(HANDLE hInfile, BOOL hasPreamble);

const PWCHAR LineTokens[] = {
  L"SVNBHBGMNSCCLLCMLB", L"store", L"VERSION ", L"NAME ",
  L"BITMAP ", L"HOTKEY ", L"begin", L"group", L"match", L"nomatch",
  L"SHIFT FREES CAPS", L"CAPS ON ONLY", L"CAPS ALWAYS OFF",
  L"LANGUAGE ", L"LAYOUT ", L"COPYRIGHT ", L"MESSAGE ", L"LANGUAGENAME ",
  L"BITMAPS " };

#define SSN__PREFIX		L"&"

const PWCHAR StoreTokens[] = {
  L"",
  SSN__PREFIX L"BITMAP",
  SSN__PREFIX L"COPYRIGHT",
  SSN__PREFIX L"HOTKEY",
  SSN__PREFIX L"LANGUAGE",
  SSN__PREFIX L"LAYOUT",
  SSN__PREFIX L"MESSAGE",
  SSN__PREFIX L"NAME",
  SSN__PREFIX L"VERSION",
  SSN__PREFIX L"CAPSONONLY",
  SSN__PREFIX L"CAPSALWAYSOFF",
  SSN__PREFIX L"SHIFTFREESCAPS",
  SSN__PREFIX L"LANGUAGENAME",
  L"",
  L"",
  SSN__PREFIX L"ETHNOLOGUECODE",
  L"",
  SSN__PREFIX L"MNEMONICLAYOUT",
  SSN__PREFIX L"INCLUDECODES",
  SSN__PREFIX L"OLDCHARPOSMATCHING",
  L"",
  L"",
  L"",
  L"",
  SSN__PREFIX L"VISUALKEYBOARD",
  SSN__PREFIX L"KMW_RTL",
  SSN__PREFIX L"KMW_HELPFILE",
  SSN__PREFIX L"KMW_HELPTEXT",
  SSN__PREFIX L"KMW_EMBEDJS",
  SSN__PREFIX L"WINDOWSLANGUAGES",
  L"",
  SSN__PREFIX L"PLATFORM",    // read only  // I3430
  SSN__PREFIX L"BASELAYOUT",  // read only  // I3430
  SSN__PREFIX L"LAYER",       // read-write via set?  // I3430
  L"",                        // I3438
  SSN__PREFIX L"LAYOUTFILE",  // I3483
  SSN__PREFIX L"KEYBOARDVERSION",   // I4140
  SSN__PREFIX L"KMW_EMBEDCSS",
  SSN__PREFIX L"TARGETS",   // I4504
  SSN__PREFIX L"CASEDKEYS", // #2241
  SSN__PREFIX L"", // TSS_BEGIN_NEWCONTEXT
  SSN__PREFIX L"", // TSS_BEGIN_POSTKEYSTROKE
  SSN__PREFIX L"NEWLAYER",
  SSN__PREFIX L"OLDLAYER",
  NULL
};

static_assert(_countof(StoreTokens) == TSS__MAX + 2, "StoreTokens should have exactly TSS__MAX+2 elements");

HINSTANCE g_hInstance;
CompilerMessageProc msgproc = NULL;
int currentLine = 0, nErrors = 0;
char CompileDir[MAX_PATH];
int ErrChr;
char ErrExtra[256];
BOOL FSaveDebug, FCompilerWarningsAsErrors, FWarnDeprecatedCode;   // I4865   // I4866
BOOL FShouldAddCompilerVersion = TRUE;
BOOL FOldCharPosMatching = FALSE, FMnemonicLayout = FALSE;
NamedCodeConstants *CodeConstants = NULL;

int BeginLine[4];

/* Compile target */

int CompileTarget;

#define CKF_KEYMAN    0
#define CKF_KEYMANWEB 1

BOOL WINAPI DllMain(HINSTANCE hinst, DWORD fdwReason, LPVOID lpvReserved)
{
  if (fdwReason == DLL_PROCESS_ATTACH) g_hInstance = hinst;
  return TRUE;
}


PWSTR strtowstr(PSTR in)
{
  PWSTR result;
  size_t len;

  mbstowcs_s(&len, NULL, 0, in, strlen(in));  // I3481
  result = new WCHAR[len + 1];
  mbstowcs_s(&len, result, len + 1, in, strlen(in));  // I3481   // I3641
  result[len] = 0;
  return result;
}

PSTR wstrtostr(PWSTR in)
{
  PSTR result;
  size_t len;

  wcstombs_s(&len, NULL, 0, in, wcslen(in));  // I3481
  result = new CHAR[len + 1];
  wcstombs_s(&len, result, len + 1, in, wcslen(in));  // I3481   // I3641
  result[len] = 0;
  return result;
}

BOOL AddCompileString(LPSTR buf)
{
  SetLastError(0);
  (*msgproc)(currentLine + 1, CWARN_Info, buf);
  return FALSE;
}

BOOL AddCompileMessage(DWORD msg)
{
  char szText[SZMAX_ERRORTEXT + 1 + 280];

  SetLastError(0);
  if (msg & CERR_FATAL)
  {
    LoadString(g_hInstance, msg, szText, SZMAX_ERRORTEXT);
    (*msgproc)(currentLine + 1, msg, szText);
    nErrors++;
    return TRUE;
  }

  if (msg & CERR_ERROR) nErrors++;
  LoadString(g_hInstance, msg, szText, SZMAX_ERRORTEXT);
  if (ErrChr > 0)
    wsprintf(strchr(szText, 0), " character offset: %d", ErrChr);
  if (*ErrExtra)
    wsprintf(strchr(szText, 0), " %s", ErrExtra);

  ErrChr = 0; *ErrExtra = 0;

  if (!(*msgproc)(currentLine, msg, szText)) return TRUE;

  return FALSE;
}

extern "C" BOOL __declspec(dllexport) SetCompilerOptions(PCOMPILER_OPTIONS options) {
  if(!options || options->dwSize < sizeof(COMPILER_OPTIONS)) {
    return FALSE;
  }

  FShouldAddCompilerVersion = options->ShouldAddCompilerVersion;
  return TRUE;
}

extern "C" BOOL __declspec(dllexport) CompileKeyboardFile(PSTR pszInfile, PSTR pszOutfile, BOOL ASaveDebug, BOOL ACompilerWarningsAsErrors, BOOL AWarnDeprecatedCode, CompilerMessageProc pMsgProc)   // I4865   // I4866
{
  HANDLE hInfile = INVALID_HANDLE_VALUE, hOutfile = INVALID_HANDLE_VALUE;
  BOOL err;
  DWORD len;
  char str[260];

  FSaveDebug = ASaveDebug;
  FCompilerWarningsAsErrors = ACompilerWarningsAsErrors;   // I4865
  FWarnDeprecatedCode = AWarnDeprecatedCode;   // I4866

  CompileTarget = CKF_KEYMAN;

  if (!pMsgProc || !pszInfile || !pszOutfile) SetError(CERR_BadCallParams);

  PSTR p;
  if (p = strrchr(pszInfile, '\\'))
  {
    strncpy_s(CompileDir, _countof(CompileDir), pszInfile, (INT_PTR)(p - pszInfile + 1));  // I3481
    CompileDir[(INT_PTR)(p - pszInfile + 1)] = 0;
  }
  else
    CompileDir[0] = 0;

  msgproc = pMsgProc;
  currentLine = 0;
  nErrors = 0;

  AddCompileString("NOTE: Using legacy compiler");

  hInfile = CreateFileA(pszInfile, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
  if (hInfile == INVALID_HANDLE_VALUE) SetError(CERR_InfileNotExist);

  // Transfer the file to a memory stream for processing UTF-8 or ANSI to UTF-16?
  // What about really large files?  Transfer to a temp file...

  if (!ReadFile(hInfile, str, 3, &len, NULL))
  {
    CloseHandle(hInfile);
    return CERR_CannotReadInfile;
  }
  SetFilePointer(hInfile, 0, NULL, FILE_BEGIN);
  if (str[0] == UTF8Sig[0] && str[1] == UTF8Sig[1] && str[2] == UTF8Sig[2])
    hInfile = UTF16TempFromUTF8(hInfile, TRUE);
  else if (str[0] == UTF16Sig[0] && str[1] == UTF16Sig[1])
    SetFilePointer(hInfile, 2, NULL, FILE_BEGIN);
  else
    hInfile = UTF16TempFromUTF8(hInfile, FALSE);  // Will fall back to ansi for invalid UTF-8
  if (hInfile == INVALID_HANDLE_VALUE)   // I3228   // I3510
  {
    return CERR_CannotCreateTempfile;
  }

  hOutfile = CreateFileA(pszOutfile, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, 0, NULL);
  if (hOutfile == INVALID_HANDLE_VALUE) SetError(CERR_CannotCreateOutfile);

  DWORD msg;
  FILE_KEYBOARD fk;
  CodeConstants = new NamedCodeConstants;

  err = CompileKeyboardHandle(hInfile, &fk);
  if (err)
  {
    if ((msg = WriteCompiledKeyboard(&fk, hOutfile)) != CERR_None)
      AddCompileMessage(msg);
  }
  else
    AddCompileMessage(CERR_InvalidValue);

  CloseHandle(hInfile);
  CloseHandle(hOutfile);

  delete CodeConstants;

  if (nErrors > 0)
  {
    DeleteFile(pszOutfile);
    return FALSE;
  }

  return err;
}


extern "C" BOOL __declspec(dllexport) CompileKeyboardFileToBuffer(PSTR pszInfile, PFILE_KEYBOARD pfkBuffer, BOOL ACompilerWarningsAsErrors, BOOL AWarnDeprecatedCode, CompilerMessageProc pMsgProc, int Target)   // I4865   // I4866
{
  HANDLE hInfile = INVALID_HANDLE_VALUE;
  BOOL err;
  DWORD len;
  char str[260];

  FSaveDebug = TRUE;   // I3681
  FCompilerWarningsAsErrors = ACompilerWarningsAsErrors;   // I4865
  FWarnDeprecatedCode = AWarnDeprecatedCode;   // I4866

  CompileTarget = Target;

  if (!pMsgProc || !pszInfile || !pfkBuffer) SetError(CERR_BadCallParams);

  PSTR p;
  if (p = strrchr(pszInfile, '\\'))
  {
    strncpy_s(CompileDir, _countof(CompileDir), pszInfile, (INT_PTR)(p - pszInfile + 1));  // I3481
    CompileDir[(INT_PTR)(p - pszInfile + 1)] = 0;
  }
  else
    CompileDir[0] = 0;

  msgproc = pMsgProc;
  currentLine = 0;
  nErrors = 0;

  AddCompileString("NOTE: Using legacy compiler");

  hInfile = CreateFileA(pszInfile, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
  if (hInfile == INVALID_HANDLE_VALUE) SetError(CERR_InfileNotExist);

  // Transfer the file to a memory stream for processing UTF-8 or ANSI to UTF-16?
  // What about really large files?  Transfer to a temp file...

  if (!ReadFile(hInfile, str, 3, &len, NULL))
  {
    CloseHandle(hInfile);
    return CERR_CannotReadInfile;
  }
  SetFilePointer(hInfile, 0, NULL, FILE_BEGIN);
  if (str[0] == UTF8Sig[0] && str[1] == UTF8Sig[1] && str[2] == UTF8Sig[2])
    hInfile = UTF16TempFromUTF8(hInfile, TRUE);
  else if (str[0] == UTF16Sig[0] && str[1] == UTF16Sig[1])
    SetFilePointer(hInfile, 2, NULL, FILE_BEGIN);
  else
    hInfile = UTF16TempFromUTF8(hInfile, FALSE);

  CodeConstants = new NamedCodeConstants;
  err = CompileKeyboardHandle(hInfile, pfkBuffer);
  delete CodeConstants;
  CloseHandle(hInfile);

  if (nErrors > 0)
    return FALSE;

  return err;
}

void GetVersionInfo(DWORD *VersionMajor, DWORD *VersionMinor)
{
  HRSRC hres = FindResource(0, MAKEINTRESOURCE(1), RT_VERSION);
  if (hres)
  {
    HGLOBAL hmem = LoadResource(0, hres);
    PSTR buf = (PSTR)LockResource(hmem);
    *VersionMajor = *((PDWORD)&buf[0x30]);
    *VersionMinor = *((PDWORD)&buf[0x34]);
  }
}

BOOL CompileKeyboardHandle(HANDLE hInfile, PFILE_KEYBOARD fk)
{
  PWSTR str, p;

  DWORD msg;

  FMnemonicLayout = FALSE;

  if (!fk) {
    SetError(CERR_SomewhereIGotItWrong);
  }

  str = new WCHAR[LINESIZE];
  if (!str) {
    SetError(CERR_CannotAllocateMemory);
  }

  fk->KeyboardID = 0;
  fk->version = 0;
  fk->dpStoreArray = NULL;
  fk->dpGroupArray = NULL;
  fk->cxStoreArray = 0;
  fk->cxGroupArray = 0;
  fk->StartGroup[0] = fk->StartGroup[1] = -1;
  fk->szName[0] = 0;
  fk->szCopyright[0] = 0;
  fk->dwFlags = KF_AUTOMATICVERSION;
  fk->currentGroup = 0xFFFFFFFF;
  fk->currentStore = 0;
  fk->cxDeadKeyArray = 0;
  fk->dpDeadKeyArray = NULL;
  fk->cxVKDictionary = 0;  // I3438
  fk->dpVKDictionary = NULL;  // I3438

/*	fk->szMessage[0] = 0;
  fk->szLanguageName[0] = 0;*/
  fk->dwBitmapSize = 0;
  fk->dwHotKey = 0;

  BeginLine[BEGIN_ANSI] = -1;
  BeginLine[BEGIN_UNICODE] = -1;
  BeginLine[BEGIN_NEWCONTEXT] = -1;
  BeginLine[BEGIN_POSTKEYSTROKE] = -1;

  /* Add a store for the Keyman 6.0 copyright information string */

  if(FShouldAddCompilerVersion) {
    DWORD vmajor, vminor;
    GetVersionInfo(&vmajor, &vminor);
    swprintf(str, LINESIZE, L"Created with Keyman Developer version %d.%d.%d.%d", HIWORD(vmajor),
      LOWORD(vmajor), HIWORD(vminor), LOWORD(vminor));  // I3481

    AddStore(fk, TSS_KEYMANCOPYRIGHT, str);
  }

  /* Add a system store for the Keyman edition number */

  swprintf(str, LINESIZE, L"%d", 0);  // I3481
  AddStore(fk, TSS_CUSTOMKEYMANEDITION, str);
  PWSTR tbuf = strtowstr((char*) "Keyman");
  AddStore(fk, TSS_CUSTOMKEYMANEDITIONNAME, tbuf);
  delete tbuf;

  // must preprocess for group and store names -> this isn't really necessary, but never mind!
  while ((msg = ReadLine(hInfile, str, TRUE)) == CERR_None)
  {
    p = str;
    switch (LineTokenType(&p))
    {
    case T_VERSION:
      *(p + 4) = 0;
      if ((msg = AddStore(fk, TSS_VERSION, p)) != CERR_None) SetError(msg);
      break;

    case T_GROUP:
      if ((msg = ProcessGroupLine(fk, p)) != CERR_None) SetError(msg);
      break;

    case T_STORE:
      if ((msg = ProcessStoreLine(fk, p)) != CERR_None) SetError(msg);
      break;

    default:
      break;
    }
  }

  if (msg != CERR_EndOfFile) SetError(msg);

  SetFilePointer(hInfile, 2, NULL, FILE_BEGIN);
  currentLine = 0;

  /* Reindex the list of codeconstants after stores added */

  CodeConstants->reindex();

  /* ReadLine will automatically skip over $Keyman lines, and parse wrapped lines */
  while ((msg = ReadLine(hInfile, str, FALSE)) == CERR_None)
  {
    msg = ParseLine(fk, str);
    if (msg != CERR_None) SetError(msg);
  }

  if (msg != CERR_EndOfFile) SetError(msg);

  ProcessGroupFinish(fk);

  if (FSaveDebug) RecordDeadkeyNames(fk);

  /* Add the compiler version as a system store */
  if ((msg = AddCompilerVersionStore(fk)) != CERR_None) SetError(msg);

  if ((msg = BuildVKDictionary(fk)) != CERR_None) SetError(msg);  // I3438

  if ((msg = CheckFilenameConsistencyForCalls(fk)) != CERR_None) SetError(msg);

  delete str;

  if (!CheckKeyboardFinalVersion(fk)) {
    return FALSE;
  }

  /* Warn on inconsistent use of NCAPS */
  if (!FMnemonicLayout) {
    CheckNCapsConsistency(fk);
  }

  /* Flag presence of deprecated features */
  CheckForDeprecatedFeatures(fk);

  return TRUE;
}

DWORD ProcessBeginLine(PFILE_KEYBOARD fk, PWSTR p)
{
  WCHAR tstr[128];
  PWSTR q, pp;
  int BeginMode;
  DWORD msg;

  pp = p;

  q = wcschr(p, '>');
  if (!q) return CERR_NoTokensFound;

  while (iswspace(*p)) p++;
  if (_wcsnicmp(p, L"unicode", 7) == 0) BeginMode = BEGIN_UNICODE;
  else if (_wcsnicmp(p, L"ansi", 4) == 0) BeginMode = BEGIN_ANSI;
  else if (_wcsnicmp(p, L"newContext", 10) == 0) BeginMode = BEGIN_NEWCONTEXT;
  else if (_wcsnicmp(p, L"postKeystroke", 13) == 0) BeginMode = BEGIN_POSTKEYSTROKE;
  else if (*p != '>') return CERR_InvalidToken;
  else BeginMode = BEGIN_ANSI;

  if(BeginLine[BeginMode] != -1) {
    return CERR_RepeatedBegin;
  }
  BeginLine[BeginMode] = currentLine;

  if ((msg = GetRHS(fk, p, tstr, 80, (int)(INT_PTR)(p - pp), FALSE)) != CERR_None) return msg;

  if (tstr[0] != UC_SENTINEL || tstr[1] != CODE_USE) {
    return CERR_InvalidBegin;
  }
  if (tstr[3] != 0) {
    return CERR_InvalidToken;
  }

  if (BeginMode == BEGIN_ANSI || BeginMode == BEGIN_UNICODE) {
    fk->StartGroup[BeginMode] = tstr[2] - 1;
    // mcd-03-01-2000: removed the secondary group idea; this was undocumented and
    // is not supported under Keyman 5.0: ugly!!
    // if(tstr[3] == UC_SENTINEL && tstr[4] == CODE_USE) fk->StartGroup[1] = tstr[5] - 1;

    if (FSaveDebug) {
      /* Record a system store for the line number of the begin statement */
      AddDebugStore(fk, BeginMode == BEGIN_UNICODE ? DEBUGSTORE_BEGIN L"Unicode" : DEBUGSTORE_BEGIN L"ANSI");
    }
  } else {
    PFILE_GROUP gp = &fk->dpGroupArray[tstr[2] - 1];
    if (!gp->fReadOnly) {
      return BeginMode == BEGIN_NEWCONTEXT ?
        CERR_NewContextGroupMustBeReadonly :
        CERR_PostKeystrokeGroupMustBeReadonly;
    }
    return AddStore(fk, BeginMode == BEGIN_NEWCONTEXT ? TSS_BEGIN_NEWCONTEXT : TSS_BEGIN_POSTKEYSTROKE, tstr, NULL);
  }

  return CERR_None;
}

DWORD ValidateMatchNomatchOutput(PWSTR p) {
  while (p && *p) {
    if (*p == UC_SENTINEL) {
      switch (*(p + 1)) {
      case CODE_CONTEXT:
      case CODE_CONTEXTEX:
      case CODE_INDEX:
        return CERR_ContextAndIndexInvalidInMatchNomatch;
      }
    }
    p = incxstr(p);
  }
  return CERR_None;
}

DWORD ParseLine(PFILE_KEYBOARD fk, PWSTR str)
{
  PWSTR p, q, pp;
  PFILE_GROUP gp;
  DWORD msg;
  int IsUnicode = TRUE; // For NOW!

  p = str;
  pp = str;

  switch (LineTokenType(&p))
  {
  case T_BLANK:
  case T_COMMENT:
    break;	// Ignore the line
  case T_VERSION:
  case T_STORE:
    break;	// The line has already been processed

  case T_BEGIN:
    // after a begin can be "Unicode", "ANSI", "NewContext", "PostKeystroke", or nothing (=ANSI)
    if ((msg = ProcessBeginLine(fk, p)) != CERR_None) return msg;
    break;

  case T_GROUP:
    if (fk->currentGroup == 0xFFFFFFFF) fk->currentGroup = 0;
    else
    {
      if ((msg = ProcessGroupFinish(fk)) != CERR_None) return msg;		// finish off previous group first?
      fk->currentGroup++;
    }
    //		if( (err = ProcessGroupLine( fk, p )) != CERR_None ) return err;
    break;

  case T_NAME:
    WarnDeprecatedHeader();   // I4866
    q = GetDelimitedString(&p, L"\"\"", 0);
    if (!q) return CERR_InvalidName;

    if ((msg = AddStore(fk, TSS_NAME, q)) != CERR_None) return msg;
    break;

  case T_COPYRIGHT:
    WarnDeprecatedHeader();   // I4866
    q = GetDelimitedString(&p, L"\"\"", 0);
    if (!q) return CERR_InvalidCopyright;

    if ((msg = AddStore(fk, TSS_COPYRIGHT, q)) != CERR_None) return msg;
    break;

  case T_MESSAGE:
    WarnDeprecatedHeader();   // I4866
    q = GetDelimitedString(&p, L"\"\"", 0);
    if (!q) return CERR_InvalidMessage;

    if ((msg = AddStore(fk, TSS_MESSAGE, q)) != CERR_None) return msg;
    break;

  case T_LANGUAGENAME:
    WarnDeprecatedHeader();   // I4866
    q = GetDelimitedString(&p, L"\"\"", 0);
    if (!q) return CERR_InvalidLanguageName;

    if ((msg = AddStore(fk, TSS_LANGUAGENAME, q)) != CERR_None) return msg;
    break;

  case T_LANGUAGE:
  {
    WarnDeprecatedHeader();   // I4866
    wchar_t *tokcontext = NULL;
    q = wcstok_s(p, L"\n", &tokcontext);  // I3481
    if ((msg = AddStore(fk, TSS_LANGUAGE, q)) != CERR_None) return msg;
    break;
  }
  case T_LAYOUT:
  {
    WarnDeprecatedHeader();   // I4866
    wchar_t *tokcontext = NULL;
    q = wcstok_s(p, L"\n", &tokcontext);  // I3481
    if ((msg = AddStore(fk, TSS_LAYOUT, q)) != CERR_None) return msg;
    break;
  }
  case T_CAPSOFF:
    WarnDeprecatedHeader();   // I4866
    if ((msg = AddStore(fk, TSS_CAPSALWAYSOFF, L"1")) != CERR_None) return msg;
    break;

  case T_CAPSON:
    WarnDeprecatedHeader();   // I4866
    if ((msg = AddStore(fk, TSS_CAPSONONLY, L"1")) != CERR_None) return msg;
    break;

  case T_SHIFT:
    WarnDeprecatedHeader();   // I4866
    if ((msg = AddStore(fk, TSS_SHIFTFREESCAPS, L"1")) != CERR_None) return msg;
    break;

  case T_HOTKEY:
  {
    WarnDeprecatedHeader();   // I4866
    wchar_t *tokcontext = NULL;
    if ((q = wcstok_s(p, L"\n", &tokcontext)) == NULL) return CERR_CodeInvalidInThisSection;  // I3481
    if ((msg = AddStore(fk, TSS_HOTKEY, q)) != CERR_None) return msg;
    break;
  }
  case T_BITMAP:
  {
    WarnDeprecatedHeader();   // I4866
    wchar_t *tokcontext = NULL;
    if ((q = wcstok_s(p, L"\n", &tokcontext)) == NULL) return CERR_InvalidBitmapLine;  // I3481

    while (iswspace(*q)) q++;
    if (*q == '"') {
      p = q;
      q = GetDelimitedString(&p, L"\"\"", 0);
      if (!q) return CERR_InvalidBitmapLine;
    }

    if ((msg = AddStore(fk, TSS_BITMAP, q)) != CERR_None) return msg;
    break;
  }
  case T_BITMAPS:
  {
    WarnDeprecatedHeader();   // I4866
    wchar_t *tokcontext = NULL;
    AddWarning(CWARN_BitmapNotUsed);

    if ((q = wcstok_s(p, L"\n", &tokcontext)) == NULL) return CERR_InvalidBitmapLine;  // I3481
    if (wcschr(q, ',')) *wcschr(q, ',') = 0;
    if ((msg = AddStore(fk, TSS_BITMAP, q)) != CERR_None) return msg;

    break;
  }
  case T_KEYTOKEY:			// A rule
    if (fk->currentGroup == 0xFFFFFFFF) return CERR_CodeInvalidInThisSection;
    if ((msg = ProcessKeyLine(fk, p, IsUnicode)) != CERR_None) return msg;
    break;

  case T_MATCH:
    if (fk->currentGroup == 0xFFFFFFFF) return CERR_CodeInvalidInThisSection;
    {
      PWCHAR buf = new WCHAR[GLOBAL_BUFSIZE];
      if ((msg = GetRHS(fk, p, buf, GLOBAL_BUFSIZE - 1, (int)(INT_PTR)(p - pp), IsUnicode)) != CERR_None)
      {
        delete buf;
        return msg;
      }

      if ((msg = ValidateMatchNomatchOutput(buf)) != CERR_None) {
        delete buf;
        return msg;
      }

      gp = &fk->dpGroupArray[fk->currentGroup];

      gp->dpMatch = new WCHAR[wcslen(buf) + 1];
      wcscpy_s(gp->dpMatch, wcslen(buf) + 1, buf);  // I3481

      delete buf;

      if (FSaveDebug)
      {
        WCHAR tstr[128];
        //char buf[256];
        //swprintf(tstr, "%d", fk->currentGroup);
        /* Record a system store for the line number of the begin statement */
        //wcscpy(tstr, DEBUGSTORE_MATCH);

        //wcscat(tstr, pw);

        swprintf(tstr, _countof(tstr), L"%ls%d %ls", DEBUGSTORE_MATCH, (int) fk->currentGroup, gp->szName);  // I3481
        AddDebugStore(fk, tstr);
      }
    }
    break;

  case T_NOMATCH:
    if (fk->currentGroup == 0xFFFFFFFF) return CERR_CodeInvalidInThisSection;
    {
      PWCHAR buf = new WCHAR[GLOBAL_BUFSIZE];
      if ((msg = GetRHS(fk, p, buf, GLOBAL_BUFSIZE, (int)(INT_PTR)(p - pp), IsUnicode)) != CERR_None)
      {
        delete[] buf;
        return msg;
      }

      if ((msg = ValidateMatchNomatchOutput(buf)) != CERR_None) {
        delete[] buf;
        return msg;
      }

      gp = &fk->dpGroupArray[fk->currentGroup];

      gp->dpNoMatch = new WCHAR[wcslen(buf) + 1];
      wcscpy_s(gp->dpNoMatch, wcslen(buf) + 1, buf);  // I3481

      delete[] buf;

      if (FSaveDebug)
      {
        WCHAR tstr[128];
        /* Record a system store for the line number of the begin statement */
        swprintf(tstr, _countof(tstr), L"%ls%d %ls", DEBUGSTORE_NOMATCH, fk->currentGroup, gp->szName);  // I3481
        AddDebugStore(fk, tstr);
      }
    }
    break;

  default:
    return CERR_InvalidToken;
  }

  return CERR_None;
}

//**********************************************************************************************************************

DWORD ProcessGroupLine(PFILE_KEYBOARD fk, PWSTR p)
{
  PFILE_GROUP gp;
  PWSTR q;

  gp = new FILE_GROUP[fk->cxGroupArray + 1];
  if (!gp) return CERR_CannotAllocateMemory;

  if (fk->dpGroupArray)
  {
    memcpy(gp, fk->dpGroupArray, sizeof(FILE_GROUP) * fk->cxGroupArray);
    delete fk->dpGroupArray;
  }

  fk->dpGroupArray = gp;
  gp = &fk->dpGroupArray[fk->cxGroupArray];
  fk->cxGroupArray++;

  gp->dpKeyArray = NULL;
  gp->dpMatch = NULL;
  gp->dpNoMatch = NULL;
  gp->cxKeyArray = 0;

  q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
  if (!q) return CERR_InvalidGroupLine;

  gp->fUsingKeys = FALSE;
  gp->fReadOnly  = IsSameToken(&p, L"readonly");
  if (!gp->fReadOnly) {
    if (IsSameToken(&p, L"using") && IsSameToken(&p, L"keys"))
      gp->fUsingKeys = TRUE;
  }

  safe_wcsncpy(gp->szName, q, SZMAX_GROUPNAME);

  gp->Line = currentLine;

  if (FSaveDebug)
  {
    WCHAR tstr[128];
    /* Record a system store for the line number of the begin statement */
    swprintf(tstr, _countof(tstr), L"%ls%d %ls", DEBUGSTORE_GROUP, fk->cxGroupArray - 1, gp->szName);  // I3481
    AddDebugStore(fk, tstr);
  }

  return CheckForDuplicateGroup(fk, gp);
}

int cmpkeys(const void *key, const void *elem)
{
  PFILE_KEY akey, aelem;
  int l1, l2;
  WCHAR char_key, char_elem;
  akey = (PFILE_KEY)key;
  aelem = (PFILE_KEY)elem;
  char_key = VKToChar(akey->Key, akey->ShiftFlags);
  char_elem = VKToChar(aelem->Key, aelem->ShiftFlags);
  if (char_key == char_elem) //akey->Key == aelem->Key)
  {
    l1 = xstrlen(akey->dpContext); l2 = xstrlen(aelem->dpContext);
    if (l1 == l2)
    {
      if (akey->Line < aelem->Line) return -1;
      if (akey->Line > aelem->Line) return 1;
      if(akey->Key == aelem->Key) {
        if(akey->ShiftFlags == aelem->ShiftFlags) {
          return akey->LineStoreIndex - aelem->LineStoreIndex;
        }
        return akey->ShiftFlags - aelem->ShiftFlags;
      }
      return akey->Key - aelem->Key;
    }
    if (l1 < l2) return 1;
    if (l1 > l2) return -1;
    if(akey->Key == aelem->Key) {
      if(akey->ShiftFlags == aelem->ShiftFlags) {
        return akey->LineStoreIndex - aelem->LineStoreIndex;
      }
      return akey->ShiftFlags - aelem->ShiftFlags;
    }
    return akey->Key - aelem->Key;
  }
  return(char_key - char_elem); // akey->Key - aelem->Key);
}

DWORD ProcessGroupFinish(PFILE_KEYBOARD fk)
{
  PFILE_GROUP gp;
  DWORD msg;

  if (fk->currentGroup == 0xFFFFFFFF) return CERR_None;
  // Just got to first group - so nothing to finish yet

  gp = &fk->dpGroupArray[fk->currentGroup];

  // Finish off the previous group stuff!
  if ((msg = ExpandCapsRulesForGroup(fk, gp)) != CERR_None) return msg;
  qsort(gp->dpKeyArray, gp->cxKeyArray, sizeof(FILE_KEY), cmpkeys);

  return VerifyUnreachableRules(gp);
}

/***************************************
* Store management
*/

DWORD ProcessStoreLine(PFILE_KEYBOARD fk, PWSTR p)
{
  PWSTR q, pp;
  PFILE_STORE sp;
  //WCHAR temp[GLOBAL_BUFSIZE];
  DWORD msg;
  int i = 0;

  pp = p;

  if ((q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL)) == NULL) return CERR_InvalidStoreLine;

  if (*q == *SSN__PREFIX)
  {
    for (i = 0; StoreTokens[i]; i++)
      if (!_wcsicmp(q, StoreTokens[i]))  // I3481
        break;
    if (!StoreTokens[i]) return CERR_InvalidSystemStore;
  }

  sp = new FILE_STORE[fk->cxStoreArray + 1];
  if (!sp) return CERR_CannotAllocateMemory;

  if (fk->dpStoreArray)
  {
    memcpy(sp, fk->dpStoreArray, sizeof(FILE_STORE) * fk->cxStoreArray);
    delete fk->dpStoreArray;
  }

  fk->dpStoreArray = sp;
  sp = &fk->dpStoreArray[fk->cxStoreArray];

  sp->line = currentLine;
  sp->fIsOption = FALSE;
  sp->fIsReserved = FALSE;
  sp->fIsStore = FALSE;
  sp->fIsDebug = FALSE;
  sp->fIsCall = FALSE;

  safe_wcsncpy(sp->szName, q, SZMAX_STORENAME);
  {
    PWCHAR temp = new WCHAR[GLOBAL_BUFSIZE];

    if ((msg = GetXString(fk, p, L"c\n", temp, GLOBAL_BUFSIZE - 1, (int)(INT_PTR)(p - pp), &p, FALSE, TRUE)) != CERR_None)
    {
      delete[] temp;
      return msg;
    }

    sp->dwSystemID = i;
    sp->dpString = new WCHAR[wcslen(temp) + 1];
    wcscpy_s(sp->dpString, wcslen(temp) + 1, temp);  // I3481

    delete[] temp;
  }

  if (xstrlen(sp->dpString) == 1 && *sp->dpString != UC_SENTINEL &&
    sp->dwSystemID == 0 && (fk->version >= VERSION_60 || fk->version == 0))
  {
    // In this case, we want to change behaviour for older versioned keyboards so that
    // we don't mix up named character codes which weren't supported in 5.x
    VERIFY_KEYBOARD_VERSION(fk, VERSION_60, CERR_60FeatureOnly_NamedCodes);
    // Add a single char store as a defined character constant
    if (Uni_IsSurrogate1(*sp->dpString))
      CodeConstants->AddCode(Uni_SurrogateToUTF32(sp->dpString[0], sp->dpString[1]), sp->szName, fk->cxStoreArray);
    else
      CodeConstants->AddCode(sp->dpString[0], sp->szName, fk->cxStoreArray);
    CodeConstants->reindex(); // has to be done after every character add due to possible use in another store.   // I4982
  }

  fk->cxStoreArray++;	// increment now, because GetXString refers to stores

  if (i > 0)
    if ((msg = ProcessSystemStore(fk, i, sp)) != CERR_None) return msg;

  return CheckForDuplicateStore(fk, sp);
}

DWORD AddStore(PFILE_KEYBOARD fk, DWORD SystemID, PWSTR str, DWORD *dwStoreID)
{
  PFILE_STORE sp;

  sp = new FILE_STORE[fk->cxStoreArray + 1];
  if (!sp) return CERR_CannotAllocateMemory;

  if (fk->dpStoreArray)
  {
    memcpy(sp, fk->dpStoreArray, sizeof(FILE_STORE) * fk->cxStoreArray);
    delete fk->dpStoreArray;
  }

  fk->dpStoreArray = sp;
  sp = &fk->dpStoreArray[fk->cxStoreArray];

  sp->line = currentLine;
  sp->fIsOption = FALSE;   // I3686
  sp->fIsReserved = (SystemID != TSS_NONE);
  sp->fIsStore = FALSE;
  sp->fIsDebug = FALSE;
  sp->fIsCall = FALSE;

  safe_wcsncpy(sp->szName, StoreTokens[SystemID], SZMAX_STORENAME);

  sp->dpString = new WCHAR[wcslen(str) + 1];
  wcscpy_s(sp->dpString, wcslen(str) + 1, str);  // I3481

  sp->dwSystemID = SystemID;

  if (dwStoreID) *dwStoreID = fk->cxStoreArray;

  fk->cxStoreArray++;

  return ProcessSystemStore(fk, SystemID, sp);
}

DWORD AddDebugStore(PFILE_KEYBOARD fk, PWSTR str)
{
  PFILE_STORE sp;
  WCHAR tstr[16];

  swprintf(tstr, _countof(tstr), L"%d", currentLine);  // I3481

  sp = new FILE_STORE[fk->cxStoreArray + 1];
  if (!sp) return CERR_CannotAllocateMemory;

  if (fk->dpStoreArray)
  {
    memcpy(sp, fk->dpStoreArray, sizeof(FILE_STORE) * fk->cxStoreArray);
    delete[] fk->dpStoreArray;
  }

  fk->dpStoreArray = sp;
  sp = &fk->dpStoreArray[fk->cxStoreArray];

  safe_wcsncpy(sp->szName, str, SZMAX_STORENAME);

  sp->dpString = new WCHAR[wcslen(tstr) + 1];
  wcscpy_s(sp->dpString, wcslen(tstr) + 1, tstr);  // I3481
  sp->line = 0;
  sp->fIsOption = FALSE;
  sp->fIsReserved = TRUE;
  sp->fIsStore = FALSE;
  sp->fIsDebug = TRUE;
  sp->fIsCall = FALSE;
  sp->dwSystemID = TSS_DEBUG_LINE;
  fk->cxStoreArray++;

  return CERR_None;
}

PWCHAR pssBuf = NULL;

DWORD ProcessSystemStore(PFILE_KEYBOARD fk, DWORD SystemID, PFILE_STORE sp)
{
  //WCHAR buf[GLOBAL_BUFSIZE];
  int i, j;
  DWORD msg;
  PWSTR p, q;
  char *pp;

  if (!pssBuf) pssBuf = new WCHAR[GLOBAL_BUFSIZE];
  PWCHAR buf = pssBuf;

  switch (SystemID)
  {
  case TSS_BITMAP:
    if ((msg = ImportBitmapFile(fk, sp->dpString, &fk->dwBitmapSize, &fk->lpBitmap)) != CERR_None)
      return msg;
    break;

  case TSS_CALLDEFINITION:
    break;

  case TSS_CALLDEFINITION_LOADFAILED:
    break;

  case TSS_CAPSALWAYSOFF:
    if (*sp->dpString == L'1') fk->dwFlags |= KF_CAPSALWAYSOFF;
    break;

  case TSS_CAPSONONLY:
    if (*sp->dpString == L'1') fk->dwFlags |= KF_CAPSONONLY;
    break;

  case TSS_COMPILEDVERSION:
    break;

  case TSS_COPYRIGHT:
    break;

  case TSS_DEBUG_LINE:
    break;

  case TSS_ETHNOLOGUECODE:
    VERIFY_KEYBOARD_VERSION(fk, VERSION_60, CERR_60FeatureOnly_EthnologueCode);
    if ((msg = ProcessEthnologueStore(sp->dpString)) != CERR_None) return msg;  // I2646
    break;

  case TSS_HOTKEY:
    if ((msg = ProcessHotKey(sp->dpString, &fk->dwHotKey)) != CERR_None) return msg;

    swprintf(buf, GLOBAL_BUFSIZE, L"%d", (int)fk->dwHotKey);  // I3481
    delete[] sp->dpString;
    sp->dpString = new WCHAR[wcslen(buf) + 1];
    wcscpy_s(sp->dpString, wcslen(buf) + 1, buf);  // I3481
    break;

  case TSS_INCLUDECODES:
    VERIFY_KEYBOARD_VERSION(fk, VERSION_60, CERR_60FeatureOnly_NamedCodes);
    pp = wstrtostr(sp->dpString);
    if (!CodeConstants->LoadFile(pp))
    {
      delete[] pp;
      return CERR_CannotLoadIncludeFile;
    }
    delete[] pp;
    CodeConstants->reindex();   // I4982
    break;

  case TSS_LANGUAGE:
  {
    wchar_t *context = NULL;
    q = wcstok_s(sp->dpString, L", ", &context);  // I3481
    if (!q) return CERR_InvalidLanguageLine;

    i = xatoi(&q);
    q = wcstok_s(NULL, L" c\n", &context);  // I3481
    if (!q)
    {
      VERIFY_KEYBOARD_VERSION(fk, VERSION_70, CERR_InvalidLanguageLine);
      j = SUBLANGID(i);
      i = PRIMARYLANGID(i);
    }
    else
      j = xatoi(&q);

    if (i < 1 || j < 1 || i > 0x3FF || j > 0x3F) return CERR_InvalidLanguageLine;
    if (i >= 0x200 || j >= 0x20) AddWarning(CWARN_CustomLanguagesNotSupported);

    fk->KeyboardID = (DWORD)MAKELANGID(i, j);

    swprintf(buf, GLOBAL_BUFSIZE, L"%x %x", i, j);  // I3481
    delete[] sp->dpString;
    sp->dpString = new WCHAR[wcslen(buf) + 1];
    wcscpy_s(sp->dpString, wcslen(buf) + 1, buf);  // I3481

    break;
  }
  case TSS_LANGUAGENAME:
    break;

  case TSS_LAYOUT:
    if (fk->KeyboardID == 0) return CERR_LayoutButNoLanguage;

    q = sp->dpString;

    fk->KeyboardID |= (xatoi(&q) << 16L);
    break;

  case TSS_MESSAGE:
    break;

  case TSS_MNEMONIC:
    VERIFY_KEYBOARD_VERSION(fk, VERSION_60, CERR_60FeatureOnly_MnemonicLayout);
    FMnemonicLayout = atoiW(sp->dpString) == 1;
    if (FMnemonicLayout && FindSystemStore(fk, TSS_CASEDKEYS) != NULL) {
      // The &CasedKeys system store is not supported for
      // mnemonic layouts
      return CERR_CasedKeysNotSupportedWithMnemonicLayout;
    }
    break;

  case TSS_NAME:
    break;

  case TSS_OLDCHARPOSMATCHING:
    VERIFY_KEYBOARD_VERSION(fk, VERSION_60, CERR_60FeatureOnly_OldCharPosMatching);
    FOldCharPosMatching = atoiW(sp->dpString);
    break;

  case TSS_SHIFTFREESCAPS:
    if (*sp->dpString == L'1') fk->dwFlags |= KF_SHIFTFREESCAPS;
    break;

  case TSS_VERSION:
    if ((fk->dwFlags & KF_AUTOMATICVERSION) == 0) return CERR_VersionAlreadyIncluded;
    p = sp->dpString;
    if (wcstof(p, NULL) < 5.0) {
      AddWarning(CWARN_OldVersion);
    }

    if (wcsncmp(p, L"3.0", 3) == 0)       fk->version = VERSION_50;   //0x0a0b000n= a.bn
    else if (wcsncmp(p, L"3.1", 3) == 0)  fk->version = VERSION_50;   //all versions < 5.0
    else if (wcsncmp(p, L"3.2", 3) == 0)  fk->version = VERSION_50;   //we compile as if
    else if (wcsncmp(p, L"4.0", 3) == 0)  fk->version = VERSION_50;   //they are 5.0.100.0
    else if (wcsncmp(p, L"5.01", 4) == 0) fk->version = VERSION_501;
    else if (wcsncmp(p, L"5.0", 3) == 0)  fk->version = VERSION_50;
    else if (wcsncmp(p, L"6.0", 3) == 0)  fk->version = VERSION_60;
    else if (wcsncmp(p, L"7.0", 3) == 0)  fk->version = VERSION_70;
    else if (wcsncmp(p, L"8.0", 3) == 0)  fk->version = VERSION_80;
    else if (wcsncmp(p, L"9.0", 3) == 0)  fk->version = VERSION_90;
    else if (wcsncmp(p, L"10.0", 4) == 0)  fk->version = VERSION_100;
    else if (wcsncmp(p, L"14.0", 4) == 0)  fk->version = VERSION_140; // Adds support for #917 -- context() with notany() for KeymanWeb
    else if (wcsncmp(p, L"15.0", 4) == 0)  fk->version = VERSION_150; // Adds support for U_xxxx_yyyy #2858
    else if (wcsncmp(p, L"16.0", 4) == 0)  fk->version = VERSION_160; // KMXPlus
    else return CERR_InvalidVersion;

    if (fk->version < VERSION_60) FOldCharPosMatching = TRUE;

    fk->dwFlags &= ~KF_AUTOMATICVERSION;

    break;

  case TSS_VISUALKEYBOARD:
    VERIFY_KEYBOARD_VERSION(fk, VERSION_70, CERR_70FeatureOnly);
    {
      // Strip path from the store, leaving bare filename only
      p = sp->dpString;
      wchar_t *pp = wcsrchr(p, L'\\');
      if (!pp) {
        pp = p;
      } else {
        pp++;
      }
      q = new WCHAR[wcslen(pp) + 1];
      wcscpy_s(q, wcslen(pp) + 1, pp);

      // Change compiled reference file extension to .kvk
      pp = wcschr(q, 0) - 5;
      if (pp > q && _wcsicmp(pp, L".kvks") == 0) {
        pp[4] = 0;
      }

      delete[] sp->dpString;
      sp->dpString = q;

      if ((msg = CheckFilenameConsistency(sp->dpString, FALSE)) != CERR_None) {
        return msg;
      }
    }
    break;
  case TSS_KMW_RTL:
  case TSS_KMW_HELPTEXT:
    VERIFY_KEYBOARD_VERSION(fk, VERSION_70, CERR_70FeatureOnly);
    break;

  case TSS_KMW_HELPFILE:
  case TSS_KMW_EMBEDJS:
    VERIFY_KEYBOARD_VERSION(fk, VERSION_70, CERR_70FeatureOnly);
    if ((msg = CheckFilenameConsistency(sp->dpString, FALSE)) != CERR_None) {
      return msg;
    }
    break;

  case TSS_KMW_EMBEDCSS:
    VERIFY_KEYBOARD_VERSION(fk, VERSION_90, CERR_90FeatureOnlyEmbedCSS);
    if ((msg = CheckFilenameConsistency(sp->dpString, FALSE)) != CERR_None) {
      return msg;
    }
    break;

  case TSS_TARGETS:   // I4504
    VERIFY_KEYBOARD_VERSION(fk, VERSION_90, CERR_90FeatureOnlyTargets);
    break;

  case TSS_WINDOWSLANGUAGES:
  {
    wchar_t *context = NULL;
    VERIFY_KEYBOARD_VERSION(fk, VERSION_70, CERR_70FeatureOnly);
    size_t szQ = wcslen(sp->dpString) * 6 + 1;  // I3481
    q = new WCHAR[szQ]; // guaranteed to be enough space for recoding
    *q = 0; WCHAR *r = q;
    p = wcstok_s(sp->dpString, L" ", &context);  // I3481
    while (p)
    {
      int n = xatoi(&p);

      j = SUBLANGID(n);
      i = PRIMARYLANGID(n);

      if (i < 1 || j < 1 || i > 0x3FF || j > 0x3F) {
        delete[] q;
        return CERR_InvalidLanguageLine;
      }

      swprintf(r, szQ - (size_t)(r - q), L"x%04.4x ", n);  // I3481

      p = wcstok_s(NULL, L" ", &context);  // I3481
      r = wcschr(q, 0);  // I3481
    }
    delete[] sp->dpString;
    if (*q) *(wcschr(q, 0) - 1) = 0; // delete final space - safe because we control the formatting - ugly? scared?
    sp->dpString = q;
    break;
  }
  case TSS_COMPARISON:
    VERIFY_KEYBOARD_VERSION(fk, VERSION_80, CERR_80FeatureOnly);
    break;

  case TSS_VKDICTIONARY:  // I3438
    VERIFY_KEYBOARD_VERSION(fk, VERSION_90, CERR_90FeatureOnlyVirtualKeyDictionary);
    break;

  case TSS_LAYOUTFILE:  // I3483
    VERIFY_KEYBOARD_VERSION(fk, VERSION_90, CERR_90FeatureOnlyLayoutFile);   // I4140
    if ((msg = CheckFilenameConsistency(sp->dpString, FALSE)) != CERR_None) {
      return msg;
    }
    // Used by KMW compiler
    break;

  case TSS_KEYBOARDVERSION:   // I4140
    VERIFY_KEYBOARD_VERSION(fk, VERSION_90, CERR_90FeatureOnlyKeyboardVersion);
    if (!IsValidKeyboardVersion(sp->dpString)) {
      return CERR_KeyboardVersionFormatInvalid;
    }

    break;

  case TSS_CASEDKEYS:
    if ((msg = VerifyCasedKeys(sp)) != CERR_None) {
      return msg;
    }
    break;

  case TSS_BEGIN_NEWCONTEXT:
  case TSS_BEGIN_POSTKEYSTROKE:
    break;

  case TSS_NEWLAYER:
  case TSS_OLDLAYER:
    break;

  default:
    return CERR_InvalidSystemStore;
  }
  return CERR_None;
}

BOOL IsValidKeyboardVersion(WCHAR *dpString) {   // I4140
  /* version format \d+(\.\d+)*  e.g. 9.0.3, 1.0, 1.2.3.4, 6.2.1.4.6.4, blank is not allowed */

  do {
    if (!iswdigit(*dpString)) {
      return FALSE;
    }
    while (iswdigit(*dpString)) {
      dpString++;
    }
    if (*dpString == '.') {
      dpString++;
      if (!iswdigit(*dpString)) {
        return FALSE;
      }
    }
  } while (*dpString != 0);

  return TRUE;
}

BOOL GetFileVersion(char *filename, WORD *d1, WORD *d2, WORD *d3, WORD *d4)
{
  char fnbuf[260];
  DWORD h;
  DWORD sz;
  PSTR p;
  VS_FIXEDFILEINFO *vffi;
  UINT len;

  GetModuleFileName(0, fnbuf, 260);
  sz = GetFileVersionInfoSize(fnbuf, &h);
  if (sz == 0) return FALSE;
  p = new char[sz];
  if (!p) return FALSE;
  GetFileVersionInfo(fnbuf, h, sz, p);
  VerQueryValue(p, "\\", (void **)&vffi, &len);

  *d1 = HIWORD(vffi->dwFileVersionMS);
  *d2 = LOWORD(vffi->dwFileVersionMS);
  *d3 = HIWORD(vffi->dwFileVersionLS);
  *d4 = LOWORD(vffi->dwFileVersionLS);

  delete[] p;
  return TRUE;
}

DWORD AddCompilerVersionStore(PFILE_KEYBOARD fk)
{
  if(!FShouldAddCompilerVersion) {
    return CERR_None;
  }

  WCHAR verstr[32];
  WORD d1, d2, d3, d4;
  DWORD msg;

  GetFileVersion(NULL, &d1, &d2, &d3, &d4);
  swprintf(verstr, _countof(verstr), L"%d.%d.%d.%d", d1, d2, d3, d4);  // I3481

  if ((msg = AddStore(fk, TSS_COMPILEDVERSION, verstr)) != CERR_None) return msg;

  return CERR_None;
}

/****************************
* Rule lines
*/

DWORD CheckStatementOffsets(PFILE_KEYBOARD fk, PFILE_GROUP gp, PWSTR context, PWSTR output, PWSTR key) {
  PWSTR p, q;
  int i;
  for (p = output; *p; p = incxstr(p)) {
    if (*p == UC_SENTINEL) {
      if (*(p + 1) == CODE_INDEX) {
        int indexStore = *(p + 2) - 1;
        int contextOffset = *(p + 3);
        for (q = context, i = 1; *q && i < contextOffset; q = incxstr(q), i++);

        if (*q == 0) {
          if (!gp->fUsingKeys)
            // no key in the rule, so offset is past end of context
            return CERR_IndexDoesNotPointToAny;
          if (i < contextOffset) // I4914
            // offset is beyond the key
            return CERR_IndexDoesNotPointToAny;
          q = key;
        }

        // find the any
        if (*q != UC_SENTINEL || *(q + 1) != CODE_ANY)
          return CERR_IndexDoesNotPointToAny;

        int anyStore = *(q + 2) - 1;

        if (xstrlen(fk->dpStoreArray[indexStore].dpString) < xstrlen(fk->dpStoreArray[anyStore].dpString)) {
          AddWarning(CWARN_IndexStoreShort); //TODO: if this fails, then we return FALSE instead of an error
        }
      } else if (*(p + 1) == CODE_CONTEXTEX) {
        int contextOffset = *(p + 2);
        if (contextOffset > xstrlen(context))
          return CERR_ContextExHasInvalidOffset;

        // Due to a limitation in earlier versions of KeymanWeb, the minimum version
        // for context() referring to notany() is 14.0. See #917 for details.
        if (CompileTarget == CKF_KEYMANWEB) {
          for (q = context, i = 1; *q && i < contextOffset; q = incxstr(q), i++);
          if (*q == UC_SENTINEL && *(q + 1) == CODE_NOTANY) {
            VERIFY_KEYBOARD_VERSION(fk, VERSION_140, CERR_140FeatureOnlyContextAndNotAnyWeb);
          }
        }
      }
    }
  }
  return CERR_None;
}

/**
 * Checks that the order of statements in the context matches the specification
 *   Rule structure: [context] ['+' key] '>' output
 *   Context structure: [nul] [if()|baselayout()|platform()]+ [char|any|context()|deadkey()|dk()|index()|notany()|outs()]
 * Test that nul is first, then if(), baselayout(), platform() statements are before any other content
 */
BOOL CheckContextStatementPositions(PWSTR context) {
  BOOL hadContextChar = FALSE;
  for (PWSTR p = context; *p; p = incxstr(p)) {
    if (*p == UC_SENTINEL) {
      switch (*(p + 1)) {
      case CODE_NUL:
        if (p > context) {
          AddWarning(CWARN_NulNotFirstStatementInContext);
        }
        break;
      case CODE_IFOPT:
      case CODE_IFSYSTEMSTORE:
        if (hadContextChar) {
          AddWarning(CWARN_IfShouldBeAtStartOfContext);
        }
        break;
      default:
        hadContextChar = TRUE;
      }
    }
    else {
      hadContextChar = TRUE;
    }
  }

  return TRUE;
}

/**
 * Checks if a use() statement is followed by other content in the output of a rule
 */
DWORD
CheckUseStatementsInOutput(const PFILE_GROUP gp, const PWSTR output) {  // I4867
  BOOL hasUse = FALSE;
  PWSTR p;
  for (p = output; *p; p = incxstr(p)) {
    if (*p == UC_SENTINEL && *(p + 1) == CODE_USE) {
      hasUse = TRUE;
    } else if (hasUse) {
      AddWarning(CWARN_UseNotLastStatementInRule);
      break;
    }
  }
  return CERR_None;
}

/**
 * Adds implicit `context` to start of output of rules for readonly groups
 */
DWORD
InjectContextToReadonlyOutput(PWSTR pklOut) {
  if (pklOut[0] != UC_SENTINEL || pklOut[1] != CODE_CONTEXT) {
    if (wcslen(pklOut) > GLOBAL_BUFSIZE - 3) {
      return CERR_CannotAllocateMemory;
    }
    memmove(pklOut + 2, pklOut, (wcslen(pklOut) + 1) * 2);
    pklOut[0] = UC_SENTINEL;
    pklOut[1] = CODE_CONTEXT;
  }
  return CERR_None;
}

/**
 * Verifies that a keyboard does not attempt to emit characters or
 * other changes to text store when processing a readonly group
 */
DWORD
CheckOutputIsReadonly(const PFILE_KEYBOARD fk, const PWSTR output) {  // I4867
  PWSTR p;
  for (p = output; *p; p = incxstr(p)) {
    if (*p != UC_SENTINEL) {
      return CERR_OutputInReadonlyGroup;
    }
    switch (*(p + 1)) {
    case CODE_CALL:
      // We cannot be sure that the callee is going to be readonly
      // but we have to operate on a trust basis for call() in any
      // case, so we'll allow it.
      continue;
    case CODE_USE:
      // We only allow use() of other readonly groups
      {
        PFILE_GROUP targetGroup = &fk->dpGroupArray[*(p + 2) - 1];
        if (!targetGroup->fReadOnly) {
          return CERR_CannotUseReadWriteGroupFromReadonlyGroup;
        }
      }
      continue;
    case CODE_SETOPT:
    case CODE_RESETOPT:
    case CODE_SAVEOPT:
      // it is okay to set, reset or save keyboard options
      // although it's hard to see good use cases for this
      continue;
    case CODE_SETSYSTEMSTORE:
      // it is okay to set system stores; Engine or Core will
      // ignore set(&) that are not permissible in the given context
      continue;
    case CODE_CONTEXT:
      // We allow `context` but only as the very first statement in output
      if (p == output) {
        continue;
      }
      return CERR_OutputInReadonlyGroup;
    default:
      // Note: conceptually, CODE_NUL could be transformed to CODE_CONTEXT
      // if the context was also empty, but it is probably safest to avoid this,
      // given CODE_CONTEXT does what we need anyway
      return CERR_StatementNotPermittedInReadonlyGroup;
    }
  }
  return CERR_None;
}

  DWORD ProcessKeyLine(PFILE_KEYBOARD fk, PWSTR str, BOOL IsUnicode)
{
  PWSTR p, pp;
  DWORD msg;
  PFILE_GROUP gp;
  PFILE_KEY kp;
  PWCHAR pklIn, pklKey, pklOut;

  pklIn = new WCHAR[GLOBAL_BUFSIZE];    // I2432 - Allocate buffers each line -- slightly slower but safer than keeping a single buffer
  pklKey = new WCHAR[GLOBAL_BUFSIZE];
  pklOut = new WCHAR[GLOBAL_BUFSIZE];
  if (!pklIn || !pklKey || !pklOut)
    return CERR_CannotAllocateMemory; // forget about the little leak if pklKey or pklOut fail...

  __try
  {

    gp = &fk->dpGroupArray[fk->currentGroup];

    pp = str;

    if (gp->fUsingKeys) {
      if ((msg = GetXString(fk, str, L"+", pklIn, GLOBAL_BUFSIZE - 1, (int)(INT_PTR)(str - pp), &p, TRUE, IsUnicode)) != CERR_None) return msg;

      str = p + 1;
      if ((msg = GetXString(fk, str, L">", pklKey, GLOBAL_BUFSIZE - 1, (int)(INT_PTR)(str - pp), &p, TRUE, IsUnicode)) != CERR_None) return msg;
      if (pklKey[0] == 0) return CERR_ZeroLengthString;
      if (xstrlen(pklKey) > 1) AddWarning(CWARN_KeyBadLength);
    } else {
      if ((msg = GetXString(fk, str, L">", pklIn, GLOBAL_BUFSIZE - 1, (int)(INT_PTR)(str - pp), &p, TRUE, IsUnicode)) != CERR_None) return msg;
      if (pklIn[0] == 0) return CERR_ZeroLengthString;
    }

    str = p + 1;
    if ((msg = GetXString(fk, str, L"c\n", pklOut, GLOBAL_BUFSIZE - 1, (int)(INT_PTR)(str - pp), &p, TRUE, IsUnicode)) != CERR_None) return msg;

    if (pklOut[0] == 0) return CERR_ZeroLengthString;

    CheckContextStatementPositions(pklIn);

    // Test index and context offsets in context
    if ((msg = CheckStatementOffsets(fk, gp, pklIn, pklOut, pklKey)) != CERR_None) return msg;

    // Test that use() statements are not followed by other content
    if ((msg = CheckUseStatementsInOutput(gp, pklOut)) != CERR_None) {
      return msg;   // I4867
    }

    if (gp->fReadOnly) {
      // Ensure no output is made from the rule, and that
      // use() statements meet required readonly semantics
      if ((msg = CheckOutputIsReadonly(fk, pklOut)) != CERR_None) {
        return msg;
      }

      // Inject `context` to start of output if group is readonly
      // to keep the output internally consistent
      if ((msg = InjectContextToReadonlyOutput(pklOut)) != CERR_None) {
        return msg;
      }
    }

    kp = new FILE_KEY[gp->cxKeyArray + 1];
    if (!kp) return CERR_CannotAllocateMemory;
    if (gp->dpKeyArray)
    {
      memcpy(kp, gp->dpKeyArray, gp->cxKeyArray * sizeof(FILE_KEY));
      delete gp->dpKeyArray;
    }

    gp->dpKeyArray = kp;
    kp = &gp->dpKeyArray[gp->cxKeyArray];
    gp->cxKeyArray++;

    kp->dpOutput = new WCHAR[wcslen(pklOut) + 1];
    wcscpy_s(kp->dpOutput, wcslen(pklOut) + 1, pklOut);  // I3481

    kp->dpContext = new WCHAR[wcslen(pklIn) + 1];
    wcscpy_s(kp->dpContext, wcslen(pklIn) + 1, pklIn);  // I3481

    kp->Line = currentLine;
    kp->LineStoreIndex = 0;

    // Finished if we are not using keys

    if (!gp->fUsingKeys)
    {
      kp->Key = 0;
      kp->ShiftFlags = 0;
      return CERR_None;
    }

    // Expand each rule out into multiple rules - much faster processing at the key hit time

    if (*pklKey == 0) return CERR_ZeroLengthString;

    if (*pklKey == UC_SENTINEL)
      switch (*(pklKey + 1))
      {
      case CODE_ANY:
        kp->ShiftFlags = 0;
        if ((msg = ExpandKp(fk, kp, *(pklKey + 2) - 1)) != CERR_None) return msg;
        break;

      case CODE_EXTENDED:
        kp->Key = *(pklKey + 3);
        kp->ShiftFlags = *(pklKey + 2);
        break;

      default:
        return CERR_InvalidCodeInKeyPartOfRule;
      }
    else
    {
      kp->ShiftFlags = 0;
      kp->Key = *pklKey;
    }

  }
  __finally
  {
    delete pklIn;   // I2432 - Allocate buffers each line -- slightly slower but safer than keeping a single buffer
    delete pklKey;
    delete pklOut;
  }

  return CERR_None;
}



DWORD ExpandKp_ReplaceIndex(PFILE_KEYBOARD fk, PFILE_KEY k, DWORD keyIndex, int nAnyIndex)
{
  /* Replace each index(xx,keyIndex) in k->dpOutput with appropriate char as based on nAnyIndex */
  PFILE_STORE s;
  int i;
  PWSTR pIndex, pStore;

  for (pIndex = k->dpOutput; *pIndex; pIndex = incxstr(pIndex))
  {
    if (*pIndex == UC_SENTINEL && *(pIndex + 1) == CODE_INDEX && *(pIndex + 3) == keyIndex)
    {
      s = &fk->dpStoreArray[*(pIndex + 2) - 1];
      for (i = 0, pStore = s->dpString; i < nAnyIndex; i++, pStore = incxstr(pStore));
      PWSTR qStore = incxstr(pStore);

      int w = (int)(INT_PTR)(qStore - pStore);
      if (w > 4)
      {
        *pIndex = UC_SENTINEL;
        *(pIndex + 1) = CODE_BEEP;
        memmove(pIndex + 2, pIndex + 4, wcslen(pIndex + 3) * 2);
      }
      else
      {
        memcpy(pIndex, pStore, w * 2);
        if (w < 4) memmove(pIndex + w, pIndex + 4, wcslen(pIndex + 3) * 2);
      }
    }
  }

  return CERR_None;
}


DWORD ExpandKp(PFILE_KEYBOARD fk, PFILE_KEY kpp, DWORD storeIndex)
{
  PFILE_KEY k;
  PWSTR pn;
  DWORD nchrs, n;
  int keyIndex;

  PFILE_STORE sp = &fk->dpStoreArray[storeIndex];
  PFILE_GROUP gp = &fk->dpGroupArray[fk->currentGroup];

  PWSTR dpContext = kpp->dpContext;
  PWSTR dpOutput = kpp->dpOutput;

  nchrs = xstrlen(sp->dpString);
  pn = sp->dpString;
  keyIndex = xstrlen(dpContext) + 1;

  /*
   Now we change them to plain characters in the output in multiple rules,
   and set the keystroke to the appropriate character in the store.
  */

  k = new FILE_KEY[gp->cxKeyArray + nchrs - 1];
  if (!k) return CERR_CannotAllocateMemory;
  memcpy(k, gp->dpKeyArray, gp->cxKeyArray * sizeof(FILE_KEY));

  kpp = &k[(INT_PTR)(kpp - gp->dpKeyArray)];

  delete gp->dpKeyArray;
  gp->dpKeyArray = k;
  gp->cxKeyArray += nchrs - 1;

  for (k = kpp, n = 0, pn = sp->dpString; *pn; pn = incxstr(pn), k++, n++)
  {
    k->dpContext = new WCHAR[wcslen(dpContext) + 1];
    k->dpOutput = new WCHAR[wcslen(dpOutput) + 1];

    wcscpy_s(k->dpContext, wcslen(dpContext) + 1, dpContext);	// copy the context.  // I3481
    wcscpy_s(k->dpOutput, wcslen(dpOutput) + 1, dpOutput);		// copy the output.

    if (*pn == UC_SENTINEL)
    {
      switch (*(pn + 1))
      {
      case CODE_EXTENDED:
        k->Key = *(pn + 3);		// set the key to store offset.
        k->ShiftFlags = *(pn + 2);
        break;
      default:
        return CERR_CodeInvalidInKeyStore;
      }
    }
    else
    {
      k->Key = *pn;				// set the key to store offset.
      k->ShiftFlags = 0;
    }
    k->Line = kpp->Line;
    k->LineStoreIndex = (WORD)n;
    ExpandKp_ReplaceIndex(fk, k, keyIndex, n);
  }

  delete dpContext;
  delete dpOutput;

  return CERR_None;
}


PWSTR GetDelimitedString(PWSTR *p, PWSTR Delimiters, WORD Flags)
{
  PWSTR q, r;
  WCHAR dOpen, dClose;

  dOpen = *Delimiters; dClose = *(Delimiters + 1);

  q = *p;
  while (iswspace(*q)) q++;            //***QUERY

  if (*q != dOpen) return NULL;

  q++;

  r = wcschr(q, dClose);			        // Find closing delimiter
  if (!r) return NULL;

  if (Flags & GDS_CUTLEAD)
    while (iswspace(*q)) q++;	        // cut off leading spaces

  if (Flags & GDS_CUTFOLL)
    if (!iswspace(*(r - 1))) *r = 0;
    else
    {
      r--;							// Cut off following spaces
      while (iswspace(*r) && r > q) r--;
      r++;
      *r = 0; r = wcschr((r + 1), dClose);
    }
  else *r = 0;

  r++; while (iswspace(*r)) r++;	        // Ignore spaces after the close
  if (*r == 0) r--;					    // Safety for terminating strings.

  *p = r;								    // Update pointer position

  return q;							// Return delimited string
}


enum LinePrefixType { lptNone, lptKeymanAndKeymanWeb, lptKeymanWebOnly, lptKeymanOnly, lptOther };

LinePrefixType GetLinePrefixType(PWSTR *p)
{
  PWSTR s = *p;

  while (iswspace(*s)) s++;

  PWSTR q = s;

  if (*s != '$') return lptNone;

  /* I1569 - fix named constants at the start of the line */
  s++;
  while (__iswcsym(*s)) s++;
  if (*s != ':') return lptNone;

  if (_wcsnicmp(q, L"$keyman:", 8) == 0)
  {
    *p += 8;
    return lptKeymanAndKeymanWeb;
  }
  if (_wcsnicmp(q, L"$keymanweb:", 11) == 0)
  {
    *p += 11;
    return lptKeymanWebOnly;
  }
  if (_wcsnicmp(q, L"$keymanonly:", 12) == 0)
  {
    *p += 12;
    return lptKeymanOnly;
  }

  return lptOther;
}

int LineTokenType(PWSTR *str)
{
  int i;
  size_t l;
  PWSTR p = *str;

  LinePrefixType lpt = GetLinePrefixType(&p);
  if (lpt == lptOther) return T_BLANK;

  /* Test KeymanWeb, Keyman and KeymanOnly prefixes */
  if (CompileTarget == CKF_KEYMAN && lpt == lptKeymanWebOnly) return T_BLANK;
  if (CompileTarget == CKF_KEYMANWEB && lpt == lptKeymanOnly) return T_BLANK;

  while (iswspace(*p)) p++;

  if (wcschr(LineTokens[0], towupper(*p)))
    for (i = 0; i <= T_W_END - T_W_START; i++)
    {
      l = wcslen(LineTokens[i + 1]);
      if (_wcsnicmp(p, LineTokens[i + 1], l) == 0)
      {
        p += l; while (iswspace(*p)) p++; *str = p;
        return i + T_W_START;
      }
    }

  switch (towupper(*p))
  {
  case 'C':
    if (iswspace(*(p + 1))) return T_COMMENT;
    break;
  case 0:
    return T_BLANK;
  default:
    if (wcschr(L"\"aAbBlLpPnN[OoxXdD0123456789\'+UuiI$", *p))   // I4784
    {
      *str = p;
      return T_KEYTOKEY;
    }
  }
  return T_UNKNOWN;
}

const PWSTR DeadKeyChars =
L"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";

BOOL strvalidchrs(PWSTR q, PWSTR chrs)
{
  for (; *q; q++)
    if (!wcschr(chrs, *q)) return FALSE;
  return TRUE;
}

DWORD GetXString(PFILE_KEYBOARD fk, PWSTR str, PWSTR token, PWSTR output, int max, int offset, PWSTR *newp, int isVKey, int isUnicode)
{
  DWORD err;
  PWSTR p = str, q, r;
  int type, mx = 0, n, n1, n2, tokenFound = FALSE, z, sFlag = 0, j;
  DWORD i;
  BOOL finished = FALSE;
  WCHAR c;

  PWCHAR tstr = NULL;
  int tstrMax = 0;

  tstr = new WCHAR[max];    // I2432 - Allocate buffers each line -- slightly slower but safer than keeping a single buffer - GetXString is re-entrant with if()
  tstrMax = max;

  __try
  {
    *tstr = 0;

    *output = 0;

    p = str;
    do
    {
      tokenFound = FALSE;
      while (iswspace(*p) && !wcschr(token, *p)) p++;
      if (!*p) break;

      ErrChr = (int)(INT_PTR)(p - str) + offset + 1;

      /*
      char *tokenTypes[] = {
        "clearcontext", "deadkey", "context", "return", "switch",
        "index", "outs", "beep", "nul", "use", "any", "fix", "dk", "k_", "x", "d", "c",
        "[", "]" };
      */

      switch (towupper(*p))
      {
      case 'X':
      case 'D':  type = 0; break;		// xFF, d130: chars, deadkey(n)
      case '\"': type = 1; break;		// "xxxx": chars
      case '\'': type = 2; break;		// 'xxxx': chars
      case 'A':  type = 3; break;		// any(s)
      case 'B':  type = 4; break;		// beep, baselayout (synonym for if(&baselayout))  // I3430
      case 'I':  type = 5; break;		// index(s,n), if
      case 'O':  type = 6; break;		// outs(s)
      case 'C':  type = 7; break;		// context, comments, clearcontext, call(s)
      case 'N':  type = 8; break;		// nul, notany
      case 'U':  type = 9; break;		// use(g)
      case 'R':  type = 10; break;	// return, reset
      case '[':  type = 11; break;	// start of vkey section
      //case ']':  type = 12; break;	// end of vkey section
      //case 'K':  type = 13; break;	// virtual key name or "key"
      case 'S':  type = 14; break;	// switch, set, save
      case 'F':  type = 15; break;	// fix (synonym for clearcontext)
      case '$':  type = 16; break;	// named code constants
      case 'P':  type = 17; break;  // platform (synonym for if(&platform))  // I3430
      case 'L':  type = 18; break;  // layer (synonym for set(&layer))  // I3437
      case '.':  type = 19; break;  // .. allows us to specify ranges -- either vkeys or characters
      default:
        if (iswdigit(*p)) type = 0;	// octal number
        else type = 99;				// error!
      }
      if (wcschr(token, *p)) tokenFound = TRUE;

      switch (type)
      {
      case 99:
        if (tokenFound) break;
        wsprintf(ErrExtra, "token: %c", (int)*p);
        return CERR_InvalidToken;
      case 0:
        if (_wcsnicmp(p, L"deadkey", z = 7) == 0 ||
          _wcsnicmp(p, L"dk", z = 2) == 0)
        {
          p += z;
          q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
          if (!q || !*q) return CERR_InvalidDeadkey;

          DWORD n = fk->cxDeadKeyArray;

          tstr[mx++] = UC_SENTINEL;
          tstr[mx++] = CODE_DEADKEY;
          if (!strvalidchrs(q, DeadKeyChars)) return CERR_InvalidDeadkey;
          tstr[mx++] = GetDeadKey(fk, q); //atoiW(q); 7-5-01: named deadkeys
          tstr[mx] = 0;
        }
        else
        {
          n = xatoi(&p);
          if (*p != '\0' && !iswspace(*p)) return CERR_InvalidValue;
          if ((err = UTF32ToUTF16(n, &n1, &n2)) != CERR_None) return err;
          tstr[mx++] = n1;
          if (n2 >= 0) tstr[mx++] = n2;
          tstr[mx] = 0;
        }
        continue;
      case 1:
        q = wcschr(p + 1, '\"');
        if (!q) return CERR_UnterminatedString;
        if ((INT_PTR)(q - p) - 1 + mx > max) return CERR_UnterminatedString;
        if (sFlag) return CERR_StringInVirtualKeySection;
        wcsncat_s(tstr, max, p + 1, (INT_PTR)(q - p) - 1);  // I3481
        mx += (int)(INT_PTR)(q - p) - 1;
        tstr[mx] = 0;
        p = q + 1;
        continue;
      case 2:
        q = wcschr(p + 1, '\'');
        if (!q) return CERR_UnterminatedString;
        if ((INT_PTR)(q - p) - 1 + mx > max) return CERR_UnterminatedString;
        if (sFlag) return CERR_StringInVirtualKeySection;
        wcsncat_s(tstr, max, p + 1, (INT_PTR)(q - p) - 1);  // I3481
        mx += (int)(INT_PTR)(q - p) - 1;
        tstr[mx] = 0;
        p = q + 1;
        continue;
      case 3:
        if (_wcsnicmp(p, L"any", 3) != 0) return CERR_InvalidToken;
        if (sFlag) return CERR_AnyInVirtualKeySection;
        p += 3;
        q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
        if (!q || !*q) return CERR_InvalidAny;

        for (i = 0; i < fk->cxStoreArray; i++)
        {
          if (_wcsicmp(q, fk->dpStoreArray[i].szName) == 0) break;
        }
        if (i == fk->cxStoreArray) return CERR_StoreDoesNotExist;

        if (!*fk->dpStoreArray[i].dpString) return CERR_ZeroLengthString;
        CheckStoreUsage(fk, i, TRUE, FALSE, FALSE);

        tstr[mx++] = UC_SENTINEL;
        tstr[mx++] = CODE_ANY;
        tstr[mx++] = (WCHAR)i + 1;	// store to index + 1, avoids End-of-string
        tstr[mx] = 0;
        continue;
      case 4:
        if (_wcsnicmp(p, L"beep", 4) == 0)
        {
          if (sFlag) return CERR_BeepInVirtualKeySection;
          p += 4;
          tstr[mx++] = UC_SENTINEL;
          tstr[mx++] = CODE_BEEP;
          tstr[mx] = 0;
        }
        else if (_wcsnicmp(p, L"baselayout", 10) == 0)  // I3430
        {
          VERIFY_KEYBOARD_VERSION(fk, VERSION_90, CERR_90FeatureOnly_IfSystemStores);
          if (sFlag) return CERR_InvalidInVirtualKeySection;
          p += 10;
          q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
          if (!q || !*q) return CERR_InvalidToken;
          err = process_baselayout(fk, q, tstr, &mx);
          if (err != CERR_None) return err;
        }
        else
          return CERR_InvalidToken;

        continue;
      case 5:
        if (_wcsnicmp(p, L"if", 2) == 0)
        {
          VERIFY_KEYBOARD_VERSION(fk, VERSION_80, CERR_80FeatureOnly);
          if (sFlag) return CERR_InvalidInVirtualKeySection;
          p += 2;
          q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
          if (!q || !*q) return CERR_InvalidIf;

          err = process_if(fk, q, tstr, &mx);
          if (err != CERR_None) return err;
        }
        else
        {
          if (_wcsnicmp(p, L"index", 5) != 0) return CERR_InvalidToken;
          if (sFlag) return CERR_IndexInVirtualKeySection;
          p += 5;
          q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);

          if (!q || !*q) return CERR_InvalidIndex;

          {
            wchar_t *context = NULL;
            r = wcstok_s(q, L" ,", &context);  // I3481
            if (!r) return CERR_InvalidIndex;

            for (i = 0; i < fk->cxStoreArray; i++)
            {
              if (_wcsicmp(r, fk->dpStoreArray[i].szName) == 0) break;
            }
            if (i == fk->cxStoreArray) return CERR_StoreDoesNotExist;

            CheckStoreUsage(fk, i, TRUE, FALSE, FALSE);

            r = wcstok_s(NULL, L" ,", &context);  // I3481
            if (!r) return CERR_InvalidIndex;
          }
          tstr[mx++] = UC_SENTINEL;
          tstr[mx++] = CODE_INDEX;
          tstr[mx++] = (WCHAR)i + 1;	    // avoid EOS for stores
          tstr[mx++] = atoiW(r);	// character offset of original any.

          tstr[mx] = 0;
        }
        continue;
      case 6:
        if (_wcsnicmp(p, L"outs", 4) != 0) return CERR_InvalidToken;
        if (sFlag) return CERR_OutsInVirtualKeySection;
        p += 4;
        q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
        if (!q || !*q) return CERR_InvalidOuts;

        for (i = 0; i < fk->cxStoreArray; i++)
        {
          if (_wcsicmp(q, fk->dpStoreArray[i].szName) == 0) break;
        }
        if (i == fk->cxStoreArray) return CERR_StoreDoesNotExist;

        CheckStoreUsage(fk, i, TRUE, FALSE, FALSE);

        for (q = fk->dpStoreArray[i].dpString; *q; q++)
        {
          tstr[mx++] = *q;
          if (mx >= max - 1) return CERR_BufferOverflow;
        }
        tstr[mx] = 0;
        continue;
      case 7:
        if (iswspace(*(p + 1))) break;		// is a comment -- pre-stripped - so why this test?
        if (_wcsnicmp(p, L"context", 7) == 0)
        {
          if (sFlag) return CERR_ContextInVirtualKeySection;
          p += 7;

          q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
          if (q && *q)
          {
            VERIFY_KEYBOARD_VERSION(fk, VERSION_60, CERR_60FeatureOnly_Contextn);
            int n1;
            n1 = atoiW(q);
            if (n1 < 1 || n1 >= 0xF000) return CERR_InvalidToken;
            tstr[mx++] = UC_SENTINEL;
            tstr[mx++] = CODE_CONTEXTEX;
            tstr[mx++] = n1;
            tstr[mx] = 0;
          }
          else
          {
            tstr[mx++] = UC_SENTINEL;
            tstr[mx++] = CODE_CONTEXT;
            tstr[mx] = 0;
          }
        }
        else if (_wcsnicmp(p, L"clearcontext", 12) == 0)
        {
          p += 12;
          tstr[mx++] = UC_SENTINEL;
          tstr[mx++] = CODE_CLEARCONTEXT;
          tstr[mx] = 0;
        }
        else if (_wcsnicmp(p, L"call", 4) == 0)
        {
          VERIFY_KEYBOARD_VERSION(fk, VERSION_501, CERR_501FeatureOnly_Call);
          if (sFlag) return CERR_CallInVirtualKeySection;
          p += 4;
          q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
          if (!q || !*q) return CERR_InvalidCall;

          for (i = 0; i < fk->cxStoreArray; i++)
          {
            if (_wcsicmp(q, fk->dpStoreArray[i].szName) == 0) break;
          }

          if (!IsValidCallStore(&fk->dpStoreArray[i])) return CERR_InvalidCall;
          CheckStoreUsage(fk, i, FALSE, FALSE, TRUE);

          if (i == fk->cxStoreArray) return CERR_StoreDoesNotExist;
          tstr[mx++] = UC_SENTINEL;
          tstr[mx++] = CODE_CALL;
          tstr[mx++] = (WCHAR)i + 1;
          tstr[mx] = 0;

          fk->dpStoreArray[i].dwSystemID = TSS_CALLDEFINITION;
        }
        else
          return CERR_InvalidToken;
        continue;
      case 8:
        if (_wcsnicmp(p, L"notany", 6) == 0)
        {
          VERIFY_KEYBOARD_VERSION(fk, VERSION_70, CERR_70FeatureOnly)
            if (sFlag) return CERR_AnyInVirtualKeySection;
          p += 6;
          q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
          if (!q || !*q) return CERR_InvalidAny;

          for (i = 0; i < fk->cxStoreArray; i++)
          {
            if (_wcsicmp(q, fk->dpStoreArray[i].szName) == 0) break;
          }
          if (i == fk->cxStoreArray) return CERR_StoreDoesNotExist;
          CheckStoreUsage(fk, i, TRUE, FALSE, FALSE);
          tstr[mx++] = UC_SENTINEL;
          tstr[mx++] = CODE_NOTANY;
          tstr[mx++] = (WCHAR)i + 1;	// store to index + 1, avoids End-of-string
          tstr[mx] = 0;
          continue;
        }
        if (_wcsnicmp(p, L"nul", 3) != 0) return CERR_InvalidToken;

        p += 3;
        tstr[mx++] = UC_SENTINEL;
        tstr[mx++] = CODE_NUL;
        tstr[mx] = 0;
        continue;
      case 9:
        if (_wcsnicmp(p, L"use", 3) != 0)
        {
          if (*(p + 1) == '+')
          {
            n = xatoi(&p);
            if (*p != '\0' && !iswspace(*p)) return CERR_InvalidValue;
            if ((err = UTF32ToUTF16(n, &n1, &n2)) != CERR_None) return err;
            tstr[mx++] = n1;
            if (n2 >= 0) tstr[mx++] = n2;
            tstr[mx] = 0;
            if (!isUnicode) AddWarning(CWARN_UnicodeInANSIGroup);
            continue;
          }
          return CERR_InvalidToken;
        }
        p += 3;

        q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
        if (!q || !*q) return CERR_InvalidUse;
        tstr[mx++] = UC_SENTINEL;
        tstr[mx++] = CODE_USE;
        tstr[mx] = GetGroupNum(fk, q);
        if (tstr[mx] == 0) return CERR_GroupDoesNotExist;
        tstr[++mx] = 0;
        continue;
      case 10:
        if (_wcsnicmp(p, L"reset", 5) == 0)
        {
          VERIFY_KEYBOARD_VERSION(fk, VERSION_80, CERR_80FeatureOnly);
          if (sFlag) return CERR_InvalidInVirtualKeySection;
          p += 5;
          q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
          if (!q || !*q) return CERR_InvalidReset;

          err = process_reset(fk, q, tstr, &mx);
          if (err != CERR_None) return err;
        }
        else
        {
          if (_wcsnicmp(p, L"return", 6) != 0) return CERR_InvalidToken;

          p += 6;
          tstr[mx++] = UC_SENTINEL;
          tstr[mx++] = CODE_RETURN;
          tstr[mx] = 0;
          wcsncpy_s(output, max, tstr, max);  // I3481
          output[max - 1] = 0;
          return 0;
        }
        continue;
      case 11:
        p++; sFlag = ISVIRTUALKEY /* 0 */; finished = FALSE;

        //printf("--EXTENDEDSTRING--\n");

        do
        {
          while (iswspace(*p)) p++;

          switch (towupper(*p))
          {
          case 'N':
            if (_wcsnicmp(p, L"NCAPS", 5) == 0)
              sFlag |= NOTCAPITALFLAG, p += 5;
            else finished = TRUE;
            break;
          case 'L':
            if (_wcsnicmp(p, L"LALT", 4) == 0)
              sFlag |= LALTFLAG, p += 4;
            else if (_wcsnicmp(p, L"LCTRL", 5) == 0)
              sFlag |= LCTRLFLAG, p += 5;
            else finished = TRUE;
            break;
          case 'R':
            if (_wcsnicmp(p, L"RALT", 4) == 0)
              sFlag |= RALTFLAG, p += 4;
            else if (_wcsnicmp(p, L"RCTRL", 5) == 0)
              sFlag |= RCTRLFLAG, p += 5;
            else finished = TRUE;
            break;
          case 'A':
            if (_wcsnicmp(p, L"ALT", 3) == 0)
              sFlag |= K_ALTFLAG, p += 3;
            else finished = TRUE;
            break;
          case 'C':
            if (_wcsnicmp(p, L"CTRL", 4) == 0)
              sFlag |= K_CTRLFLAG, p += 4;
            else if (_wcsnicmp(p, L"CAPS", 4) == 0)
              sFlag |= CAPITALFLAG, p += 4;
            else finished = TRUE;
            break;
          case 'S':
            if (_wcsnicmp(p, L"SHIFT", 5) == 0)
              sFlag |= K_SHIFTFLAG, p += 5;
            else finished = TRUE;
            break;
          default:
            finished = TRUE;
            break;
          }
        } while (!finished);

        if ((sFlag & (LCTRLFLAG | LALTFLAG)) && (sFlag & (RCTRLFLAG | RALTFLAG))) {
          AddWarning(CWARN_MixingLeftAndRightModifiers);
        }

        // If we use chiral modifiers, or we use state keys, and we target web in the keyboard, and we don't manually specify a keyboard version, bump the minimum
        // version to 10.0. This makes an assumption that if we are using these features in a keyboard and it has no version specified, that we want to use the features
        // in the web target platform, even if there are platform() rules excluding this possibility. In that (rare) situation, the keyboard developer should simply specify
        // the &version to be 9.0 or whatever to avoid this behaviour.
        if (sFlag & (LCTRLFLAG | LALTFLAG | RCTRLFLAG | RALTFLAG | CAPITALFLAG | NOTCAPITALFLAG | NUMLOCKFLAG | NOTNUMLOCKFLAG | SCROLLFLAG | NOTSCROLLFLAG) &&
          CompileTarget == CKF_KEYMANWEB &&
          fk->dwFlags & KF_AUTOMATICVERSION) {
          VERIFY_KEYBOARD_VERSION(fk, VERSION_100, 0);
        }
        //printf("sFlag: %x\n", sFlag);

        tstr[mx++] = UC_SENTINEL;
        tstr[mx++] = CODE_EXTENDED;
        tstr[mx++] = sFlag;

        while (iswspace(*p)) p++;

        q = p;

        if (*q == ']')
        {
          return CERR_InvalidToken; // I3137 - key portion of VK is missing e.g. "[CTRL ALT]", this generates invalid kmx file that can crash Keyman or compiler later on   // I3511
        }

        while (*q != ']')
        {
          if (*q == '\'' || *q == '"')
          {
            VERIFY_KEYBOARD_VERSION(fk, VERSION_60, CERR_60FeatureOnly_VirtualCharKey);
            if (!FMnemonicLayout) AddWarning(CWARN_VirtualCharKeyWithPositionalLayout);
            WCHAR chQuote = *q;
            q++; if (*q == chQuote || *q == '\n' || *q == 0) return CERR_InvalidToken;
            tstr[mx - 1] |= VIRTUALCHARKEY;
            tstr[mx++] = *q;
            q++; if (*q != chQuote) return CERR_InvalidToken;
            q++;
            while (iswspace(*q)) q++;
            if (*q != ']') return CERR_InvalidToken;
            break; /* out of while loop */
          }

          for (j = 0; !iswspace(*q) && *q != ']' && *q != 0; q++, j++);

          if (*q == 0) return CERR_InvalidToken;

          WCHAR vkname[SZMAX_VKDICTIONARYNAME];  // I3438

          if (j >= SZMAX_VKDICTIONARYNAME) return CERR_InvalidToken;

          wcsncpy_s(vkname, _countof(vkname), p, j);  // I3481
          vkname[j] = 0;

          if (_wcsicmp(vkname, L"K_NPENTER") == 0)
            i = 5;  // I649 - K_NPENTER hack
          else
          {
            for (i = 0; i <= VK__MAX; i++)
            {
              if (_wcsicmp(vkname, VKeyNames[i]) == 0 || _wcsicmp(vkname, VKeyISO9995Names[i]) == 0)
                break;
            }
          }

          if (i == VK__MAX + 1)
          {
            VERIFY_KEYBOARD_VERSION(fk, VERSION_90, CERR_InvalidToken);

            i = GetVKCode(fk, vkname);  // I3438
            if (i == 0)
              return CERR_InvalidToken;
          }

          p = q;

          tstr[mx++] = (int)i;

          if (FMnemonicLayout && (i <= VK__MAX) && VKeyMayBeVCKey[i]) AddWarning(CWARN_VirtualKeyWithMnemonicLayout);  // I3438

          while (iswspace(*q)) q++;
        }
        tstr[mx++] = UC_SENTINEL_EXTENDEDEND;
        tstr[mx] = 0;
        //printf("--EXTENDEDEND--\n");

        p = q + 1;

        sFlag = 0;

        continue;
      case 14:
        if (_wcsnicmp(p, L"set", 3) == 0)
        {
          VERIFY_KEYBOARD_VERSION(fk, VERSION_80, CERR_80FeatureOnly);
          p += 3;
          q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
          if (!q || !*q) return CERR_InvalidSet;

          err = process_set(fk, q, tstr, &mx);
          if (err != CERR_None) return err;
        }
        else if (_wcsnicmp(p, L"save", 4) == 0)
        {
          VERIFY_KEYBOARD_VERSION(fk, VERSION_80, CERR_80FeatureOnly);
          p += 4;
          q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
          if (!q || !*q) return CERR_InvalidSave;

          err = process_save(fk, q, tstr, &mx);
          if (err != CERR_None) return err;
        }
        else
        {
          if (_wcsnicmp(p, L"switch", 6) != 0) return CERR_InvalidToken;
          p += 6;
          q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
          if (!q || !*q) return CERR_InvalidSwitch;
          tstr[mx++] = UC_SENTINEL;
          tstr[mx++] = CODE_SWITCH;
          tstr[mx++] = atoiW(q);
          tstr[mx] = 0;
        }
        continue;
      case 15:
        if (_wcsnicmp(p, L"fix", 3) == 0)
        {
          p += 3;
          tstr[mx++] = UC_SENTINEL;
          tstr[mx++] = CODE_CLEARCONTEXT;
          tstr[mx] = 0;
        }
        else
          return CERR_InvalidToken;
        continue;
      case 16:
        VERIFY_KEYBOARD_VERSION(fk, VERSION_60, CERR_60FeatureOnly_NamedCodes);
        q = p + 1;
        while (*q && !iswspace(*q)) q++;
        c = *q; *q = 0;
        n = CodeConstants->GetCode(p + 1, &i);
        *q = c;
        if (n == 0) return CERR_InvalidNamedCode;
        if (i < 0xFFFFFFFFL) CheckStoreUsage(fk, i, TRUE, FALSE, FALSE);   // I2993
        if (n > 0xFFFF)
        {
          tstr[mx++] = Uni_UTF32ToSurrogate1(n);
          tstr[mx++] = Uni_UTF32ToSurrogate2(n);
        }
        else
          tstr[mx++] = n;
        tstr[mx] = 0;
        p = q;
        continue;
      case 17:
        if (_wcsnicmp(p, L"platform", 8) != 0) return CERR_InvalidToken;  // I3430
        VERIFY_KEYBOARD_VERSION(fk, VERSION_90, CERR_90FeatureOnly_IfSystemStores);
        if (sFlag) return CERR_InvalidInVirtualKeySection;
        p += 8;
        q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
        if (!q || !*q) return CERR_InvalidToken;
        err = process_platform(fk, q, tstr, &mx);
        if (err != CERR_None) return err;
        continue;
      case 18:  // I3437
        if (_wcsnicmp(p, L"layer", 5) != 0) return CERR_InvalidToken;
        VERIFY_KEYBOARD_VERSION(fk, VERSION_90, CERR_90FeatureOnly_SetSystemStores);
        if (sFlag) return CERR_InvalidInVirtualKeySection;
        p += 5;
        q = GetDelimitedString(&p, L"()", GDS_CUTLEAD | GDS_CUTFOLL);
        if (!q || !*q) return CERR_InvalidToken;
        err = process_set_synonym(TSS_LAYER, fk, q, tstr, &mx);
        if (err != CERR_None) return err;
        continue;
      case 19:  // #2241
        if (*(p + 1) != '.') return CERR_InvalidToken;
        if (sFlag) return CERR_InvalidInVirtualKeySection;
        p += 2;
        err = process_expansion(fk, p, tstr, &mx, max);
        if (err != CERR_None) return err;
        continue;
      default:
        return CERR_InvalidToken;
      }
      if (tokenFound)
      {
        *newp = p;
        wcsncpy_s(output, max, tstr, max);  // I3481
        output[max - 1] = 0;
        ErrChr = 0;
        return CERR_None;
      }
      if (mx >= max) return CERR_BufferOverflow;
    } while (*p);

    if (!*token)
    {
      *newp = p;
      wcsncpy_s(output, max, tstr, max);  // I3481
      output[max - 1] = 0;
      ErrChr = 0;
      return CERR_None;
    }

  }
  __finally
  {
    delete[] tstr;   // I2432 - Allocate buffers each line -- slightly slower but safer than keeping a single buffer - GetXString is re-entrant with if()
  }

  return CERR_NoTokensFound;
}

DWORD process_if_synonym(DWORD dwSystemID, PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx);  // I3430

DWORD process_baselayout(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx)  // I3430
{
  /* baselayout(<XString+outs>) */
  return process_if_synonym(TSS_BASELAYOUT, fk, q, tstr, mx);
}

DWORD process_platform(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx)  // I3430
{
  /* platform(<XString+outs>) */
  return process_if_synonym(TSS_PLATFORM, fk, q, tstr, mx);
}

DWORD process_if_synonym(DWORD dwSystemID, PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx)  // I3430
{
  PWCHAR temp = new WCHAR[GLOBAL_BUFSIZE];

  DWORD msg;

  PWSTR r;

  if ((msg = GetXString(fk, q, L"", temp, GLOBAL_BUFSIZE - 1, 0, &r, FALSE, TRUE)) != CERR_None)
  {
    delete temp;
    return msg;
  }

  DWORD dwStoreID;

  if ((msg = AddStore(fk, TSS_COMPARISON, temp, &dwStoreID)) != CERR_None)
  {
    delete temp;
    return msg;
  }

  tstr[(*mx)++] = UC_SENTINEL;
  tstr[(*mx)++] = (WCHAR)CODE_IFSYSTEMSTORE;
  tstr[(*mx)++] = (WCHAR)(dwSystemID + 1);   // I4785
  tstr[(*mx)++] = 2;
  tstr[(*mx)++] = (WCHAR)(dwStoreID + 1);
  tstr[(*mx)] = 0;

  delete[] temp;

  return CERR_None;
}

DWORD process_if(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx)  // I3431
{
  /* if(<store> <'='|'!='> <XString+outs>) */
  DWORD i, code, not = FALSE;
  LPWSTR r = q, s = q;
  while (*s && *s != L' ' && *s != L'!' && *s != L'=') s++;
  r = s;
  while (*s == L' ') s++;
  if (*s == L'!')
  {
    s++;
    not = TRUE;
  }

  if (*s != '=') return CERR_InvalidIf;
  s++;
  while (*s == ' ') s++;
  *r = 0;
  r = q;

  if (r[0] == '&')
  {
    VERIFY_KEYBOARD_VERSION(fk, VERSION_90, CERR_90FeatureOnly_IfSystemStores);
    for (i = 0; StoreTokens[i]; i++)
    {
      if (_wcsicmp(r, StoreTokens[i]) == 0) break;
    }
    if (!StoreTokens[i]) return CERR_IfSystemStore_NotFound;
    code = CODE_IFSYSTEMSTORE;
  }
  else
  {
    code = CODE_IFOPT;

    for (i = 0; i < fk->cxStoreArray; i++)
    {
      if (_wcsicmp(r, fk->dpStoreArray[i].szName) == 0) break;
    }
    if (i == fk->cxStoreArray) return CERR_StoreDoesNotExist;
    CheckStoreUsage(fk, i, FALSE, TRUE, FALSE);
  }

  PWCHAR temp = new WCHAR[GLOBAL_BUFSIZE];

  DWORD msg;

  if ((msg = GetXString(fk, s, L"", temp, GLOBAL_BUFSIZE - 1, 0, &r, FALSE, TRUE)) != CERR_None)
  {
    delete[] temp;
    return msg;
  }

  DWORD dwStoreID;

  if ((msg = AddStore(fk, TSS_COMPARISON, temp, &dwStoreID)) != CERR_None)
  {
    delete[] temp;
    return msg;
  }

  tstr[(*mx)++] = UC_SENTINEL;
  tstr[(*mx)++] = (WCHAR)code;
  tstr[(*mx)++] = (WCHAR)(i + 1);
  tstr[(*mx)++] = not ? 1 : 2;
  tstr[(*mx)++] = (WCHAR)(dwStoreID + 1);
  tstr[(*mx)] = 0;

  return CERR_None;
}

DWORD process_reset(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx)
{
  /* reset(<store>) */
  DWORD i;
  for (i = 0; i < fk->cxStoreArray; i++)
  {
    if (_wcsicmp(q, fk->dpStoreArray[i].szName) == 0) break;
  }
  if (i == fk->cxStoreArray) return CERR_StoreDoesNotExist;
  CheckStoreUsage(fk, i, FALSE, TRUE, FALSE);

  tstr[(*mx)++] = UC_SENTINEL;
  tstr[(*mx)++] = CODE_RESETOPT;
  tstr[(*mx)++] = (WCHAR)(i + 1);
  tstr[(*mx)] = 0;

  return CERR_None;
}

DWORD process_expansion(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx, int max) {
  BOOL isVKey = FALSE;

  WORD BaseKey=0, BaseShiftFlags=0;
  DWORD BaseChar=0;

  if (*mx == 0) {
    return CERR_ExpansionMustFollowCharacterOrVKey;
  }
  LPWSTR p = &tstr[*mx];
  p = decxstr(p, tstr);
  if (*p == UC_SENTINEL) {
    if (*(p + 1) != CODE_EXTENDED) {
      return CERR_ExpansionMustFollowCharacterOrVKey;
    }
    isVKey = TRUE;
    BaseKey = *(p + 3);
    BaseShiftFlags = *(p + 2);
  }
  else {
    BaseChar = Uni_UTF16ToUTF32(p);
  }

  // Look ahead at next element
  WCHAR temp[GLOBAL_BUFSIZE];
  PWCHAR r = NULL;

  DWORD msg;

  if ((msg = GetXString(fk, q, L"", temp, _countof(temp) - 1, 0, &r, FALSE, TRUE)) != CERR_None)
  {
    return msg;
  }

  WORD HighKey, HighShiftFlags;
  DWORD HighChar;

  switch(temp[0]) {
  case 0:
    return isVKey ? CERR_VKeyExpansionMustBeFollowedByVKey : CERR_CharacterExpansionMustBeFollowedByCharacter;
  case UC_SENTINEL:
    // Verify that range is valid virtual key range
    if(!isVKey) {
      return CERR_CharacterExpansionMustBeFollowedByCharacter;
    }
    if (temp[1] != CODE_EXTENDED) {
      return CERR_VKeyExpansionMustBeFollowedByVKey;
    }
    HighKey = temp[3], HighShiftFlags = temp[2];
    if (HighShiftFlags != BaseShiftFlags) {
      return CERR_VKeyExpansionMustUseConsistentShift;
    }
    if (HighKey <= BaseKey) {
      return CERR_ExpansionMustBePositive;
    }
    // Verify space in buffer
    if (*mx + (HighKey - BaseKey) * 5 + 1 >= max) return CERR_BufferOverflow;
    // Inject an expansion.
    for (BaseKey++; BaseKey < HighKey; BaseKey++) {
      // < HighKey because caller will add HighKey to output
      tstr[(*mx)++] = UC_SENTINEL;
      tstr[(*mx)++] = CODE_EXTENDED;
      tstr[(*mx)++] = BaseShiftFlags;
      tstr[(*mx)++] = BaseKey;
      tstr[(*mx)++] = UC_SENTINEL_EXTENDEDEND;
    }
    tstr[*mx] = 0;
    break;
  default:
    // Verify that range is a valid character range
    if (isVKey) {
      return CERR_VKeyExpansionMustBeFollowedByVKey;
    }

    HighChar = Uni_UTF16ToUTF32(temp);
    if (HighChar <= BaseChar) {
      return CERR_ExpansionMustBePositive;
    }
    // Inject an expansion.
    for (BaseChar++; BaseChar < HighChar; BaseChar++) {
      // < HighChar because caller will add HighChar to output
      if (Uni_IsSMP(BaseChar)) {
        // We'll test on each char to avoid complex calculations crossing SMP boundary
        if (*mx + 3 >= max) return CERR_BufferOverflow;
        tstr[(*mx)++] = (WCHAR) Uni_UTF32ToSurrogate1(BaseChar);
        tstr[(*mx)++] = (WCHAR) Uni_UTF32ToSurrogate2(BaseChar);
      }
      else {
        if (*mx + 2 >= max) return CERR_BufferOverflow;
        tstr[(*mx)++] = (WCHAR) BaseChar;
      }
    }
    tstr[*mx] = 0;
  }

  return CERR_None;
}

DWORD process_set_synonym(DWORD dwSystemID, PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx)  // I3437
{
  /* set(<store> <'='> <XString+outs>), layer */
  DWORD code = CODE_SETSYSTEMSTORE;
  PWCHAR temp = new WCHAR[GLOBAL_BUFSIZE], r;
  DWORD msg;

  if ((msg = GetXString(fk, q, L"", temp, GLOBAL_BUFSIZE - 1, 0, &r, FALSE, TRUE)) != CERR_None)
  {
    delete[] temp;
    return msg;
  }

  DWORD dwStoreID;

  msg = AddStore(fk, TSS_COMPARISON, temp, &dwStoreID);
  delete[] temp;
  if (msg != CERR_None) return msg;

  tstr[(*mx)++] = UC_SENTINEL;
  tstr[(*mx)++] = (WCHAR)CODE_SETSYSTEMSTORE;
  tstr[(*mx)++] = (WCHAR)(dwSystemID + 1);
  tstr[(*mx)++] = (WCHAR)(dwStoreID + 1);
  tstr[(*mx)] = 0;
  return CERR_None;
}

DWORD process_set(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx)
{
  /* set(<store> <'='> <XString+outs> */
  LPWSTR r = q, s = q;  // I3440
  while (*s && *s != L' ' && *s != L'=') s++;
  r = s;
  while (*s == L' ') s++;
  if (*s != '=') return CERR_InvalidSet;
  s++;
  while (*s == ' ') s++;
  *r = 0;
  r = q;

  DWORD i, code;

  if (r[0] == '&')
  {
    VERIFY_KEYBOARD_VERSION(fk, VERSION_90, CERR_90FeatureOnly_SetSystemStores);  // I3437
    for (i = 0; StoreTokens[i]; i++)
    {
      if (_wcsicmp(r, StoreTokens[i]) == 0) break;
    }
    if (!StoreTokens[i]) return CERR_SetSystemStore_NotFound;
    code = CODE_SETSYSTEMSTORE;
  }
  else
  {
    wchar_t *context = NULL;
    LPWSTR r = wcstok_s(q, L" =", &context);  // I3481

    for (i = 0; i < fk->cxStoreArray; i++)
    {
      if (_wcsicmp(r, fk->dpStoreArray[i].szName) == 0) break;
    }
    if (i == fk->cxStoreArray) return CERR_StoreDoesNotExist;
    CheckStoreUsage(fk, i, FALSE, TRUE, FALSE);
    code = CODE_SETOPT;
  }

  PWCHAR temp = new WCHAR[GLOBAL_BUFSIZE];

  DWORD msg;

  //r = wcstok(NULL, L" =");

  if ((msg = GetXString(fk, s, L"", temp, GLOBAL_BUFSIZE - 1, 0, &r, FALSE, TRUE)) != CERR_None)
  {
    delete[] temp;
    return msg;
  }

  DWORD dwStoreID;

  msg = AddStore(fk, TSS_COMPARISON, temp, &dwStoreID);
  delete[] temp;
  if (msg != CERR_None) return msg;

  tstr[(*mx)++] = UC_SENTINEL;
  tstr[(*mx)++] = (WCHAR)code;
  tstr[(*mx)++] = (WCHAR)(i + 1);
  tstr[(*mx)++] = (WCHAR)(dwStoreID + 1);
  tstr[(*mx)] = 0;
  return CERR_None;
}

DWORD process_save(PFILE_KEYBOARD fk, LPWSTR q, LPWSTR tstr, int *mx)
{
  /* save(<store>) */
  DWORD i;
  for (i = 0; i < fk->cxStoreArray; i++)
  {
    if (_wcsicmp(q, fk->dpStoreArray[i].szName) == 0) break;
  }
  if (i == fk->cxStoreArray) return CERR_StoreDoesNotExist;
  CheckStoreUsage(fk, i, FALSE, TRUE, FALSE);

  tstr[(*mx)++] = UC_SENTINEL;
  tstr[(*mx)++] = CODE_SAVEOPT;
  tstr[(*mx)++] = (WCHAR)(i + 1);
  tstr[(*mx)] = 0;
  return CERR_None;
}

int xatoi(PWSTR *p)
{
  PWSTR endptr;
  int n;

  switch (towupper(**p))
  {
  case 'U':
    (*p)++;
    if (**p != '+') return 0;
    (*p)++;
    n = (int)wcstol(*p, &endptr, 16);
    *p = endptr;
    break;
  case 'X':
    (*p)++;
    n = (int)wcstol(*p, &endptr, 16);
    *p = endptr;
    break;
  case 'D':
    (*p)++;
    n = (int)wcstol(*p, &endptr, 10);
    *p = endptr;
    break;
  default:
    n = (int)wcstol(*p, &endptr, 8);
    *p = endptr;
    break;
  }
  return n;
}

int GetGroupNum(PFILE_KEYBOARD fk, PWSTR p)
{
  PFILE_GROUP gp;
  DWORD i;

  for (i = 0, gp = fk->dpGroupArray; i < fk->cxGroupArray; gp++, i++)
  {
    if (_wcsicmp(gp->szName, p) == 0) return i + 1;
  }
  return 0;
}


DWORD ProcessEthnologueStore(PWSTR p) // I2646
{
  DWORD res = CERR_None;
  PWSTR q = NULL;
  while (*p)
  {
    while (wcschr(L" ,;", *p))
    {
      if (*p != ' ') res = CWARN_PunctuationInEthnologueCode;
      p++;
    }
    if (q == p) return CERR_InvalidEthnologueCode;
    if (*p)
    {
      for (int i = 0; i < 3; i++)
      {
        if (!iswalpha(*p)) return CERR_InvalidEthnologueCode;
        p++;
      }
    }
    q = p;
  }
  return res;
}

#define K_HOTKEYSHIFTFLAGS (K_SHIFTFLAG | K_CTRLFLAG | K_ALTFLAG | ISVIRTUALKEY)

DWORD ProcessHotKey(PWSTR p, DWORD *hk)
{
  PWSTR q, r;
  DWORD sFlag;
  int j, i;

  *hk = 0;

  if (*p == UC_SENTINEL && *(p + 1) == CODE_EXTENDED) {
    WORD Key = *(p + 3);
    WORD ShiftFlags = *(p + 2);

    // Convert virtual key to hotkey (different bitflags)

    if (ShiftFlags & ~K_HOTKEYSHIFTFLAGS) {
      AddWarning(CWARN_HotkeyHasInvalidModifier);
    }

    if (ShiftFlags & K_SHIFTFLAG) *hk |= HK_SHIFT;
    if (ShiftFlags & K_CTRLFLAG) *hk |= HK_CTRL;
    if (ShiftFlags & K_ALTFLAG) *hk |= HK_ALT;

    *hk |= Key;

    return CERR_None;
  }

  q = wcschr(p, '[');
  if (q)
  {
    q++;
    sFlag = 0;

    do
    {
      while (iswspace(*q)) q++;

      if (_wcsnicmp(q, L"ALT", 3) == 0) sFlag |= HK_ALT, q += 3;
      else if (_wcsnicmp(q, L"CTRL", 4) == 0) sFlag |= HK_CTRL, q += 4;
      else if (_wcsnicmp(q, L"SHIFT", 5) == 0) sFlag |= HK_SHIFT, q += 5;
      else if (towupper(*q) != 'K') return CERR_InvalidToken;
    } while (towupper(*q) != 'K');

    r = wcschr(q, ']');
    if (r)
    {
      r--;
      while (iswspace(*r) && r > q) r--;
      r++;
    }
    else return CERR_NoTokensFound;

    j = (int)(INT_PTR)(r - q);

    for (i = 0; i <= VK__MAX; i++)  // I3438
      if (j == (int)wcslen(VKeyNames[i]) && _wcsnicmp(q, VKeyNames[i], j) == 0) break;

    if (i == VK__MAX + 1) return CERR_InvalidToken;  // I3438

    *hk = i | sFlag;

    return CERR_None;
  }

  q = GetDelimitedString(&p, L"\"\"", GDS_CUTLEAD | GDS_CUTFOLL);
  if (q)
  {
    if (wcschr(q, '^')) *hk |= HK_CTRL;
    if (wcschr(q, '+')) *hk |= HK_SHIFT;
    if (wcschr(q, '%')) *hk |= HK_ALT;
    q = wcschr(q, 0) - 1;
    *hk |= *q;
    return CERR_None;
  }

  return CERR_CodeInvalidInThisSection;
}


void SetChecksum(LPBYTE buf, LPDWORD CheckSum, DWORD sz)
{
  BuildCRCTable();
  *CheckSum = CalculateBufferCRC(buf, sz);
}


BOOL CheckStoreUsage(PFILE_KEYBOARD fk, int storeIndex, BOOL fIsStore, BOOL fIsOption, BOOL fIsCall)
{
  PFILE_STORE sp = &fk->dpStoreArray[storeIndex];
  if (fIsStore && !sp->fIsStore)
  {
    if (sp->fIsDebug || sp->fIsOption || sp->fIsReserved || sp->fIsCall)
      AddWarning(CWARN_StoreAlreadyUsedAsOptionOrCall);
    sp->fIsStore = TRUE;
  }
  else if (fIsOption && !sp->fIsOption)
  {
    if (sp->fIsDebug || sp->fIsStore || sp->fIsReserved || sp->fIsCall)
      AddWarning(CWARN_StoreAlreadyUsedAsStoreOrCall);
    sp->fIsOption = TRUE;
  }
  else if (fIsCall && !sp->fIsCall)
  {
    if (sp->fIsDebug || sp->fIsStore || sp->fIsReserved || sp->fIsOption)
      AddWarning(CWARN_StoreAlreadyUsedAsStoreOrOption);
    sp->fIsCall = TRUE;
  }

  return TRUE;
}

DWORD WriteCompiledKeyboard(PFILE_KEYBOARD fk, HANDLE hOutfile)
{
  PFILE_GROUP fgp;
  PFILE_STORE fsp;
  PFILE_KEY fkp;

  PCOMP_KEYBOARD ck;
  PCOMP_GROUP gp;
  PCOMP_STORE sp;
  PCOMP_KEY kp;
  PBYTE buf;
  size_t offset;
  size_t size;
  DWORD i, j;

  // Calculate how much memory to allocate

  size = sizeof(COMP_KEYBOARD) +
    fk->cxGroupArray * sizeof(COMP_GROUP) +
    fk->cxStoreArray * sizeof(COMP_STORE) +
    /*wcslen(fk->szName)*2 + 2 +
    wcslen(fk->szCopyright)*2 + 2 +
    wcslen(fk->szLanguageName)*2 + 2 +
    wcslen(fk->szMessage)*2 + 2 +*/
    fk->dwBitmapSize;

  for (i = 0, fgp = fk->dpGroupArray; i < fk->cxGroupArray; i++, fgp++)
  {
    if (FSaveDebug) size += wcslen(fgp->szName) * 2 + 2;
    size += fgp->cxKeyArray * sizeof(COMP_KEY);
    for (j = 0, fkp = fgp->dpKeyArray; j < fgp->cxKeyArray; j++, fkp++)
    {
      size += wcslen(fkp->dpOutput) * 2 + 2;
      size += wcslen(fkp->dpContext) * 2 + 2;
    }

    if (fgp->dpMatch) size += wcslen(fgp->dpMatch) * 2 + 2;
    if (fgp->dpNoMatch) size += wcslen(fgp->dpNoMatch) * 2 + 2;
  }

  for (i = 0; i < fk->cxStoreArray; i++)
  {
    size += wcslen(fk->dpStoreArray[i].dpString) * 2 + 2;
    if (FSaveDebug || fk->dpStoreArray[i].fIsOption) size += wcslen(fk->dpStoreArray[i].szName) * 2 + 2;
  }

  buf = new BYTE[size];
  if (!buf) return CERR_CannotAllocateMemory;
  memset(buf, 0, size);

  ck = (PCOMP_KEYBOARD)buf;

  ck->dwIdentifier = FILEID_COMPILED;
  ck->dwFileVersion = fk->version;
  ck->dwCheckSum = 0;		// do checksum afterwards.
  ck->KeyboardID = fk->KeyboardID;
  ck->IsRegistered = TRUE;    // I5135
  ck->cxStoreArray = fk->cxStoreArray;
  ck->cxGroupArray = fk->cxGroupArray;
  ck->StartGroup[0] = fk->StartGroup[0];
  ck->StartGroup[1] = fk->StartGroup[1];
  ck->dwHotKey = fk->dwHotKey;

  ck->dwFlags = fk->dwFlags;

  offset = sizeof(COMP_KEYBOARD);

  /*ck->dpLanguageName = offset;
  wcscpy((PWSTR)(buf + offset), fk->szLanguageName);
  offset += wcslen(fk->szLanguageName)*2 + 2;

  ck->dpName = offset;
  wcscpy((PWSTR)(buf + offset), fk->szName);
  offset += wcslen(fk->szName)*2 + 2;

  ck->dpCopyright = offset;
  wcscpy((PWSTR)(buf + offset), fk->szCopyright);
  offset += wcslen(fk->szCopyright)*2 + 2;

  ck->dpMessage = offset;
  wcscpy((PWSTR)(buf + offset), fk->szMessage);
  offset += wcslen(fk->szMessage)*2 + 2;*/

  ck->dpStoreArray = (DWORD)offset;
  sp = (PCOMP_STORE)(buf + offset);
  fsp = fk->dpStoreArray;
  offset += sizeof(COMP_STORE) * ck->cxStoreArray;
  for (i = 0; i < ck->cxStoreArray; i++, sp++, fsp++)
  {
    sp->dwSystemID = fsp->dwSystemID;
    sp->dpString = (DWORD)offset;
    wcscpy_s((PWSTR)(buf + offset), (size - offset) / sizeof(WCHAR), fsp->dpString);  // I3481   // I3641
    offset += wcslen(fsp->dpString) * 2 + 2;

    if (FSaveDebug || fsp->fIsOption)
    {
      sp->dpName = (DWORD)offset;
      wcscpy_s((PWSTR)(buf + offset), (size - offset) / sizeof(WCHAR), fsp->szName);  // I3481   // I3641
      offset += wcslen(fsp->szName) * 2 + 2;
    }
    else sp->dpName = 0;
  }

  ck->dpGroupArray = (DWORD)offset;
  gp = (PCOMP_GROUP)(buf + offset);
  fgp = fk->dpGroupArray;

  offset += sizeof(COMP_GROUP) * ck->cxGroupArray;

  for (i = 0; i < ck->cxGroupArray; i++, gp++, fgp++)
  {
    gp->cxKeyArray = fgp->cxKeyArray;
    gp->fUsingKeys = fgp->fUsingKeys;

    gp->dpMatch = gp->dpNoMatch = 0;

    if (fgp->dpMatch)
    {
      gp->dpMatch = (DWORD)offset;
      wcscpy_s((PWSTR)(buf + offset), (size - offset) / sizeof(WCHAR), fgp->dpMatch);  // I3481   // I3641
      offset += wcslen(fgp->dpMatch) * 2 + 2;
    }
    if (fgp->dpNoMatch)
    {
      gp->dpNoMatch = (DWORD)offset;
      wcscpy_s((PWSTR)(buf + offset), (size - offset) / sizeof(WCHAR), fgp->dpNoMatch);  // I3481   // I3641
      offset += wcslen(fgp->dpNoMatch) * 2 + 2;
    }

    if (FSaveDebug)
    {
      gp->dpName = (DWORD)offset;
      wcscpy_s((PWSTR)(buf + offset), (size - offset) / sizeof(WCHAR), fgp->szName);  // I3481   // I3641
      offset += wcslen(fgp->szName) * 2 + 2;
    }
    else gp->dpName = 0;

    gp->dpKeyArray = (DWORD)offset;
    kp = (PCOMP_KEY)(buf + offset);
    fkp = fgp->dpKeyArray;
    offset += gp->cxKeyArray * sizeof(COMP_KEY);
    for (j = 0; j < gp->cxKeyArray; j++, kp++, fkp++)
    {
      kp->_reserved = 0;
      kp->Key = fkp->Key;
      if (FSaveDebug) kp->Line = fkp->Line; else kp->Line = 0;
      kp->ShiftFlags = fkp->ShiftFlags;
      kp->dpOutput = (DWORD)offset;
      wcscpy_s((PWSTR)(buf + offset), (size - offset) / sizeof(WCHAR), fkp->dpOutput);  // I3481   // I3641
      offset += wcslen(fkp->dpOutput) * 2 + 2;
      kp->dpContext = (DWORD)offset;
      wcscpy_s((PWSTR)(buf + offset), (size - offset) / sizeof(WCHAR), fkp->dpContext);  // I3481   // I3641
      offset += wcslen(fkp->dpContext) * 2 + 2;
    }
  }

  ck->dwBitmapSize = fk->dwBitmapSize;
  ck->dpBitmapOffset = (DWORD)offset;
  memcpy(buf + offset, fk->lpBitmap, fk->dwBitmapSize);
  offset += fk->dwBitmapSize;

  if (offset != size) {
    delete[] buf;
    return CERR_SomewhereIGotItWrong;
  }

  if (ck->dwFileVersion < VERSION_160) {
    SetChecksum(buf, &ck->dwCheckSum, (DWORD)size);
  }
  else {
    ck->dwCheckSum = 0; // checksum is deprecated for 16.0+
  }

  DWORD dwBytesWritten = 0;
  WriteFile(hOutfile, buf, (DWORD)size, &dwBytesWritten, NULL);

  if (dwBytesWritten != size) {
    delete[] buf;
    return CERR_UnableToWriteFully;
  }

  delete[] buf;

  return CERR_None;
}

DWORD ReadLine(HANDLE hInfile, PWSTR wstr, BOOL PreProcess)
{
  DWORD len;
  PWSTR p;
  BOOL LineCarry = FALSE, InComment = FALSE;
  DWORD n;
  WCHAR currentQuotes = 0;
  WCHAR str[LINESIZE + 3];

  if (!ReadFile(hInfile, str, LINESIZE * 2, &len, NULL)) return CERR_CannotReadInfile;
  len /= 2;
  str[len] = 0;

  if (SetFilePointer(hInfile, 0, NULL, FILE_CURRENT) == GetFileSize(hInfile, NULL))
    // Always a "\r\n" to the EOF, avoids funny bugs
    wcscat_s(str, _countof(str), L"\r\n");  // I3481

  if (len == 0) return CERR_EndOfFile;

  for (p = str, n = 0; n < len; n++, p++)
  {
    if (currentQuotes != 0)
    {
      if (*p == L'\n')
      {
        *p = 0;  // I2525
        wcscpy_s(wstr, LINESIZE, str);  // I3481
        return (PreProcess ? CERR_None : CERR_UnterminatedString);
      }
      if (*p == currentQuotes) currentQuotes = 0;
      continue;
    }
    if (InComment) {
      if (*p == L'\n') break;
      *p = L' ';
      continue;
    }
    if (*p == L'\\') {
      LineCarry = TRUE;
      *p = L' ';
      continue;
    }
    if (LineCarry)
    {
      switch (*p)
      {
      case L' ':
      case L'\t':
      case L'\r':
        *p = L' ';
        continue;
      case L'\n':
        currentLine++;
        LineCarry = FALSE;
        *p = L' ';
        continue;
      }
      *p = 0; // I2525
      wcscpy_s(wstr, LINESIZE, str);  // I3481
      return (PreProcess ? CERR_None : CERR_InvalidLineContinuation);
    }

    if (*p == L'\n') break;
    switch (*p)
    {
    case L'c':
    case L'C':
      if ((p == str || iswspace(*(p - 1))) && iswspace(*(p + 1))) {
        InComment = TRUE;
        *p = L' ';
      }
      continue;
    case L'\r':
    case L'\t':
      *p = L' ';
      continue;
    case L'\'':
    case L'\"':
      currentQuotes = *p;
      continue;
    }
  }

  if (n == len)
  {
    str[LINESIZE - 1] = 0;  // I2525
    wcscpy_s(wstr, LINESIZE, str);  // I3481
    if (len == LINESIZE)
      return (PreProcess ? CERR_None : CERR_LineTooLong);
  }

  if (*p == L'\n') currentLine++;

  SetFilePointer(hInfile, -(int)(len * 2 - (INT_PTR)(p - str) * 2 - 2), NULL, FILE_CURRENT);

  p--;
  while (p >= str && iswspace(*p)) p--;
  p++;
  *p++ = L'\n';
  *p = 0;
  // trim spaces now, why not?
  wcscpy_s(wstr, LINESIZE, str);  // I3481

  return CERR_None;
}

DWORD GetRHS(PFILE_KEYBOARD fk, PWSTR p, PWSTR buf, int bufsize, int offset, int IsUnicode)
{
  PWSTR q;

  p = wcschr(p, '>');

  if (!p) return CERR_NoTokensFound;

  p++;

  return GetXString(fk, p, L"c\n", buf, bufsize, offset, &q, TRUE, IsUnicode);
}

void safe_wcsncpy(PWSTR out, PWSTR in, int cbMax)
{
  wcsncpy_s(out, cbMax, in, cbMax - 1);  // I3481
  out[cbMax - 1] = 0;
}

BOOL IsSameToken(PWSTR *p, PWSTR token)
{
  PWSTR q;
  q = *p;
  while (iswspace(*q)) q++;
  if (_wcsnicmp(q, token, wcslen(token)) == 0)
  {
    q += wcslen(token);
    while (iswspace(*q)) q++;
    *p = q;
    return TRUE;
  }
  return FALSE;
}


DWORD ImportBitmapFile(PFILE_KEYBOARD fk, PWSTR szName, PDWORD FileSize, PBYTE *Buf)
{
  HANDLE hFile;
  char szNewName[260], *p;

  p = wstrtostr(szName);

  if (IsRelativePath(p))
  {
    strcpy_s(szNewName, _countof(szNewName), CompileDir);  // I3481
    strcat_s(szNewName, _countof(szNewName), p);  // I3481
  }
  else
    strcpy_s(szNewName, _countof(szNewName), p);  // I3481

  hFile = CreateFileA(szNewName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
  if (hFile == INVALID_HANDLE_VALUE)
  {
    strcat_s(szNewName, _countof(szNewName), ".bmp");  // I3481
    hFile = CreateFileA(szNewName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
    if (hFile == INVALID_HANDLE_VALUE) return CERR_CannotReadBitmapFile;
  }

  DWORD msg;

  if ((msg = CheckFilenameConsistency(szNewName, FALSE)) != CERR_None) {
    return msg;
  }

  delete[] p;

  *FileSize = GetFileSize(hFile, NULL);

  if (*FileSize < 2) return CERR_CannotReadBitmapFile;

  *Buf = new BYTE[*FileSize];

  if (!ReadFile(hFile, *Buf, *FileSize, FileSize, NULL)) {
    delete[] * Buf;
    *Buf = NULL;
    return CERR_CannotReadBitmapFile;
  }

  CloseHandle(hFile);

  /* Test for version 7.0 icon support */
  if (*((PCHAR)*Buf) != 'B' && *(((PCHAR)*Buf) + 1) != 'M') {
    VERIFY_KEYBOARD_VERSION(fk, VERSION_70, CERR_70FeatureOnly);
  }

  return CERR_None;
}

int atoiW(PWSTR p)
{
  PSTR q = wstrtostr(p);
  int i = atoi(q);
  delete[] q;
  return i;
}

int CheckUTF16(int n)
{
  const int res[] = {
    0xFDD0, 0xFDD1, 0xFDD2, 0xFDD3, 0xFDD4, 0xFDD5, 0xFDD6, 0xFDD7,
    0xFDD8, 0xFDD9, 0xFDDA, 0xFDDB, 0xFDDC, 0xFDDD, 0xFDDE, 0xFDDF,
    0xFDE0, 0xFDE1, 0xFDE2, 0xFDE3, 0xFDE4, 0xFDE5, 0xFDE6, 0xFDE7,
    0xFDE8, 0xFDE9, 0xFDEA, 0xFDEB, 0xFDEC, 0xFDED, 0xFDEE, 0xFDEF,
    0xFFFF, 0xFFFE, 0 };

  if (n == 0) return CERR_ReservedCharacter;
  for (int i = 0; res[i] > 0; i++)
    if (n == res[i])
    {
      AddWarning(CWARN_ReservedCharacter);
      break;
    }
  return CERR_None;
}

int UTF32ToUTF16(int n, int *n1, int *n2)
{
  *n2 = -1;
  if (n <= 0xFFFF)
  {
    *n1 = n;
    if (n >= 0xD800 && n <= 0xDFFF) AddWarning(CWARN_UnicodeSurrogateUsed);
    return CheckUTF16(*n1);
  }

  if ((n & 0xFFFF) == 0xFFFF || (n & 0xFFFF) == 0xFFFE) AddWarning(CWARN_ReservedCharacter);
  if (n < 0 || n > 0x10FFFF) return CERR_InvalidCharacter;
  n = n - 0x10000;
  *n1 = (n / 0x400) + 0xD800;
  *n2 = (n % 0x400) + 0xDC00;
  if ((n = CheckUTF16(*n1)) != CERR_None) return n;
  return CheckUTF16(*n2);
}

DWORD BuildVKDictionary(PFILE_KEYBOARD fk)  // I3438
{
  DWORD i;
  size_t len = 0;
  if (fk->cxVKDictionary == 0) return CERR_None;
  for (i = 0; i < fk->cxVKDictionary; i++)
  {
    len += wcslen(fk->dpVKDictionary[i].szName) + 1;
  }
  PWSTR storeval = new WCHAR[len], p = storeval;
  for (i = 0; i < fk->cxVKDictionary; i++)
  {
    wcscpy_s(p, len - (size_t)(p - storeval), fk->dpVKDictionary[i].szName);  // I3481
    p = wcschr(p, 0);
    *p = ' ';
    p++;
  }

  p--;
  *p = 0;

  DWORD dwStoreID;
  DWORD msg = AddStore(fk, TSS_VKDICTIONARY, storeval, &dwStoreID);
  delete[] storeval;
  return msg;
}

int GetVKCode(PFILE_KEYBOARD fk, PWSTR p)  // I3438 // TODO: Consolidate GetDeadKey and GetVKCode?
{
  DWORD i;

  for (i = 0; i < fk->cxVKDictionary; i++)
    if (_wcsicmp(fk->dpVKDictionary[i].szName, p) == 0)
      return i + VK__MAX + 1;  // 256

  if (fk->cxVKDictionary % 10 == 0)
  {
    PFILE_VKDICTIONARY pvk = new FILE_VKDICTIONARY[fk->cxVKDictionary + 10];
    memcpy(pvk, fk->dpVKDictionary, fk->cxVKDictionary * sizeof(FILE_VKDICTIONARY));
    delete fk->dpVKDictionary;
    fk->dpVKDictionary = pvk;
  }
  wcsncpy_s(fk->dpVKDictionary[fk->cxVKDictionary].szName, _countof(fk->dpVKDictionary[fk->cxVKDictionary].szName), p, SZMAX_VKDICTIONARYNAME - 1);  // I3481
  fk->dpVKDictionary[fk->cxVKDictionary].szName[SZMAX_VKDICTIONARYNAME - 1] = 0;

  fk->cxVKDictionary++;
  return fk->cxVKDictionary + VK__MAX; // 256-1
}

int GetDeadKey(PFILE_KEYBOARD fk, PWSTR p)
{
  DWORD i;

  for (i = 0; i < fk->cxDeadKeyArray; i++)
    if (_wcsicmp(fk->dpDeadKeyArray[i].szName, p) == 0)
      return i + 1;

  if (fk->cxDeadKeyArray % 10 == 0)
  {
    PFILE_DEADKEY dk = new FILE_DEADKEY[fk->cxDeadKeyArray + 10];
    memcpy(dk, fk->dpDeadKeyArray, fk->cxDeadKeyArray * sizeof(FILE_DEADKEY));
    delete[] fk->dpDeadKeyArray;
    fk->dpDeadKeyArray = dk;
  }
  wcsncpy_s(fk->dpDeadKeyArray[fk->cxDeadKeyArray].szName, _countof(fk->dpDeadKeyArray[fk->cxDeadKeyArray].szName), p, SZMAX_DEADKEYNAME);  // I3481
  fk->dpDeadKeyArray[fk->cxDeadKeyArray].szName[SZMAX_DEADKEYNAME - 1] = 0;

  fk->cxDeadKeyArray++;
  return fk->cxDeadKeyArray;
}

void RecordDeadkeyNames(PFILE_KEYBOARD fk)
{
  WCHAR buf[SZMAX_DEADKEYNAME + 16];
  DWORD i;
  for (i = 0; i < fk->cxDeadKeyArray; i++)
  {
    swprintf(buf, _countof(buf), L"%ls%d %ls", DEBUGSTORE_DEADKEY, (int)i, fk->dpDeadKeyArray[i].szName);  // I3481
    AddDebugStore(fk, buf);
  }
}

BOOL IsValidCallStore(PFILE_STORE fs)
{
  int i;
  PWSTR p;
  for (i = 0, p = fs->dpString; *p; p++)
    if (*p == ':') i++;
    else if (!((*p >= 'a' && *p <= 'z') ||
      (*p >= 'A' && *p <= 'Z') ||
      (*p >= '0' && *p <= '9') ||
      *p == '.' ||
      *p == '_'))
      return FALSE;

  return i == 1;
}

HANDLE CreateTempFile()
{
  char szTempPathBuffer[MAX_PATH], szTempFileName[MAX_PATH];   // I3228   // I3510
  if (!GetTempPath(MAX_PATH, szTempPathBuffer)) return INVALID_HANDLE_VALUE;
  if (!GetTempFileName(szTempPathBuffer, "kmx", 0, szTempFileName)) return INVALID_HANDLE_VALUE;     // I3228   // I3510
  return CreateFile(szTempFileName, GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_ALWAYS,
    FILE_ATTRIBUTE_TEMPORARY | FILE_FLAG_DELETE_ON_CLOSE, NULL);
}

///////////////////
HANDLE UTF16TempFromUTF8(HANDLE hInfile, BOOL hasPreamble)
{
  HANDLE hOutfile = CreateTempFile();
  if (hOutfile == INVALID_HANDLE_VALUE)     // I3228   // I3510
  {
    CloseHandle(hInfile);
    return INVALID_HANDLE_VALUE;
  }

  PBYTE buf, p;
  PWSTR outbuf, poutbuf;
  DWORD len, len2;
  WCHAR prolog = 0xFEFF;
  WriteFile(hOutfile, &prolog, 2, &len2, NULL);

  len = GetFileSize(hInfile, NULL);
  if (hasPreamble) {
    SetFilePointer(hInfile, 3, NULL, FILE_BEGIN); // Cut off UTF-8 marker
    len -= 3;
  }

  buf = new BYTE[len + 1]; // null terminated
  outbuf = new WCHAR[len + 1];
  if (ReadFile(hInfile, buf, len, &len2, NULL)) {
    buf[len2] = 0;
    p = buf;
    poutbuf = outbuf;
    if (hasPreamble) {
      // We have a preamble, so we attempt to read as UTF-8 and allow conversion errors to be filtered. This is not great for a
      // compiler but matches existing behaviour -- in future versions we may not do lenient conversion.
      ConversionResult cr = ConvertUTF8toUTF16(&p, &buf[len2], (UTF16 **)&poutbuf, (const UTF16 *)&outbuf[len], lenientConversion);
      WriteFile(hOutfile, outbuf, (DWORD)(INT_PTR)(poutbuf - outbuf) * 2, &len2, NULL);
    }
    else {
      // No preamble, so we attempt to read as strict UTF-8 and fall back to ANSI if that fails
      ConversionResult cr = ConvertUTF8toUTF16(&p, &buf[len2], (UTF16 **)&poutbuf, (const UTF16 *)&outbuf[len], strictConversion);
      if (cr == sourceIllegal) {
        // Not a valid UTF-8 file, so fall back to ANSI
        AddCompileMessage(CHINT_NonUnicodeFile);
        poutbuf = strtowstr((PSTR)buf);
        WriteFile(hOutfile, poutbuf, (DWORD)wcslen(poutbuf) * 2, &len2, NULL);
        delete[] poutbuf;
      }
      else {
        WriteFile(hOutfile, outbuf, (DWORD)(INT_PTR)(poutbuf - outbuf) * 2, &len2, NULL);
      }
    }
  }

  CloseHandle(hInfile);
  delete[] buf;
  delete[] outbuf;
  SetFilePointer(hOutfile, 2, NULL, FILE_BEGIN);
  return hOutfile;
}

extern "C" void __declspec(dllexport) Keyman_Diagnostic(int mode) {
  if (mode == 0) {
    RaiseException(0x0EA0BEEF, EXCEPTION_NONCONTINUABLE, 0, NULL);
  }
}

PFILE_STORE FindSystemStore(PFILE_KEYBOARD fk, DWORD dwSystemID) {
  assert(fk != NULL);
  assert(dwSystemID != 0);

  PFILE_STORE sp = fk->dpStoreArray;
  for (DWORD i = 0; i < fk->cxStoreArray; i++, sp++) {
    if (sp->dwSystemID == dwSystemID) {
      return sp;
    }
  }
  return NULL;
}
