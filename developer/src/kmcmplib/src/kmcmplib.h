#pragma once

#include <kmcmplibapi.h>
#include "compfile.h"
#include "NamedCodeConstants.h"

namespace kmcmp {
  KMX_BOOL AddCompileWarning(char* buf);
  extern int currentLine;
  extern KMX_BOOL FShouldAddCompilerVersion;
  extern KMX_BOOL  FSaveDebug, FCompilerWarningsAsErrors;   // I4865   // I4866
  extern int ErrChr;
  extern int nErrors;
  extern KMX_BOOL FMnemonicLayout;
  extern KMX_BOOL FOldCharPosMatching;
  extern int CompileTarget;
  extern KMX_CHAR CompileDir[260];  // TODO: this should not be a fixed buffer
  extern int BeginLine[4];
  extern int currentLine;
  extern NamedCodeConstants *CodeConstants;

  void RecordDeadkeyNames(PFILE_KEYBOARD fk);
  KMX_DWORD AddCompilerVersionStore(PFILE_KEYBOARD fk);
}

extern kmcmp_CompilerMessageProc msgproc;
extern void* msgprocContext;

extern KMX_BOOL AWarnDeprecatedCode_GLOBAL_LIB;
#define ERR_EXTRA_LIB_LEN 256
#define ERR_EXTRA_W_LEN 256
extern char ErrExtraLIB[ERR_EXTRA_LIB_LEN];
extern KMX_WCHAR ErrExtraW[ERR_EXTRA_W_LEN];
KMX_BOOL AddCompileError(KMX_DWORD msg);

/// Use AddWarningBool for functions that return bool or KMX_BOOL
#define AddWarningBool(warn)  { if(AddCompileError(warn)) return FALSE; }
/// Use AddWarning for functions that return KMX_DWORD
#define AddWarning(warn)      { if(AddCompileError(warn)) return CERR_Break; }

PKMX_WCHAR strtowstr(PKMX_STR in);
PFILE_STORE FindSystemStore(PFILE_KEYBOARD fk, KMX_DWORD dwSystemID);
FILE* UTF16TempFromUTF8(FILE* fp_in , KMX_BOOL hasPreamble);
KMX_DWORD WriteCompiledKeyboard(PFILE_KEYBOARD fk, FILE* fp_out);
KMX_DWORD AddStore(PFILE_KEYBOARD fk, KMX_DWORD SystemID, KMX_WCHAR const * str, KMX_DWORD *dwStoreID= NULL);
KMX_DWORD ReadLine(FILE* fp_in , PKMX_WCHAR wstr, KMX_BOOL PreProcess);
KMX_DWORD ParseLine(PFILE_KEYBOARD fk, PKMX_WCHAR str);
KMX_DWORD ProcessGroupLine(PFILE_KEYBOARD fk, PKMX_WCHAR p);
KMX_DWORD ProcessGroupFinish(PFILE_KEYBOARD fk);
KMX_DWORD ProcessStoreLine(PFILE_KEYBOARD fk, PKMX_WCHAR p);
int LineTokenType(PKMX_WCHAR *str);
KMX_DWORD BuildVKDictionary(PFILE_KEYBOARD fk); // I3438
