#pragma once

#define SZMAX_ERRORTEXT 512
#define COMPILE_ERROR_MAX_LEN (SZMAX_ERRORTEXT + 1 + 280)
#define ERR_EXTRA_LIB_LEN 256

namespace kmcmp {
  extern kmcmp_CompilerMessageProc msgproc;
  KMX_BOOL AddCompileWarning(char* buf);
}

extern char ErrExtraLIB[ERR_EXTRA_LIB_LEN];

extern void* msgprocContext;
KMX_BOOL AddCompileError(KMX_DWORD msg);

/// Use AddWarningBool for functions that return bool or KMX_BOOL
#define AddWarningBool(warn)  { if(AddCompileError(warn)) return FALSE; }
/// Use AddWarning for functions that return KMX_DWORD
#define AddWarning(warn)      { if(AddCompileError(warn)) return CERR_Break; }
