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
void AddCompileError(KMX_DWORD msg); // TODO rename to AddCompileMessage

/// Use AddWarningBool for functions that return bool or KMX_BOOL; TODO merge with AddCompileMessage
#define AddWarningBool(warn)  AddCompileError(warn)
/// Use AddWarning for functions that return KMX_DWORD; TODO merge with AddCompileMessage
#define AddWarning(warn)      AddCompileError(warn)
