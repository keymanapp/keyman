#pragma once

#include "compfile.h"

namespace kmcmp {
  KMX_BOOL AddCompileWarning(LPSTR buf);
  extern BOOL FWarnDeprecatedCode;
  extern int currentLine;
}
extern char ErrExtraLIB[256];
extern KMX_WCHAR ErrExtraW[256];
KMX_BOOL AddCompileError(KMX_DWORD msg);

// TODO: These macros can return FALSE in functions that expect a DWORD CERR_x
//       return value type. This is just plain wrong!
#define SetError(err)       { if(AddCompileError(err) || (err & CERR_FATAL)) return FALSE; }
#define AddWarning(warn)    { if(AddCompileError(warn)) return FALSE; }

PKMX_WCHAR strtowstr(PKMX_STR in);
PFILE_STORE FindSystemStore(PFILE_KEYBOARD fk, KMX_DWORD dwSystemID);
