#pragma once

#include <windows.h>
#include "compfile.h"

KMX_BOOL AddCompileString(LPSTR buf);
KMX_BOOL AddCompileMessage(KMX_DWORD msg);

#define SetError(err)       { if(AddCompileMessage(err) || (err & (CERR_HINT|CERR_FATAL))) return FALSE; }
#define AddWarning(warn)    { if(AddCompileMessage(warn)) return FALSE; }

extern BOOL FWarnDeprecatedCode;
extern int currentLine;

PKMX_WCHAR strtowstr(PKMX_STR in);
PFILE_STORE FindSystemStore(PFILE_KEYBOARD fk, KMX_DWORD dwSystemID);
