#pragma once

#include <windows.h>
#include "compfile.h"

BOOL AddCompileString(LPSTR buf);
BOOL AddCompileMessage(DWORD msg);

#define SetError(err)       { if(AddCompileMessage(err) || (err & (CERR_MEMORY|CERR_FATAL))) return FALSE; }
#define AddWarning(warn)    { if(AddCompileMessage(warn)) return FALSE; }

extern BOOL FWarnDeprecatedCode;
extern int currentLine;

PWSTR strtowstr(PSTR in);
PFILE_STORE FindSystemStore(PFILE_KEYBOARD fk, DWORD dwSystemID);


//**************************************************************

PKMX_FILE_STORE KMX_FindSystemStore(PKMX_FILE_KEYBOARD fk, KMX_DWORD dwSystemID);