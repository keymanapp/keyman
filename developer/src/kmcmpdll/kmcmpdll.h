#pragma once

#include <windows.h>
#include "compfile.h"

BOOL AddCompileString(LPSTR buf);
BOOL AddCompileMessage(DWORD msg);

// TODO: These macros can return FALSE in functions that expect a DWORD CERR_x
//       return value type. This is just plain wrong!
#define SetError(err)       { if(AddCompileMessage(err) || (err & CERR_FATAL)) return FALSE; }
#define AddWarning(warn)    { if(AddCompileMessage(warn)) return FALSE; }

extern BOOL FWarnDeprecatedCode;
extern int currentLine;
extern char ErrExtra[];


PWSTR strtowstr(PSTR in);
PFILE_STORE FindSystemStore(PFILE_KEYBOARD fk, DWORD dwSystemID);
