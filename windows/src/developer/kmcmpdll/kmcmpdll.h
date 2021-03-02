#pragma once

#include <windows.h>

BOOL AddCompileString(LPSTR buf);
BOOL AddCompileMessage(DWORD msg);

#define SetError(err)       { if(AddCompileMessage(err) || (err & (CERR_MEMORY|CERR_FATAL))) return FALSE; }
#define AddWarning(warn)    { if(AddCompileMessage(warn)) return FALSE; }

extern BOOL FWarnDeprecatedCode;
extern int currentLine;

PWSTR strtowstr(PSTR in);
