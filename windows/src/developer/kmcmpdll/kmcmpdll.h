#pragma once

#include <windows.h>

BOOL AddCompileString(LPSTR buf);
BOOL AddCompileMessage(DWORD msg);

#define SetError(err)       { if(AddCompileMessage(err)) return FALSE; }
#define AddWarning(warn)    { if(AddCompileMessage(warn)) return FALSE; }

extern BOOL FWarnDeprecatedCode;
extern int currentLine;