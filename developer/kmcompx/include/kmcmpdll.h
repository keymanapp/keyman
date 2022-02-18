#pragma once

//#include <windows.h>       // Can be removed here :-) 
#include "compfile.h"


//**************************************************************

KMX_BOOL AddCompileString(LPSTR buf);
KMX_BOOL AddCompileMessage(KMX_DWORD msg);

#define SetError(err)       { if(AddCompileMessage(err) || (err & (CERR_MEMORY|CERR_FATAL))) return FALSE; }
#define AddWarning(warn)    { if(AddCompileMessage(warn)) return FALSE; }

extern BOOL FWarnDeprecatedCode;
extern int currentLine;

PKMX_WCHAR strtowstr(PKMX_STR in);
PFILE_STORE FindSystemStore(PFILE_KEYBOARD fk, KMX_DWORD dwSystemID);

//***old***********************************************************
/*
BOOL AddCompileString(LPSTR buf);
BOOL AddCompileMessage(DWORD msg);

#define SetError(err)       { if(AddCompileMessage(err) || (err & (CERR_MEMORY|CERR_FATAL))) return FALSE; }
#define AddWarning(warn)    { if(AddCompileMessage(warn)) return FALSE; }

extern BOOL FWarnDeprecatedCode;
extern int currentLine;

PWSTR strtowstr(PSTR in);
PFILE_STORE FindSystemStore(PFILE_KEYBOARD fk, DWORD dwSystemID);
*/