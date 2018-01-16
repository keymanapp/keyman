#pragma once

#include "keyman64.h"
#include "AbstractKeymanRuleProcessor.h"
#include "imdll.h"

typedef struct tagINTKEYBOARDOPTIONS
{
  PWCHAR Value;
  PWCHAR OriginalStore;
} INTKEYBOARDOPTIONS, *LPINTKEYBOARDOPTIONS;

typedef struct tagINTKEYBOARDPROFILE
{
  WCHAR Locale[LOCALE_NAME_MAX_LENGTH];
  LANGID LangID;
  GUID Guid;
} INTKEYBOARDPROFILE, *LPINTKEYBOARDPROFILE;

// The members of this structure, from first through to IMDLLs, must match KEYBOARDINFO from keymanapi.h
typedef struct tagINTKEYBOARDINFO
{
  DWORD      KeymanID;
  DWORD      __filler_Hotkey;
  DWORD      __filler; // makes same as KEYBOARDINFO   // I4462
  char       Name[256];
  LPKEYBOARD Keyboard;
  DWORD      nIMDLLs;
  LPIMDLL    IMDLLs;
  int        __filler2; // makes same as KEYBOARDINFO
  LPINTKEYBOARDOPTIONS KeyboardOptions;
  int        nProfiles;
  LPINTKEYBOARDPROFILE Profiles;
  AbstractKeymanRuleProcessor *ruleProcessor;
} INTKEYBOARDINFO, *LPINTKEYBOARDINFO;
