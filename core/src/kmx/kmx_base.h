#pragma once

#include "keyman_core.h"

#if defined(_WIN32) || defined(_WIN64)
#define snprintf _snprintf
#define vsnprintf _vsnprintf
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif

namespace km {
namespace core {
namespace kmx {

typedef struct tagSTORE
{
  KMX_DWORD dwSystemID;
  PKMX_WCHAR dpName;
  PKMX_WCHAR dpString;
} STORE, *LPSTORE;


typedef struct tagKEY
{
  KMX_WCHAR Key;
  KMX_DWORD Line;
  KMX_DWORD ShiftFlags;
  PKMX_WCHAR dpOutput;
  PKMX_WCHAR dpContext;
} KEY, *LPKEY;


typedef struct tagGROUP
{
  PKMX_WCHAR dpName;
  LPKEY dpKeyArray;   // [LPKEY] address of first item in key array
  PKMX_WCHAR dpMatch;
  PKMX_WCHAR dpNoMatch;
  KMX_DWORD cxKeyArray;   // in array entries
  KMX_BOOL  fUsingKeys;   // group(xx) [using keys] <-- specified or not
} GROUP, *LPGROUP;


typedef struct tagKEYBOARD
{
  KMX_DWORD dwIdentifier;   // Keyman compiled keyboard id

  KMX_DWORD dwFileVersion;  // Version of the file - Keyman 4.0 is 0x0400

  KMX_DWORD dwCheckSum;   // As stored in keyboard. DEPRECATED as of 16.0
  KMX_DWORD xxkbdlayout;      // as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
  KMX_DWORD IsRegistered;   // layout id, from same key
  KMX_DWORD version;      // keyboard version

  KMX_DWORD cxStoreArray;   // in array entries
  KMX_DWORD cxGroupArray;   // in array entries

  LPSTORE dpStoreArray; // [LPSTORE] address of first item in store array, from start of file
  LPGROUP dpGroupArray; // [LPGROUP] address of first item in group array, from start of file

  KMX_DWORD StartGroup[2];  // index of starting groups [2 of them]
              // Ansi=0, Unicode=1

  KMX_DWORD dwFlags;      // Flags for the keyboard file

  KMX_DWORD dwHotKey;     // standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)
} KEYBOARD, *LPKEYBOARD;

typedef struct tagINTKEYBOARDOPTIONS
{
  PKMX_WCHAR Value;
  PKMX_WCHAR OriginalStore;
} INTKEYBOARDOPTIONS, *LPINTKEYBOARDOPTIONS;

// The members of this structure, from first through to IMDLLs, must match KEYBOARDINFO from keymanapi.h
typedef struct tagINTKEYBOARDINFO
{
  KMX_DWORD      KeymanID;
  char       Name[256];
  LPKEYBOARD Keyboard;
  LPINTKEYBOARDOPTIONS KeyboardOptions;
} INTKEYBOARDINFO, *LPINTKEYBOARDINFO;

typedef struct tagKMSTATE
{
  KMX_BOOL StopOutput;
  int LoopTimes;
  KMX_BOOL isExtended;
  KMX_WORD vkey; // I934
  KMX_WCHAR charCode;   // I4582
} KMSTATE;
// I3616
enum ProcessStringReturn { psrPostMessages, psrCheckMatches };

} // namespace kmx
} // namespace core
} // namespace km
