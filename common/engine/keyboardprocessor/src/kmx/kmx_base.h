#pragma once

#include <keyman/keyboardprocessor_bits.h>
#include <keyman/keyboardprocessor.h>

#if defined(_WIN32) || defined(_WIN64) 
#define snprintf _snprintf 
#define vsnprintf _vsnprintf 
#define strcasecmp _stricmp 
#define strncasecmp _strnicmp 
#endif

#if __x86_64__
/* 64-bit, g++ */
#define KMX_64BIT
#endif

#if defined(_WIN64) && !defined(USE_64)
/* 64-bit, Windows */
#define KMX_64BIT
#endif

typedef uint32_t            KMX_DWORD;
typedef int32_t             KMX_BOOL;
typedef uint8_t             KMX_BYTE;
typedef uint16_t            KMX_WORD;

typedef km_kbp_cp KMX_WCHAR;    // wc,   16-bit UNICODE character
typedef KMX_WCHAR *PKMX_WCHAR;

typedef char KMX_CHAR;
typedef KMX_CHAR *PKMX_CHAR;

typedef uint32_t             KMX_UINT;

typedef KMX_BYTE *PKMX_BYTE;
typedef KMX_WORD *PKMX_WORD;
typedef KMX_DWORD *PKMX_DWORD;

#ifndef FALSE
#define FALSE               0
#endif

#ifndef TRUE
#define TRUE                1
#endif

namespace km {
namespace kbp {
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

  KMX_DWORD dwCheckSum;   // As stored in keyboard
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
} // namespace kbp
} // namespace km
