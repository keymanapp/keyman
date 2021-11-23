

// taken from C:\Projects\keyman\keyman\common\core\desktop\src\kmx\kmx_base.h
//  we are here: C:\Projects\keyman\keyman\developer\kmcompx\include
#pragma once
#include <stdint.h>

//#include <keyman/keyboardprocessor.h>		// removed S
#include "keyman/keyboardprocessor.h"	




// old ->new
typedef uint32_t            KMX_DWORD;
typedef int32_t             KMX_BOOL;
typedef uint8_t             KMX_BYTE;
typedef uint16_t            KMX_WORD;
typedef uint32_t            KMX_UINT;
typedef char                KMX_CHAR;

typedef km_kbp_cp KMX_WCHAR;    // wc,   16-bit UNICODE character

typedef KMX_CHAR *   PKMX_CHAR;
typedef KMX_WCHAR *  PKMX_WCHAR;
typedef KMX_BYTE *   PKMX_BYTE;        // never used?
typedef KMX_BYTE *  LPKMX_BYTE;        // never used?
typedef KMX_WORD *   PKMX_WORD;        // KMX_WORD  * never used?  (KMX_WORD  is used)
typedef KMX_WORD *  LPKMX_WORD;        // KMX_WORD  * never used?  (KMX_WORD  is used)
typedef KMX_DWORD *  PKMX_DWORD;      // KMX_DWORD * never used?  (KMX_DWORD is used)
typedef KMX_DWORD * LPKMX_DWORD;      // KMX_DWORD * never used?  (KMX_DWORD is used)
typedef char *       PKMX_STR;
typedef KMX_WCHAR  * PKMX_WSTR;
typedef int INT_PKMX;



//typedef KMX_DWORD *KMX_LPDWORD;

typedef KMX_WCHAR * KMX_PWSTR;      // n win: typedef WCHAR *PWSTR;
typedef KMX_PWSTR * PKMX_PWSTR;      
typedef KMX_PWSTR * LPKMX_STR;      


typedef KMX_WCHAR * PKMX_WSTR;      // n win: typedef WCHAR *PWSTR;
typedef KMX_PWSTR * PPKMX_WSTR;      
//typedef KMX_PWSTR * LPKMX_STR;      

//DWORD x;
namespace km {
namespace kbp {
namespace kmx {
/*
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

*/

} // namespace kmx
} // namespace kbp
} // namespace km