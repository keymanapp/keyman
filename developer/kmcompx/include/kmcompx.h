

// taken from C:\Projects\keyman\keyman\common\core\desktop\src\kmx\kmx_base.h
#pragma once
#include <stdint.h>

#include <keyman/keyboardprocessor.h>		
#include <keyman/keyboardprocessor_bits.h>	




// typedef:old ->new
typedef uint32_t            KMX_DWORD;
typedef uint32_t            KMX_UINT;
typedef uint16_t            KMX_WORD;
typedef uint8_t             KMX_BYTE;
typedef int32_t             KMX_BOOL;
typedef int                 INT_PKMX;
typedef char                KMX_CHAR;
//typedef char16_t          km_kbp_cp
typedef km_kbp_cp           KMX_WCHAR;    // wc,   16-bit UNICODE character -> char16_t
typedef wchar_t             KMX_WCHART;    // wc,   16-bit UNICODE character-> wchar_t

typedef KMX_CHAR *   PKMX_CHAR;
typedef KMX_WCHAR *  PKMX_WCHAR;
typedef KMX_WCHAR *  PKMX_WSTR;      // n win: typedef WCHAR *PWSTR;
//typedef KMX_WCHAR *  KMX_PWSTR;      // n win: typedef WCHAR *PWSTR; // s ??
typedef KMX_WCHART * PKMX_WCHART;      // n win: typedef WCHAR *PWSTR; // s ??

typedef KMX_BYTE *   PKMX_BYTE;        // never used?
typedef KMX_BYTE *  LPKMX_BYTE;        // never used?
typedef KMX_WORD *   PKMX_WORD;        // KMX_WORD  * never used?  (KMX_WORD  is used)
typedef KMX_WORD *  LPKMX_WORD;        // KMX_WORD  * never used?  (KMX_WORD  is used)
typedef KMX_DWORD *  PKMX_DWORD;       // KMX_DWORD * never used?  (KMX_DWORD is used)
typedef KMX_DWORD * LPKMX_DWORD;       // KMX_DWORD * never used?  (KMX_DWORD is used)
typedef char *       PKMX_STR;

//typedef KMX_DWORD *KMX_LPDWORD;
typedef PKMX_WSTR *  PKMX_PWSTR;      // s ??
typedef PKMX_WSTR * LPKMX_STR;       // s ??
typedef PKMX_WSTR * PPKMX_WSTR;      // s ??
typedef KMX_WCHART* LPKMX_WCHART;      // s ??
typedef KMX_WCHAR* LPKMX_WCHAR;      // s ??
//typedef KMX_PWSTR * LPKMX_STR;      

//DWORD x;
namespace km {
namespace kbp {
namespace kmx {

typedef struct KMX_tagSTORE
{
  KMX_DWORD dwSystemID;
  PKMX_WCHAR dpName;
  PKMX_WCHAR dpString;
} KMX_STORE, *LPKMX_STORE;


typedef struct KMX_tagKEY
{
  KMX_WCHAR Key;
  KMX_DWORD Line;
  KMX_DWORD ShiftFlags;
  PKMX_WCHAR dpOutput;
  PKMX_WCHAR dpContext;
} KMX_KEY, *LPKMX_KEY;


typedef struct KMX_tagGROUP
{
  PKMX_WCHAR dpName;
  LPKMX_KEY dpKeyArray;   // [LPKEY] address of first item in key array
  PKMX_WCHAR dpMatch;
  PKMX_WCHAR dpNoMatch;
  KMX_DWORD cxKeyArray;   // in array entries
  KMX_BOOL  fUsingKeys;   // group(xx) [using keys] <-- specified or not
} KMX_GROUP, *LPKMX_GROUP;


typedef struct KMX_tagKEYBOARD
{
  KMX_DWORD dwIdentifier;   // Keyman compiled keyboard id

  KMX_DWORD dwFileVersion;  // Version of the file - Keyman 4.0 is 0x0400

  KMX_DWORD dwCheckSum;     // As stored in keyboard
  KMX_DWORD xxkbdlayout;    // as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
  KMX_DWORD IsRegistered;   // layout id, from same key
  KMX_DWORD version;        // keyboard version

  KMX_DWORD cxStoreArray;   // in array entries
  KMX_DWORD cxGroupArray;   // in array entries

  LPKMX_STORE dpStoreArray; // [LPSTORE] address of first item in store array, from start of file
  LPKMX_GROUP dpGroupArray; // [LPGROUP] address of first item in group array, from start of file

  KMX_DWORD StartGroup[2];  // index of starting groups [2 of them]
                            // Ansi=0, Unicode=1

  KMX_DWORD dwFlags;        // Flags for the keyboard file

  KMX_DWORD dwHotKey;       // standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)
} KMX_KEYBOARD, *LPKMX_KEYBOARD;

typedef struct KMX_tagINTKEYBOARDOPTIONS
{
  PKMX_WCHAR Value;
  PKMX_WCHAR OriginalStore;
} KMX_INTKEYBOARDOPTIONS, *LPKMX_INTKEYBOARDOPTIONS;

// The members of this structure, from first through to IMDLLs, must match KEYBOARDINFO from keymanapi.h
typedef struct KMX_tagINTKEYBOARDINFO
{
  KMX_DWORD      KeymanID;
  KMX_CHAR       Name[256];   // _S2 :this was original:  char       Name[256];     
  LPKMX_KEYBOARD Keyboard;
  LPKMX_INTKEYBOARDOPTIONS KeyboardOptions;
} KMX_INTKEYBOARDINFO, *LPKMX_INTKEYBOARDINFO;

typedef struct KMX_tagKMSTATE
{
  KMX_BOOL StopOutput;
  int LoopTimes;
  KMX_BOOL isExtended;
  KMX_WORD vkey; // I934
  KMX_WCHAR charCode;   // I4582
} KMX_KMSTATE;
// I3616
enum KMX_ProcessStringReturn { KMX_psrPostMessages, KMX_psrCheckMatches };

} // namespace kmx
} // namespace kbp
} // namespace km