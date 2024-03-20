#pragma once
#include <stdint.h>

#include <stdint.h>

/*
#if defined(_WIN32) || defined(_WIN64)
#define snprintf _snprintf
#define vsnprintf _vsnprintf
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif
*/

#if defined(__LP64__) || defined(_LP64)
/* 64-bit, g++ */
#define KMX_64BIT
#endif

#if defined(_WIN64) && !defined(USE_64)
/* 64-bit, Windows */
#define KMX_64BIT
#endif

typedef uint32_t   KMX_DWORD;
typedef int32_t    KMX_BOOL;
typedef uint8_t    KMX_BYTE;
typedef uint16_t   KMX_WORD;

#if defined(__cplusplus)
typedef char16_t   km_kbp_cp;
typedef char32_t   km_kbp_usv;
#else
typedef uint16_t   km_kbp_cp;          // code point
typedef uint32_t   km_kbp_usv;         // Unicode Scalar Value
#endif

typedef km_kbp_cp  KMX_WCHAR;    // wc,   16-bit UNICODE character

typedef wchar_t    WCHAR;               // _S2 needs to be removed/ wchart-> char16
typedef WCHAR      KMX_WCHART;          // _S2 needs to be removed/ wchart-> char16
typedef KMX_WCHAR* PKMX_WCHAR;          // _S2
typedef wchar_t*   PWSTR;               // _S2 needs to be removed/ wchart-> char16
typedef PWSTR      PKMX_WCHART;         // _S2 needs to be removed/ wchart-> char16

typedef wchar_t*   LPKMX_WCHART;        // _S2 needs to be removed/ wchart-> char16

typedef char*      LPSTR;               // _S2 needs to be removed?
typedef LPSTR      LPKMX_STR;           // _S2 needs to be removed?

typedef uint8_t*   LPBYTE;              // _S2 needs to be removed/?
typedef LPBYTE     LPKMX_BYTE;          // _S2 needs to be removed?

typedef uint8_t*   PBYTE;               // _S2 needs to be removed/?
typedef PBYTE      PKMX_BYTE;           // _S2 needs to be removed?

                                        // _S2 LPKEYBOARD ok to leave as is??

typedef char       KMX_CHAR;            // _S2 needs to be removed/?
typedef char*      PKMX_STR;            // _S2 needs to be removed/?

typedef KMX_CHAR*  PKMX_CHAR;           // _S2 needs to be removed/?

typedef uint32_t   KMX_UINT;

typedef KMX_BYTE*  PKMX_BYTE;
typedef KMX_WORD*  PKMX_WORD;
typedef KMX_DWORD* PKMX_DWORD;

#ifndef FALSE
#define FALSE               0
#endif

#ifndef TRUE
#define TRUE                1
#endif

// Macros and types to support char16_t vs wchar_t depending on project

#ifdef USE_CHAR16_T
#define lpuch(x) u ## x
typedef  km_kbp_cp KMX_UCHAR;
#else
#define lpuch(x) L ## x
typedef  wchar_t KMX_UCHAR;
#endif

typedef KMX_UCHAR* KMX_PUCHAR;
