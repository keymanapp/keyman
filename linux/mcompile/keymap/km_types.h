#pragma once
#ifndef KM_TYPES
#define KM_TYPES

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

typedef char16_t   KMX_WCHAR;           // _S2
typedef KMX_WCHAR* PKMX_WCHAR;          // _S2
typedef wchar_t*   PWSTR;               // _S2 needs to be removed/ wchart-> char16
//typedef PWSTR      PKMX_WCHART;         // _S2 needs to be removed/ wchart-> char16
//typedef const wchar_t*  PCKMX_WCHART;   // _S2 needs to be removed/?

typedef char*      LPSTR;               // _S2 needs to be removed?
//typedef LPSTR      LPKMX_STR;           // _S2 needs to be removed?

typedef uint8_t*   LPBYTE;              // _S2 needs to be removed/?
typedef uint8_t*   LPKMX_BYTE;          // _S2 needs to be removed?

typedef uint8_t*   PBYTE;               // _S2 needs to be removed/?
typedef uint8_t*   PKMX_BYTE;           // _S2 needs to be removed?

typedef char       KMX_CHAR;            // _S2 needs to be removed/?
//typedef char*      PKMX_STR;            // _S2 needs to be removed/?

typedef unsigned int UINT;              // _S2 needs to be removed/?
typedef unsigned char   BYTE;           // _S2 needs to be removed?
typedef unsigned long  DWORD;           // _S2 needs to be removed/?
typedef unsigned short WORD;            // _S2 needs to be removed/?
typedef wchar_t*     LPWSTR;            // _S2 needs to be removed/?
typedef WCHAR*       PWCHAR;            // _S2 needs to be removed/?

typedef KMX_CHAR*  PKMX_CHAR;           // _S2 needs to be removed/?

typedef int        BOOL;                // _S2 needs to be removed/? or is it int32_t??

                                        // in WIN:
                                        // PVOID A pointer to any type.
                                        // typedef PVOID HANDLE;
                                        // typedef HANDLE HKL;
typedef void* KMX_HKL;                  // _S2 what is the equivalent to HKL and do I need it?? I assume a void*

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

#define VK_SPACE    0x20
#define VK_COLON	0xBA
#define VK_EQUAL	0xBB
#define VK_COMMA	0xBC
#define VK_HYPHEN	0xBD
#define VK_PERIOD	0xBE
#define	VK_SLASH	0xBF
#define VK_ACCENT	0xC0
#define VK_LBRKT	0xDB
#define VK_BKSLASH	0xDC
#define VK_RBRKT	0xDD
#define VK_QUOTE	0xDE
#define VK_xDF		0xDF
#define VK_OEM_102  0xE2  //  "<>" or "\|" on RT 102-key kbd.

/*#define VK_NUMPAD0  0x5A
#define VK_NUMPAD1  0x57
#define VK_NUMPAD2  0x58
#define VK_NUMPAD3  0x59
#define VK_NUMPAD4  0x53
#define VK_NUMPAD5  0x54
#define VK_NUMPAD6  0x55
#define VK_NUMPAD7  0x4F
#define VK_NUMPAD8  0x50
#define VK_NUMPAD9  0x51
#define VK_DIVIDE   0x6A
#define VK_CANCEL   0x09
#define VK_DECIMAL  0x5B*/

// _S2 correct?? Do I need NUMPAD??
#define VK_NUMPAD0  96
#define VK_NUMPAD1  97
#define VK_NUMPAD2  98
#define VK_NUMPAD3  99
#define VK_NUMPAD4  100
#define VK_NUMPAD5  101
#define VK_NUMPAD6  102
#define VK_NUMPAD7  103
#define VK_NUMPAD8  104
#define VK_NUMPAD9  105
#define VK_DIVIDE   111
#define VK_CANCEL   3
#define VK_DECIMAL  110

#define VK_OEM_CLEAR      0xFE
#define VK_LSHIFT         0xA0
#define VK_RSHIFT         0xA1
#define VK_LCONTROL       0xA2
#define VK_RCONTROL       0xA3
#define VK_LMENU          0xA4
#define VK_RMENU          0xA5

#define VK_SHIFT          0x10
#define VK_CONTROL        0x11
#define VK_MENU           0x12
#define VK_PAUSE          0x13
#define VK_CAPITAL        0x14

#endif /*KM_TYPES*/
