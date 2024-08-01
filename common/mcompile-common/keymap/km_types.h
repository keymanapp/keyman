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

typedef unsigned int  UINT;
typedef unsigned long DWORD;

typedef unsigned char BYTE;
typedef char          KMX_CHAR;

typedef char16_t      KMX_WCHAR;
typedef KMX_WCHAR*    PKMX_WCHAR;

typedef wchar_t       WCHAR;
typedef WCHAR         KMX_WCHART;
typedef wchar_t*      PWSTR;
typedef WCHAR*        PWCHAR;

typedef uint8_t*      LPKMX_BYTE;
typedef uint8_t*      PKMX_BYTE;

typedef KMX_BYTE*     PKMX_BYTE;
typedef KMX_DWORD*    PKMX_DWORD;
typedef int           BOOL;

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
#define VK_COLON	  0xBA
#define VK_EQUAL	  0xBB
#define VK_COMMA	  0xBC
#define VK_HYPHEN   0xBD
#define VK_PERIOD	  0xBE
#define	VK_SLASH	  0xBF
#define VK_ACCENT	  0xC0
#define VK_LBRKT	  0xDB
#define VK_BKSLASH	0xDC
#define VK_RBRKT	  0xDD
#define VK_QUOTE	  0xDE
#define VK_xDF		  0xDF
#define VK_OEM_102  0xE2  //  "<>" or "\|" on RT 102-key kbd.

#define VK_DIVIDE   0x6F
#define VK_CANCEL   3
#define VK_DECIMAL  0x2E

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
