#pragma once

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

#if defined(__cplusplus)
// FROM keyboardprocessor.h, to eliminate once we merge
typedef char16_t   km_kbp_cp;
typedef char32_t   km_kbp_usv;
#else
typedef uint16_t    km_kbp_cp;          // code point
typedef uint32_t    km_kbp_usv;         // Unicode Scalar Value
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

