#pragma once

typedef uint32_t            DWORD;
typedef int32_t             BOOL;
typedef uint8_t             BYTE;
typedef uint16_t            WORD;


#if defined(__cplusplus)
typedef char16_t   km_kbp_cp;
typedef char32_t   km_kbp_usv;
#else
typedef uint16_t    km_kbp_cp;          // code point
typedef uint32_t    km_kbp_usv;         // Unicode Scalar Value
#endif

typedef km_kbp_cp WCHAR;    // wc,   16-bit UNICODE character
typedef WCHAR *LPWSTR, *PWSTR;
typedef WCHAR *PWCHAR;

typedef char CHAR;
typedef CHAR *PCHAR;
typedef CHAR *LPSTR, *PSTR;

#define far
#define near

typedef uint32_t             UINT;

typedef BYTE near           *PBYTE;
typedef BYTE far            *LPBYTE;
typedef WORD near           *PWORD;
typedef WORD far            *LPWORD;
typedef DWORD near          *PDWORD;
typedef DWORD far           *LPDWORD;


#ifndef FALSE
#define FALSE               0
#endif

#ifndef TRUE
#define TRUE                1
#endif

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
