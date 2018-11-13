#pragma once

typedef unsigned long       DWORD;
typedef int                 BOOL;
typedef unsigned char       BYTE;
typedef unsigned short      WORD;


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

typedef unsigned int        UINT;

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
