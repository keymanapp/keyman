#pragma once

typedef unsigned long       DWORD;
typedef int                 BOOL;
typedef unsigned char       BYTE;
typedef unsigned short      WORD;

typedef wchar_t WCHAR;    // wc,   16-bit UNICODE character
typedef _Null_terminated_ WCHAR *NWPSTR, *LPWSTR, *PWSTR;
typedef WCHAR *PWCHAR, *LPWCH, *PWCH;

typedef char CHAR;
typedef CHAR *PCHAR, *LPCH, *PCH;
typedef _Null_terminated_ CHAR *NPSTR, *LPSTR, *PSTR;

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
