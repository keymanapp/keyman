

// taken from C:\Projects\keyman\keyman\common\core\desktop\src\kmx\kmx_base.h
#pragma once
#include <stdint.h>
#include <km_types.h>

// typedef:old ->new
typedef int                 INT_PKMX;
typedef wchar_t             KMX_WCHART;    // wc,   16-bit UNICODE character-> wchar_t
typedef KMX_WCHAR *  PKMX_WSTR;      // n win: typedef WCHAR *PWSTR;
typedef KMX_WCHART * PKMX_WCHART;      // n win: typedef WCHAR *PWSTR; // s ??
typedef KMX_BYTE *  LPKMX_BYTE;        // never used?
typedef KMX_WORD *  LPKMX_WORD;        // KMX_WORD  * never used?  (KMX_WORD  is used)
typedef KMX_DWORD * LPKMX_DWORD;       // KMX_DWORD * never used?  (KMX_DWORD is used)
typedef char *       PKMX_STR;
typedef KMX_WCHAR* LPKMX_WCHAR;      // s ??

// TODO: Windows-specific
#ifndef CALLBACK
#define CALLBACK 
#endif

typedef int (CALLBACK *CompilerMessageProc)(int line, KMX_DWORD dwMsgCode, char* szText);
extern "C" KMX_BOOL __declspec(dllexport) CompileKeyboardFile(PKMX_STR pszInfile,
    PKMX_STR pszOutfile, KMX_BOOL ASaveDebug, KMX_BOOL ACompilerWarningsAsErrors,
	KMX_BOOL AWarnDeprecatedCode, CompilerMessageProc pMsgProc) ;  // I4865   // I4866

