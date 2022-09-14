// taken from C:\Projects\keyman\keyman\common\core\desktop\src\kmx\kmx_base.h
#pragma once
#include <stdint.h>
#include <km_types.h>

typedef int         INT_PKMX;
typedef wchar_t     KMX_WCHART;
typedef KMX_BYTE *  LPKMX_BYTE;
typedef KMX_DWORD * LPKMX_DWORD;
typedef char *      PKMX_STR;
typedef KMX_WCHAR*  LPKMX_WCHAR ;

// TODO: Windows-specific
#ifndef CALLBACK
#define CALLBACK 
#endif

typedef int (CALLBACK *CompilerMessageProc)(int line, KMX_DWORD dwMsgCode, char* szText);
extern "C" KMX_BOOL __declspec(dllexport) CompileKeyboardFile(PKMX_STR pszInfile,
    PKMX_STR pszOutfile, KMX_BOOL ASaveDebug, KMX_BOOL ACompilerWarningsAsErrors,
	KMX_BOOL AWarnDeprecatedCode, CompilerMessageProc pMsgProc) ;  // I4865   // I4866
