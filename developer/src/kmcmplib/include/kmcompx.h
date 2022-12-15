#pragma once
#include <stdint.h>
#include <km_types.h>

typedef int         KMX_INT;
typedef wchar_t     KMX_WCHART;
typedef KMX_DWORD * LPKMX_DWORD;
typedef char *      PKMX_STR;
typedef KMX_WCHAR*  LPKMX_WCHAR ;

// TODO: Windows-specific
#ifndef CALLBACK
#define CALLBACK 
#endif

typedef int (CALLBACK *CompilerMessageProc)(int line, KMX_DWORD dwMsgCode, char* szText);
extern "C" KMX_BOOL __declspec(dllexport) kmcmp_CompileKeyboardFile(PKMX_STR pszInfile,
    PKMX_STR pszOutfile, KMX_BOOL ASaveDebug, KMX_BOOL ACompilerWarningsAsErrors,
	KMX_BOOL AWarnDeprecatedCode, CompilerMessageProc pMsgProc) ;  // I4865   // I4866
