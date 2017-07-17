#ifndef DBGHELPPROTOS_H
#define DBGHELPPROTOS_H

#pragma once

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

#include <windows.h>
#include <dbghelp.h>

// dbghelp.dll

// unicode agnostic functions
// StackWalk64
typedef BOOL (WINAPI*SymWalkStack)(UINT, HANDLE, HANDLE, STACKFRAME64*, CONTEXT*, 
		PREAD_PROCESS_MEMORY_ROUTINE64, PFUNCTION_TABLE_ACCESS_ROUTINE64,
		PGET_MODULE_BASE_ROUTINE64, PTRANSLATE_ADDRESS_ROUTINE64);
// SymSetOptions
typedef DWORD (WINAPI*SetSymOptions)(DWORD);
// SymGetOptions
typedef DWORD (WINAPI*GetSymOptions)(void);
// ImageHlpApiVersionEx
typedef API_VERSION* (WINAPI*SymSetApiVer)(API_VERSION*);
// SymCleanup
typedef BOOL(WINAPI*SymClose)(HANDLE);
// SymFunctionTableAccess64
typedef PVOID (WINAPI*SymFuncTable)(HANDLE, DWORD64);
// SymRegisterCallbackProc64
typedef BOOL (WINAPI*SymReg)(HANDLE, PSYMBOL_REGISTERED_CALLBACK64, ULONG64);
// SymGetSearchPathW
typedef BOOL (WINAPI*SymGetSymPathW)(HANDLE, PWSTR, ULONG);
// SymGetSearchPathW
typedef BOOL (WINAPI*SymSetSymPathW)(HANDLE, PCWSTR);
// SymGetSearchPath
typedef BOOL (WINAPI*SymGetSymPath)(HANDLE, PSTR, ULONG);
// SymGetSearchPath
typedef BOOL (WINAPI*SymSetSymPath)(HANDLE, PCSTR);
// SymLoadModuleExW
typedef DWORD64 (WINAPI*SymLoadModW)(HANDLE, HANDLE, PCWSTR, PCWSTR, DWORD64, DWORD, PMODLOAD_DATA, DWORD);
// SymFromAddrW
typedef BOOL (WINAPI*SymAtAddrW)(HANDLE, DWORD64, PDWORD64, PSYMBOL_INFOW);
// SymFromNameW
typedef BOOL (WINAPI*SymAtNameW)(HANDLE, LPCWSTR, PSYMBOL_INFOW);
// UndecorateSymbolNameW
typedef BOOL (WINAPI*SymUndecoratorW)(PCWCH, PWCH, UINT, UINT);
// SymGetModuleInfo64W
typedef BOOL (WINAPI*SymModuleInfoW)(HANDLE, DWORD64, PIMAGEHLP_MODULEW64);
// SymInitializeW
typedef BOOL (WINAPI*SymInitW)(HANDLE, LPCWSTR, BOOL);
// SymInitialize
typedef BOOL (WINAPI*SymInit)(HANDLE, LPCSTR, BOOL);
// SymGetModuleBaseW64
typedef DWORD64 (WINAPI*SymModBaseW)(HANDLE, DWORD64);
// SymGetModuleInfoW64
typedef BOOL (WINAPI*SymModuleInfoW)(HANDLE, DWORD64, PIMAGEHLP_MODULEW64);
// SymGetLineFromAddrW64
typedef BOOL (WINAPI*SymLineW)(HANDLE, DWORD64, PDWORD, PIMAGEHLP_LINEW64);
// SymGetLineFromAddr64
typedef BOOL (WINAPI*SymLine)(HANDLE, DWORD64, PDWORD, PIMAGEHLP_LINE64);
// SymLoadModuleExW
typedef DWORD64 (WINAPI*SymLoadModW)(HANDLE, HANDLE, PCWSTR, PCWSTR, DWORD64, DWORD, PMODLOAD_DATA, DWORD);
// SymFromAddr
typedef BOOL (WINAPI*SymAtAddr)(HANDLE, DWORD64, PDWORD64, PSYMBOL_INFO);
// UndecorateSymbolName
typedef BOOL (WINAPI*SymUndecorator)(PCCH, PCH, UINT, UINT);
// SymGetModuleBase64
typedef BOOL (WINAPI*SymModuleInfo)(HANDLE, DWORD64, PIMAGEHLP_MODULE64);
// SymGetModuleBase64
typedef DWORD64 (WINAPI*SymModBase)(HANDLE, DWORD64);
// SymGetModuleInfo64
typedef BOOL (WINAPI*SymModuleInfo)(HANDLE, DWORD64, PIMAGEHLP_MODULE64);
// SymEnumSymbols
typedef BOOL (WINAPI*SymEnumW)(HANDLE, ULONG64, PCWSTR, PSYM_ENUMERATESYMBOLS_CALLBACKW, const PVOID);
// SymAddSymbolW
typedef BOOL (WINAPI*SymAddSymW)(HANDLE, ULONG64, PCWSTR, DWORD64, DWORD, DWORD);
// SymDeleteSymbolW
typedef BOOL (WINAPI*SymDelSymW)(HANDLE, ULONG64, PCWSTR, DWORD64, DWORD);

#endif
