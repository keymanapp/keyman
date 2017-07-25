#ifndef STDAFX_H
#define STDAFX_H

#define BUILDING_DRIVER

#include <ntifs.h>
#include <stddef.h>
#include "ScopedObjects.h"
#include "DbgText.h"

__if_not_exists(HWND)
{
	DECLARE_HANDLE(HWND);
}

typedef unsigned int UINT;

static const ULONG g_dwHighestVersion = 0x06010100; // NTDDI_WIN7SP1
static const ULONG g_dwLowestVersion = NTDDI_VISTA;

// implementation of these and the globals are in globals.cpp
void* __cdecl operator new(size_t size);
void __cdecl operator delete(PVOID);
void* __cdecl operator new[](size_t size);
void __cdecl operator delete[](PVOID);

__if_not_exists(ATOM)
{
	typedef USHORT ATOM;
}

// names of the links are the same as those from the original version
extern UNICODE_STRING g_usSymName;
// current os version in same format as the NTDDI values
extern ULONG g_dwOSVersion;
// win32k/gdi resource
extern PERESOURCE* g_ppResUser;
// win32k atom array used by hooks
extern ATOM* g_pAtomArray;
// type of validatehwnd
typedef struct tagWND* (__fastcall*pfnValidateHwnd)(HWND hwnd);
extern pfnValidateHwnd g_pValidateHwnd;
// type of UserGetAtomName
typedef ULONG (NTAPI*pfnUserGetAtomName)(int atom, WCHAR* pName, int len);
extern pfnUserGetAtomName g_pUserGetAtomName;
// list of session WindowStations
extern PVOID* g_ppWinstaList;

static const ULONG g_heapTag = ' gsm';

#pragma intrinsic(memset)

#endif
