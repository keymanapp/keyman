/*
  Name:             keymanapi
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Nov 2009

  Modified Date:    16 Oct 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          30 Nov 2009 - mcdurdin - I934 - Prep for x64 - deprecate .def use, use dllexport
                    16 Oct 2014 - mcdurdin - I4462 - V9.0 - Keyman had a mismatch between KEYBOARDINFO and INTKEYBOARDINFO
*/
// Header file for Keyman API
// 5.0.107.0: nIMDLLs and IMDLLs added to KEYBOARDINFO.  Earlier versions of Keyman should not be used.
// 6.0.153.0: Bug in API: fIsPositionalLayout defined in KEYBOARDINFO, breaking apps using V5 functions
// 6.0.154.0: fIsPositionalLayout removed from KEYBOARDINFO.
//            New function Keyman_GetAPIVersion: if it doesn't exist, assume V5.
// 6.0.155.0: New function Keyman_ActivateKeyboardName
//            New function Keyman_BuildKeyboardListEx(LPKEYBOARDINFOEX); 
//            New function Keyman_GetSystemStore(LPKEYBOARDINFOEX kbd, DWORD SystemStoreID, PWSTR *buf);
//            KEYBOARDINFOEX will have DWORD cbSize at start of structure

#ifndef _KEYMANAPI_H
#define _KEYMANAPI_H

// The members of this structure, from first through to IMDLLs, must match INTKEYBOARDINFO from keyman64.h   // I4462
typedef struct tagKEYBOARDINFO
{
	DWORD      KeymanID;
	DWORD      HotKey;
	DWORD      KeyboardID;
	char       Name[256];
	LPKEYBOARD Keyboard;
	DWORD      nIMDLLs;
	LPIMDLL    IMDLLs;
} KEYBOARDINFO, *LPKEYBOARDINFO;

extern "C" BOOL  _declspec(dllexport) WINAPI Keyman_BuildKeyboardList(LPKEYBOARDINFO kbd, int *n);
extern "C" HWND  _declspec(dllexport) WINAPI Keyman_GetLastActiveWindow();
extern "C" HWND  _declspec(dllexport) WINAPI Keyman_GetLastFocusWindow();

// Implemented in 6.0.154.0 and later

#define KEYMAN_API_V6	0x0600

extern "C" DWORD  _declspec(dllexport) WINAPI Keyman_GetAPIVersion();

// Implemented in 6.0.155.0 and later

/*
extern "C" DWORD WINAPI Keyman_ActivateKeyboardName(LPCSTR kbdname); -- not yet implemented
extern "C" BOOL WINAPI Keyman_BuildKeyboardListEx(LPKEYBOARDINFOEX kbd, int *n);
extern "C" BOOL WINAPI Keyman_GetSystemStore(LPKEYBOARDINFOEX kbd, DWORD SystemStoreID, PWSTR *buf);

typedef struct tagLPKEYBOARDINFOEX
{
    DWORD    cbSize;
	DWORD    KeymanID;
	DWORD    HotKey;
	DWORD    KeyboardID;
	char     Name[256];
	char *   pInternalData; {Actual type LPKEYBOARD}
} KEYBOARDINFOEX, *LPKEYBOARDINFOEX;
*/

#endif
