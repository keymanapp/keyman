/*
  Name:             Km95api
  Copyright:        Copyright (C) SIL International.
*/

#ifndef _KM95API_H
#define _KM95API_H

extern "C" {

//#define HKL_LOCALE(hkl) ((DWORD)LOWORD(hkl))
//#define HKL_LAYOUT(hkl) (HIWORD(hkl) & 0xF000 ? ((DWORD)(hkl) & 0x0FFFFFFFL) : (DWORD)(LOWORD(hkl)))
//#define HKL_SUBST(hkl) (HIWORD(hkl) & 0xF000 ? ((DWORD)(hkl) & 0x0FFFFFFFL) : (DWORD)(HIWORD(hkl)))
//DWORD WINAPI HKL_LAYOUT( HKL hkl );

#define RWM_KMMESSAGE	 "WM_KMMESSAGE"

#define RWM_KEYMAN "wm_keyman"

// wParam for wm_keyman
// These messages should be posted to a window
#define KM_DISABLEUI	1
#define KM_ENABLEUI		2
// These messages should be sent to a window
//#define KM_GETUISTATE	3
#define KM_FOCUSCHANGED	5		// Never use this flag: internal to Keyman
#define KM_ACTIVECHANGED 6  // Never use this flag: internal to Keyman
#define KM_EXITFLUSH  8 // Disconnects GetMessage hook 

#define KMF_WINDOWCHANGED 1

typedef struct tagKEYMANKEYBOARDINFO {
	DWORD	cbSize;
	DWORD	KeymanID;
	DWORD	layout; // Deprecated: always 0 with Keyman 9   // I3613
	char	szName[80];
	char	szFileName[256];
	char	szMessage[1024];
	char	szCopyright[256];
	HBITMAP hBitmap;
	int		bcx, bcy;
	DWORD	hotkey;
	} KEYMANKEYBOARDINFO, FAR *LPKEYMANKEYBOARDINFO;

typedef struct tagKEYMANINFO {
	DWORD cbSize;
	} KEYMANINFO, FAR *LPKEYMANINFO;

  BOOL  _declspec(dllexport) WINAPI GetKeymanKeyboardInfo( LPKEYMANKEYBOARDINFO kki );
  BOOL  _declspec(dllexport) WINAPI GetKeymanInfo( LPKEYMANINFO ki );
}

#endif	// _KM95API_H
