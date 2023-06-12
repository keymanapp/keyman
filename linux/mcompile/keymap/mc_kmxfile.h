#pragma once
#include "km_types.h"

KMX_DWORD TEST;

#ifndef _KMXFILE_H
#define _KMXFILE_H

typedef struct tagSTORE {
	KMX_DWORD dwSystemID;
	PKMX_WCHART dpName;
	PKMX_WCHART dpString;
} STORE, *LPSTORE;


typedef struct tagKEY {
	KMX_WCHAR Key;
	KMX_DWORD Line;
	KMX_DWORD ShiftFlags;
	PKMX_WCHART dpOutput;
	PKMX_WCHART dpContext;
} KEY, *LPKEY;


typedef struct tagGROUP {
	PKMX_WCHART dpName;
	LPKEY dpKeyArray;		// [LPKEY] address of first item in key array
	PKMX_WCHART dpMatch;
	PKMX_WCHART dpNoMatch;
	KMX_DWORD cxKeyArray;		// in array entries
	KMX_BOOL  fUsingKeys;		// group(xx) [using keys] <-- specified or not
} GROUP, *LPGROUP;



typedef struct tagKEYBOARD {
	KMX_DWORD dwIdentifier;		// Keyman compiled keyboard id

	KMX_DWORD dwFileVersion;	// Version of the file - Keyman 4.0 is 0x0400

	KMX_DWORD dwCheckSum;		// As stored in keyboard. DEPRECATED as of 16.0
	KMX_DWORD xxkbdlayout;    	// as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
	KMX_DWORD IsRegistered;		// layout id, from same registry key
	KMX_DWORD version;			// keyboard version

	KMX_DWORD cxStoreArray;		// in array entries
	KMX_DWORD cxGroupArray;		// in array entries

	LPSTORE dpStoreArray;	// [LPSTORE] address of first item in store array, from start of file
	LPGROUP dpGroupArray;	// [LPGROUP] address of first item in group array, from start of file

	KMX_DWORD StartGroup[2];	// index of starting groups [2 of them]
							// Ansi=0, Unicode=1

	KMX_DWORD dwFlags;			// Flags for the keyboard file

	KMX_DWORD dwHotKey;			// standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

	//PKMX_WCHART dpName;			// offset of name
	//PKMX_WCHART dpLanguageName;	// offset of language name;
	//PKMX_WCHART dpCopyright;		// offset of copyright
	//PKMX_WCHART dpMessage;		// offset of message in Keyboard About box

	KMX_DWORD dpBitmapOffset;	// 0038 offset of the bitmaps in the file
	KMX_DWORD dwBitmapSize;		// 003C size in bytes of the bitmaps
	//HBITMAP	hBitmap;		// handle to the bitmap in the file;
} KEYBOARD, *LPKEYBOARD;

KMX_BOOL LoadKeyboard(LPKMX_WCHART fileName, LPKEYBOARD *lpKeyboard);		// _S2 LPKEYBOARD ok to leave as is??

#endif






//---------------------old----------------------------------------
/*
#include <Windows.h>

#ifndef _KMXFILE_H
#define _KMXFILE_H

typedef struct tagSTORE {
	DWORD dwSystemID;
	PWSTR dpName;
	PWSTR dpString;
} STORE, *LPSTORE;

typedef struct tagKEY {
	WCHAR Key;
	DWORD Line;
	DWORD ShiftFlags;
	PWSTR dpOutput;
	PWSTR dpContext;
} KEY, *LPKEY;


typedef struct tagGROUP {
	PWSTR dpName;
	LPKEY dpKeyArray;		// [LPKEY] address of first item in key array
	PWSTR dpMatch;
	PWSTR dpNoMatch;
	DWORD cxKeyArray;		// in array entries
	BOOL  fUsingKeys;		// group(xx) [using keys] <-- specified or not
} GROUP, *LPGROUP;


typedef struct tagKEYBOARD {
	DWORD dwIdentifier;		// Keyman compiled keyboard id

	DWORD dwFileVersion;	// Version of the file - Keyman 4.0 is 0x0400

	DWORD dwCheckSum;		// As stored in keyboard. DEPRECATED as of 16.0
	DWORD xxkbdlayout;    	// as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
	DWORD IsRegistered;		// layout id, from same registry key
	DWORD version;			// keyboard version

	DWORD cxStoreArray;		// in array entries
	DWORD cxGroupArray;		// in array entries

	LPSTORE dpStoreArray;	// [LPSTORE] address of first item in store array, from start of file
	LPGROUP dpGroupArray;	// [LPGROUP] address of first item in group array, from start of file

	DWORD StartGroup[2];	// index of starting groups [2 of them]
							// Ansi=0, Unicode=1

	DWORD dwFlags;			// Flags for the keyboard file

	DWORD dwHotKey;			// standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

	//PWSTR dpName;			// offset of name
	//PWSTR dpLanguageName;	// offset of language name;
	//PWSTR dpCopyright;		// offset of copyright
	//PWSTR dpMessage;		// offset of message in Keyboard About box

	DWORD dpBitmapOffset;	// 0038 offset of the bitmaps in the file
	DWORD dwBitmapSize;		// 003C size in bytes of the bitmaps
	//HBITMAP	hBitmap;		// handle to the bitmap in the file;
} KEYBOARD, *LPKEYBOARD;

BOOL LoadKeyboard(LPWSTR fileName, LPKEYBOARD *lpKeyboard);

#endif
*/

