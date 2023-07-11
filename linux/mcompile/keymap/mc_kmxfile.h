#pragma once
#ifndef MC_KMXFILE_H
#define MC_KMXFILE_H

#include "km_types.h"
#include "kmx_file.h"
#include "filesystem.h"
#include "mcompile.h"

#include <iostream>		// _S2 can be removed later

int dummytest_mc_kmx_file();
//+++++++++++++++++++++++++++++++++++

#ifndef _KMXFILE_H
#define _KMXFILE_H

typedef struct tagSTORE {
	DWORD dwSystemID;
	PWSTR dpName;
	PWSTR dpString;
} STORE, *LPSTORE;

						typedef struct KMX_tagSTORE {
							KMX_DWORD dwSystemID;
							PKMX_WCHAR dpName;
							PKMX_WCHAR dpString;
						} KMX_STORE, *LPKMX_STORE;


typedef struct tagKEY {
	WCHAR Key;
	DWORD Line;
	DWORD ShiftFlags;
	PWSTR dpOutput;
	PWSTR dpContext;
} KEY, *LPKEY;

					typedef struct KMX_tagKEY {
						KMX_WCHAR Key;
						KMX_DWORD Line;
						KMX_DWORD ShiftFlags;
						PKMX_WCHAR dpOutput;
						PKMX_WCHAR dpContext;
					} KMX_KEY, *LPKMX_KEY;


typedef struct tagGROUP {
	PWSTR dpName;
	LPKEY dpKeyArray;		// [LPKEY] address of first item in key array
	PWSTR dpMatch;
	PWSTR dpNoMatch;
	DWORD cxKeyArray;		// in array entries
	int  fUsingKeys;		// group(xx) [using keys] <-- specified or not
} GROUP, *LPGROUP;

					typedef struct KMX_tagGROUP {
						KMX_WCHAR* dpName;
						LPKMX_KEY dpKeyArray;		// [LPKEY] address of first item in key array
						PKMX_WCHAR dpMatch;
						PKMX_WCHAR dpNoMatch;
						KMX_DWORD cxKeyArray;		// in array entries  // _S2 was DWORD
						int32_t  fUsingKeys;		// group(xx) [using keys] <-- specified or not
					} KMX_GROUP, *LPKMX_GROUP;




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

									typedef struct KMX_tagKEYBOARD {
										KMX_DWORD dwIdentifier;		// Keyman compiled keyboard id

										KMX_DWORD dwFileVersion;	// Version of the file - Keyman 4.0 is 0x0400

										KMX_DWORD dwCheckSum;		// As stored in keyboard. DEPRECATED as of 16.0
										KMX_DWORD xxkbdlayout;    	// as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
										KMX_DWORD IsRegistered;		// layout id, from same registry key
										KMX_DWORD version;			// keyboard version

										KMX_DWORD cxStoreArray;		// in array entries
										KMX_DWORD cxGroupArray;		// in array entries

										LPKMX_STORE dpStoreArray;	// [LPSTORE] address of first item in store array, from start of file
										LPKMX_GROUP dpGroupArray;	// [LPGROUP] address of first item in group array, from start of file

										KMX_DWORD StartGroup[2];	// index of starting groups [2 of them]
																// Ansi=0, Unicode=1

										KMX_DWORD dwFlags;			// Flags for the keyboard file

										KMX_DWORD dwHotKey;			// standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

										//PWSTR dpName;			// offset of name
										//PWSTR dpLanguageName;	// offset of language name;
										//PWSTR dpCopyright;		// offset of copyright
										//PWSTR dpMessage;		// offset of message in Keyboard About box

										KMX_DWORD dpBitmapOffset;	// 0038 offset of the bitmaps in the file
										KMX_DWORD dwBitmapSize;		// 003C size in bytes of the bitmaps
										//HBITMAP	hBitmap;		// handle to the bitmap in the file;
									} KMX_KEYBOARD, *LPKMX_KEYBOARD;

BOOL LoadKeyboard(char* fileName, LPKEYBOARD *lpKeyboard);		// _S2 LPKEYBOARD ok to leave as is??
BOOL LoadKeyboard(wchar_t* fileName, LPKEYBOARD *lpKeyboard);		// _S2 LPKEYBOARD ok to leave as is??
BOOL LoadKeyboard(char16_t* fileName, LPKEYBOARD *lpKeyboard);		// _S2 LPKEYBOARD ok to leave as is??
		KMX_BOOL KMX_LoadKeyboard(char* fileName, LPKMX_KEYBOARD *lpKeyboard);		// _S2 LPKEYBOARD ok to leave as is??
		KMX_BOOL KMX_LoadKeyboard(wchar_t* fileName, LPKMX_KEYBOARD *lpKeyboard);		// _S2 LPKEYBOARD ok to leave as is??
		KMX_BOOL KMX_LoadKeyboard(char16_t* fileName, LPKMX_KEYBOARD *lpKeyboard);		// _S2 LPKEYBOARD ok to leave as is??


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


#endif /*MC_KMXFILE_H*/
