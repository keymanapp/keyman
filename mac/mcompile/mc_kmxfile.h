#pragma once
#ifndef MC_KMXFILE_H
#define MC_KMXFILE_H

#include "../../common/include/km_types.h"
#include "../../common/include/kmx_file.h"
#include "mcompile.h"

#ifndef _KMXFILE_H
#define _KMXFILE_H

typedef struct KMX_tagSTORE {
	KMX_DWORD dwSystemID;
	PKMX_WCHAR dpName;
	PKMX_WCHAR dpString;
} KMX_STORE, *LPKMX_STORE;

typedef struct KMX_tagKEY {
	KMX_WCHAR Key;
	KMX_DWORD Line;
	KMX_DWORD ShiftFlags;
	PKMX_WCHAR dpOutput;
	PKMX_WCHAR dpContext;
} KMX_KEY, *LPKMX_KEY;

typedef struct KMX_tagGROUP {
	KMX_WCHAR* dpName;
	LPKMX_KEY dpKeyArray;  // [LPKEY] address of first item in key array
	PKMX_WCHAR dpMatch;
	PKMX_WCHAR dpNoMatch;
	KMX_DWORD cxKeyArray;  // in array entries
	int32_t fUsingKeys;    // group(xx) [using keys] <-- specified or not
} KMX_GROUP, *LPKMX_GROUP;

typedef struct KMX_tagKEYBOARD {
	KMX_DWORD dwIdentifier;  // Keyman compiled keyboard id

	KMX_DWORD dwFileVersion;  // Version of the file - Keyman 4.0 is 0x0400

	KMX_DWORD dwCheckSum;    // As stored in keyboard. DEPRECATED as of 16.0
	KMX_DWORD xxkbdlayout;   // as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
	KMX_DWORD IsRegistered;  // layout id, from same registry key
	KMX_DWORD version;       // keyboard version

	KMX_DWORD cxStoreArray;  // in array entries
	KMX_DWORD cxGroupArray;  // in array entries

	LPKMX_STORE dpStoreArray;  // [LPSTORE] address of first item in store array, from start of file
	LPKMX_GROUP dpGroupArray;  // [LPGROUP] address of first item in group array, from start of file

	KMX_DWORD StartGroup[2];  // index of starting groups [2 of them]
														// Ansi=0, Unicode=1

	KMX_DWORD dwFlags;  // Flags for the keyboard file

	KMX_DWORD dwHotKey;  // standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

	// PWSTR dpName;						// offset of name
	// PWSTR dpLanguageName;		// offset of language name;
	// PWSTR dpCopyright;			// offset of copyright
	// PWSTR dpMessage;				// offset of message in Keyboard About box

	KMX_DWORD dpBitmapOffset;  // 0038 offset of the bitmaps in the file
	KMX_DWORD dwBitmapSize;    // 003C size in bytes of the bitmaps
	//HBITMAP	hBitmap;				// handle to the bitmap in the file;
} KMX_KEYBOARD, *LPKMX_KEYBOARD;

/** @brief load a keyboard kmx-file */
KMX_BOOL KMX_LoadKeyboard(KMX_CHAR* fileName, LPKMX_KEYBOARD* lpKeyboard);

/** @brief save keyboard to file */
KMX_BOOL KMX_SaveKeyboard(LPKMX_KEYBOARD kbd, KMX_CHAR* fileName);

/** @brief increment in a string */
PKMX_WCHAR KMX_incxstr(PKMX_WCHAR p);

/** @brief open a file */
FILE* Open_File(const KMX_CHAR* Filename, const KMX_CHAR* mode);

#endif  // _KMXFILE_H

#endif /*MC_KMXFILE_H*/
