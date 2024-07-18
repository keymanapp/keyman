#pragma once

/***************************************************************************/
/* In memory keyboard information */
typedef struct tagSTORE
{
    DWORD dwSystemID;
    PWSTR dpName;
    PWSTR dpString;
} STORE, * LPSTORE;

typedef struct tagKEY
{
    WCHAR Key;
    DWORD Line;
    DWORD ShiftFlags;
    PWSTR dpOutput;
    PWSTR dpContext;
} KEY, * LPKEY;

typedef struct tagGROUP
{
    PWSTR dpName;
    LPKEY dpKeyArray;		// [LPKEY] address of first item in key array
    PWSTR dpMatch;
    PWSTR dpNoMatch;
    DWORD cxKeyArray;		// in array entries
    BOOL  fUsingKeys;		// group(xx) [using keys] <-- specified or not
} GROUP, * LPGROUP;

typedef struct tagKEYBOARD
{
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

    HBITMAP	hBitmap;		// handle to the bitmap in the file;
} KEYBOARD, * LPKEYBOARD;
