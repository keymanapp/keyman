
#ifndef	_COMPV4_H
#define _COMPV4_H

#define V4_FILEID_COMPILED	0x5354584B

#define V4_SZMAX_LANGUAGENAME 80
#define V4_SZMAX_KEYBOARDNAME 80
#define V4_SZMAX_COPYRIGHT 256
#define V4_SZMAX_MESSAGE 1024

#define V4_CODE_ANY			0x01
#define V4_CODE_INDEX		0x02
#define V4_CODE_CONTEXT		0x03
#define V4_CODE_NUL			0x04
#define V4_CODE_USE			0x05
#define V4_CODE_RETURN		0x06
#define V4_CODE_BEEP		0x07
#define V4_CODE_DEADKEY		0x08
//	0x09 = bkspace.
#define V4_CODE_EXTENDED	0x0A
#define V4_CODE_EXTENDEDEND	0x0B
#define V4_CODE_SWITCH		0x0C
#define V4_CODE_KEY			0x0D
#define	V4_CODE_CLEARCONTEXT 0x0E

#define V4_CODE_LASTCODE	0x0E

#define V4_KF_SHIFTFREESCAPS	0x0001
#define V4_KF_CAPSONONLY		0x0002
#define V4_KF_CAPSALWAYSOFF		0x0004

#define V4_HK_ALT			0x00010000
#define V4_HK_CTRL			0x00020000
#define V4_HK_SHIFT			0x00040000

#define V4_LCTRLFLAG		0x0001		// Left Control flag
#define V4_RCTRLFLAG		0x0002		// Right Control flag
#define V4_LALTFLAG			0x0004		// Left Alt flag
#define V4_RALTFLAG			0x0008		// Right Alt flag
#define V4_K_SHIFTFLAG		0x0010		// Either shift flag
#define V4_K_CTRLFLAG		0x0020		// Either ctrl flag
#define V4_K_ALTFLAG		0x0040		// Either alt flag
#define V4_CAPITALFLAG		0x0100		// Caps lock on
#define V4_NOTCAPITALFLAG	0x0200		// Caps lock NOT on
#define V4_NUMLOCKFLAG		0x0400		// Num lock on
#define V4_NOTNUMLOCKFLAG	0x0800		// Num lock NOT on
#define V4_SCROLLFLAG		0x1000		// Scroll lock on
#define V4_NOTSCROLLFLAG	0x2000		// Scroll lock NOT on
#define V4_ISVIRTUALKEY		0x4000		// It is a Virtual Key Sequence


struct V4_STORE {
	LPBYTE dpString;		// from start of store structure
	};

typedef V4_STORE *LPV4_STORE;

struct V4_KEY {
	BYTE Key;
	WORD ShiftFlags;
	LPBYTE dpOutput;		// from start of key structure
	LPBYTE dpContext;		// from start of key structure
	};

typedef V4_KEY *LPV4_KEY;

struct V4_GROUP {
	LPV4_KEY dpKeyArray;	// [LPKEY] address of first item in key array, from start of group structure
	LPBYTE dpMatch;			// from start of group structure
	LPBYTE dpNoMatch;		// from start of group structure
	DWORD cxKeyArray;		// in array entries
	BOOL  fUsingKeys;		// group(xx) [using keys] <-- specified or not
	};

typedef V4_GROUP *LPV4_GROUP;

struct V4_KEYBOARD {
	DWORD dwIdentifier;		// Keyman compiled keyboard id

	DWORD dwFileVersion;	// Version of the file - Keyman 4.0 is 0x0400
	
	DWORD dwCheckSum;		// As stored in keyboard
	DWORD kbdlayout;    	// as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
	DWORD layoutId;			// layout id;
	DWORD version;			// keyboard version

	DWORD cxStoreArray;		// in array entries
	DWORD cxGroupArray;		// in array entries

	LPV4_STORE dpStoreArray;	// [LPSTORE] address of first item in store array, from start of store structure
	LPV4_GROUP dpGroupArray;	// [LPGROUP] address of first item in group array, from start of group structure
	
	DWORD StartGroup[2];	// index of starting groups [2 of them]
	DWORD StartGroupIndex;	// StartGroup current index

	DWORD dwFlags;			// Flags for the keyboard file

	DWORD dwHotKey;			// standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

	LPBYTE dpName;			// offset of name
	LPBYTE dpLanguageName;	// offset of language name;
	LPBYTE dpCopyright;		// offset of copyright
	LPBYTE dpMessage;		// offset of message in Keyboard About box

	HBITMAP	hBitmap;		// handle to the bitmap in the file;
	};

typedef V4_KEYBOARD *LPV4_KEYBOARD;


struct V4_COMP_STORE {
	DWORD dpString;			// from start of store structure
	};

struct V4_COMP_KEY {
	BYTE Key;
	WORD ShiftFlags;
	DWORD dpOutput;			// from start of key structure
	DWORD dpContext;		// from start of key structure
	};

struct V4_COMP_GROUP {
	DWORD dpKeyArray;		// [LPKEY] address of first item in key array, from start of group structure
	DWORD dpMatch;			// from start of group structure
	DWORD dpNoMatch;		// from start of group structure
	DWORD cxKeyArray;		// in array entries
	BOOL  fUsingKeys;		// group(xx) [using keys] <-- specified or not
	};

struct V4_COMP_KEYBOARD {
	DWORD dwIdentifier;		// Keyman compiled keyboard id
	
	DWORD dwFileVersion;	// Version of the file - Keyman 4.0 is 0x0400
	
	DWORD dwCheckSum;		// As stored in keyboard
	DWORD kbdlayout;    	// as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
	DWORD layoutId;			// layout id
	DWORD version;			// keyboard version
	
	DWORD cxStoreArray;		// in array entries
	DWORD cxGroupArray;		// in array entries

	DWORD dpStoreArray;		// [LPSTORE] address of first item in store array, from start of store structure
	DWORD dpGroupArray;		// [LPGROUP] address of first item in group array, from start of group structure
	
	DWORD StartGroup[2];	// index of starting groups [2 of them]
	DWORD StartGroupIndex;	// StartGroup current index

	DWORD dwFlags;			// Flags for the keyboard file

	DWORD dwHotKey;			// standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

	DWORD dpName;			// offset of name
	DWORD dpLanguageName;	// offset of language name;
	DWORD dpCopyright;		// offset of copyright
	DWORD dpMessage;		// offset of message in Keyboard About box

	DWORD dpBitmapOffset;	// offset of the bitmaps in the file
	DWORD dwBitmapSize;		// size in bytes of the bitmaps
	};

typedef V4_COMP_KEYBOARD *PV4_COMP_KEYBOARD;
typedef V4_COMP_STORE *PV4_COMP_STORE;
typedef V4_COMP_KEY *PV4_COMP_KEY;
typedef V4_COMP_GROUP *PV4_COMP_GROUP;

#endif		// _COMPV4_H

