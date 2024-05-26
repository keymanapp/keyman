// TODO: merge and replace with kmx_file.h from core

/*
  Name:             legacy_kmx_file
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:      Describes .kmx binary format. To be replaced with common/include/kmx_file.h
  Create Date:      4 Jan 2007

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          04 Jan 2007 - mcdurdin - Add CODE_NOTANY
                    22 Mar 2010 - mcdurdin - Compiler tidyup
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    19 Mar 2014 - mcdurdin - I4140 - V9.0 - Add keyboard version information to keyboards
                    16 Jun 2014 - mcdurdin - I4271 - V9.0 - Switch language for all applications is not working
                    31 Dec 2014 - mcdurdin - I4548 - V9.0 - When Alt is down, release of Ctrl, Shift is not detectable within TIP in some languages
                    24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile

*/

#ifndef	_COMPILER_H
#define _COMPILER_H



/* WM_UNICHAR */

#define WM_UNICHAR		0x0109
#define UNICODE_NOCHAR	0xFFFF

/* */

#define KEYMAN_LAYOUT_DEFAULT	0x000005FE

#define KEYMANID_NONKEYMAN	0xFFFFFFFF
#define KEYMANID_IGNORE		  0xFFFFFFFE
#define KEYMANID_INVALID    0xFFFFFFFD

/* Shift flags for hotkeys (version 1.0) */

#define SHIFTFLAG 0x2000
#define CTRLFLAG 0x4000
#define	ALTFLAG 0x8000

/* Miscellaneous flags and defines */

#define UM_DRAWICONS	0x01
#define NUL '\0'

#define MAXGROUPS	128

/* File version identifiers */

#define VERSION_30	0x00000300
#define VERSION_31	0x00000301
#define VERSION_32	0x00000302
#define VERSION_40	0x00000400
#define VERSION_50	0x00000500
#define VERSION_501	0x00000501
#define VERSION_60	0x00000600
#define VERSION_70	0x00000700
#define VERSION_80  0x00000800
#define VERSION_90  0x00000900
#define VERSION_100 0x00000A00
#define VERSION_140 0x00000E00
#define VERSION_150 0x00000F00
#define VERSION_160 0x00001000
#define VERSION_170 0x00001100
#define VERSION_MIN	VERSION_50
#define VERSION_MAX	VERSION_170

/*
 Special flag for WM_CHAR/WM_KEY???/WM_SYSKEY???: says that key has been
 processed by Keyman.
*/

//#define KEYMAN_CHARFLAG 0x0000000L
#define KEYMAN_CHARFLAG   0x02000000L

#define CHAR_TRANSTATE    0x00000001L			// Flag for WM_CHAR: key is down, first repeat
#define KEYUP_TRANSTATE	  0xC0000001L			// Flag for WM_KEYUP: key is up, first repeat
#define KEYDOWN_TRANSTATE 0x00000001L			// Flag for WM_KEYDOWN: key is down, first rpt
#define ALT_TRANSTATE     0x20000000L			// Flag for WM_KEYBOARD messages: alt is down

#define IDM_DISABLEKEY	0xFF00

#define WINDOWS_VERSION_3_1		0x030A
#define WINDOWS_VERSION_3_11	0x030B
#define WINDOWS_VERSION_4_0		0x035F
#define WINDOWS_VERSION_95		0x035F

#define HKLM HKEY_LOCAL_MACHINE
#define HKCU HKEY_CURRENT_USER

//
// DEBUGINFO states
//
#define KDS_KEYBOARD	0x0001
#define KDS_PROGRAM		0x0002
#define KDS_MESSAGE		0x0004
#define KDS_INTERNAT	0x0008

#define KDS_CONTROL		0x8000

//
// Backspace flags
//

// Delete a deadkey from context, do not pass on to app
#define BK_DEFAULT    0
#define BK_DEADKEY		1

// User pressed backspace so clear deadkeys either side of next deleted character
#define BK_BACKSPACE	2

// Next character to delete is a Unicode surrogate pair
#define BK_SURROGATE  4

/*
 A blank key (in a group without "using keys") cannot be '0' as that is
 used for error testing and blanking out unused keys and you don't really
 want that tested!
*/

#define BLANKKEY				0xFF		// Blank key

// Different begin types
#define BEGIN_ANSI		0
#define BEGIN_UNICODE	1
#define BEGIN_NEWCONTEXT 2
#define BEGIN_POSTKEYSTROKE 3

//#define lpuch		(LPBYTE)

#define TSS_NONE				0
#define TSS_BITMAP				1
#define TSS_COPYRIGHT			2
#define TSS_HOTKEY				3
#define TSS_LANGUAGE			4
#define TSS_LAYOUT				5
#define TSS_MESSAGE				6
#define TSS_NAME				7
#define TSS_VERSION				8
#define TSS_CAPSONONLY			9
#define TSS_CAPSALWAYSOFF		10
#define TSS_SHIFTFREESCAPS		11
#define TSS_LANGUAGENAME		12

#define TSS_CALLDEFINITION		13
#define TSS_CALLDEFINITION_LOADFAILED	14

#define TSS_ETHNOLOGUECODE		15

#define	TSS_DEBUG_LINE			16

#define TSS_MNEMONIC			17

#define TSS_INCLUDECODES		18

#define TSS_OLDCHARPOSMATCHING	19

#define TSS_COMPILEDVERSION		20
#define TSS_KEYMANCOPYRIGHT		21

#define TSS_CUSTOMKEYMANEDITION		22
#define TSS_CUSTOMKEYMANEDITIONNAME	23

/* Keyman 7.0 system stores */

#define TSS__KEYMAN_60_MAX		23

#define TSS_VISUALKEYBOARD	24
#define TSS_KMW_RTL			25
#define TSS_KMW_HELPFILE	26
#define TSS_KMW_HELPTEXT	27
#define TSS_KMW_EMBEDJS		28

#define TSS_WINDOWSLANGUAGES 29

#define TSS__KEYMAN_70_MAX  29

/* Keyman 8.0 system stores */

#define TSS_COMPARISON 30

/* Keyman 9.0 system stores */

#define TSS_PLATFORM    31
#define TSS_BASELAYOUT  32
#define TSS_LAYER       33

#define TSS_PLATFORM_NOMATCH  0x8001  // Reserved for internal use - after platform statement is run, set to either TSS_PLATFORM_NOMATCH or TSS_PLATFORM_MATCH
#define TSS_PLATFORM_MATCH    0x8002  // Reserved for internal use - as the result will never change for the lifetime of the process.

#define TSS_VKDICTIONARY     34      // Dictionary of virtual key names for v9 dynamic layouts
#define TSS_LAYOUTFILE       35      // Keyman 9 layer-based JSON OSK
#define TSS_KEYBOARDVERSION  36      // &keyboardversion system store   // I4140
#define TSS_KMW_EMBEDCSS     37

#define TSS_TARGETS          38

#define TSS_CASEDKEYS        39

#define TSS__KEYMAN_140_MAX  39

#define TSS_BEGIN_NEWCONTEXT    40
#define TSS_BEGIN_POSTKEYSTROKE 41

#define TSS_NEWLAYER            42
#define TSS_OLDLAYER            43

#define TSS__MAX                43

/* wm_keyman_control_internal message control codes */

#define KMCI_SELECTKEYBOARD     3   // I3933
#define KMCI_SELECTKEYBOARD_TSF 4   // I3933
#define KMCI_GETACTIVEKEYBOARD  5   // I3933
#define KMCI_SETFOREGROUND      6   // I3933
#define KMCI_SELECTKEYBOARD_BACKGROUND      7   // I4271
#define KMCI_SELECTKEYBOARD_BACKGROUND_TSF  8   // I4271

#define FILEID_COMPILED	 0x5354584B

#define SZMAX_LANGUAGENAME 80
#define SZMAX_KEYBOARDNAME 80
#define SZMAX_COPYRIGHT 256
#define SZMAX_MESSAGE 1024

#define UC_SENTINEL				0xFFFF
#define UC_SENTINEL_EXTENDEDEND	0x10 // was ((CODE_LASTCODE)+1)... what was I thinking?

/*
 * VK__MAX defines the highest virtual key code defined in the system = 0xFF.  Custom VK codes start at 256
 */
#define VK__MAX 255

#define CODE_ANY			0x01
#define CODE_INDEX			0x02
#define CODE_CONTEXT		0x03
#define CODE_NUL			0x04
#define CODE_USE			0x05
#define CODE_RETURN			0x06
#define CODE_BEEP			0x07
#define CODE_DEADKEY		0x08
//	0x09 = bkspace.-- we don't need to keep this separate though with UC_SENTINEL
#define CODE_EXTENDED		0x0A
//#define CODE_EXTENDEDEND	0x0B  deprecated
#define CODE_SWITCH			0x0C
#define CODE_KEY			0x0D
#define	CODE_CLEARCONTEXT	0x0E
#define CODE_CALL			0x0F
// UC_SENTINEL_EXTENDEDEND  0x10
#define CODE_CONTEXTEX		0x11

#define CODE_NOTANY   0x12

#define CODE_KEYMAN70_LASTCODE		0x12

#define CODE_SETOPT    0x13
#define CODE_IFOPT     0x14
#define CODE_SAVEOPT   0x15
#define CODE_RESETOPT  0x16

#define CODE_KEYMAN80_LASTCODE  0x16

/* Keyman 9.0 codes */

#define CODE_IFSYSTEMSTORE  0x17
#define CODE_SETSYSTEMSTORE 0x18

#define CODE_LASTCODE		0x18

#define KF_SHIFTFREESCAPS	0x0001
#define KF_CAPSONONLY		0x0002
#define KF_CAPSALWAYSOFF	0x0004
#define KF_LOGICALLAYOUT	0x0008
#define KF_AUTOMATICVERSION 0x0010

// 16.0: Support for LDML Keyboards in KMXPlus file format
#define KF_KMXPLUS  0x0020

#define HK_ALT			0x00010000
#define HK_CTRL			0x00020000
#define HK_SHIFT		0x00040000

#define HK_RALT_INVALID			0x00100000
#define HK_RCTRL_INVALID		0x00200000
#define HK_RSHIFT_INVALID		0x00400000

#define LCTRLFLAG		0x0001		// Left Control flag
#define RCTRLFLAG		0x0002		// Right Control flag
#define LALTFLAG		0x0004		// Left Alt flag
#define RALTFLAG		0x0008		// Right Alt flag
#define K_SHIFTFLAG		0x0010		// Either shift flag
#define K_CTRLFLAG		0x0020		// Either ctrl flag
#define K_ALTFLAG		0x0040		// Either alt flag
//#define K_METAFLAG  0x0080    // Either Meta-key flag (tentative).  Not usable in keyboard rules;
                                // Used internally (currently, only by KMW) to ensure Meta-key
                                // shortcuts safely bypass rules
                                // Meta key = Command key on macOS, Windows key on Windows
#define CAPITALFLAG		0x0100		// Caps lock on
#define NOTCAPITALFLAG	0x0200		// Caps lock NOT on
#define NUMLOCKFLAG		0x0400		// Num lock on
#define NOTNUMLOCKFLAG	0x0800		// Num lock NOT on
#define SCROLLFLAG		0x1000		// Scroll lock on
#define NOTSCROLLFLAG	0x2000		// Scroll lock NOT on
#define ISVIRTUALKEY	0x4000		// It is a Virtual Key Sequence
#define VIRTUALCHARKEY	0x8000		// Keyman 6.0: Virtual Key Cap Sequence NOT YET

#define K_MODIFIERFLAG  0x007F
#define K_NOTMODIFIERFLAG 0xFF00   // I4548

// Note: OTHER_MODIFIER = 0x10000, used by KMX+ for the
// other modifier flag in layers, > 16 bit so not available here.
// See keys_mod_other in keyman_core_ldml.ts

/*
  These sanity checks help ensure we don't
  break on-disk struct sizes when we cross
  compilers, bitness and platforms. They must
  correspond to the equivalent constants in
  kmxfile.pas. For historical reasons, these
  structures have the prefix COMP_ while
  the pas versions are TKeyboardFile_, names
  which correspond more closely to what the
  structures are for.
*/

#define KEYBOARDFILEHEADER_SIZE 64
#define KEYBOARDFILESTORE_SIZE  12
#define KEYBOARDFILEGROUP_SIZE  24
#define KEYBOARDFILEKEY_SIZE    20

struct COMP_STORE {
	DWORD dwSystemID;
	DWORD dpName;
	DWORD dpString;
	};

static_assert(sizeof(COMP_STORE) == KEYBOARDFILESTORE_SIZE, "COMP_STORE must be KEYBOARDFILESTORE_SIZE bytes");

struct COMP_KEY {
	WORD Key;
	WORD _reserved;
	DWORD Line;
	DWORD ShiftFlags;
	DWORD dpOutput;
	DWORD dpContext;
	};

static_assert(sizeof(COMP_KEY) == KEYBOARDFILEKEY_SIZE, "COMP_KEY must be KEYBOARDFILEKEY_SIZE bytes");

struct COMP_GROUP {
	DWORD dpName;
	DWORD dpKeyArray;		// [LPKEY] address of first item in key array
	DWORD dpMatch;
	DWORD dpNoMatch;
	DWORD cxKeyArray;		// in array entries
	BOOL  fUsingKeys;		// group(xx) [using keys] <-- specified or not
	};

static_assert(sizeof(COMP_GROUP) == KEYBOARDFILEGROUP_SIZE, "COMP_GROUP must be KEYBOARDFILEGROUP_SIZE bytes");

struct COMP_KEYBOARD {
	DWORD dwIdentifier;		// 0000 Keyman compiled keyboard id

	DWORD dwFileVersion;	// 0004 Version of the file - Keyman 4.0 is 0x0400

	DWORD dwCheckSum;		  // 0008 As stored in keyboard. DEPRECATED as of 16.0
	DWORD KeyboardID;    	// 000C as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
	DWORD IsRegistered;		// 0010
	DWORD version;			  // 0014 keyboard version

	DWORD cxStoreArray;		// 0018 in array entries
	DWORD cxGroupArray;		// 001C in array entries

	DWORD dpStoreArray;		// 0020 [LPSTORE] address of first item in store array
	DWORD dpGroupArray;		// 0024 [LPGROUP] address of first item in group array

	DWORD StartGroup[2];	// 0028 index of starting groups [2 of them]
	//DWORD StartGroupIndex;	// StartGroup current index

	DWORD dwFlags;			  // 0030 Flags for the keyboard file

	DWORD dwHotKey;			  // 0034 standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

	//DWORD dpName;			// offset of name
	//DWORD dpLanguageName;	// offset of language name;
	//DWORD dpCopyright;		// offset of copyright
	//DWORD dpMessage;		// offset of message in Keyboard About box

	DWORD dpBitmapOffset;	// 0038 offset of the bitmaps in the file
	DWORD dwBitmapSize;		// 003C size in bytes of the bitmaps
	};

static_assert(sizeof(COMP_KEYBOARD) == KEYBOARDFILEHEADER_SIZE, "COMP_KEYBOARD must be KEYBOARDFILEHEADER_SIZE bytes");

typedef COMP_KEYBOARD *PCOMP_KEYBOARD;
typedef COMP_STORE *PCOMP_STORE;
typedef COMP_KEY *PCOMP_KEY;
typedef COMP_GROUP *PCOMP_GROUP;


typedef struct _COMPILER_OPTIONS {
  DWORD dwSize;
  BOOL ShouldAddCompilerVersion;
} COMPILER_OPTIONS;

typedef COMPILER_OPTIONS *PCOMPILER_OPTIONS;

typedef int (CALLBACK *CompilerMessageProc)(int line, DWORD dwMsgCode, LPSTR szText);

#endif		// _COMPILER_H

